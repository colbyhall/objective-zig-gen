const std = @import("std");
const meta = std.meta;
const mem = std.mem;
const Allocator = mem.Allocator;
const fs = std.fs;

const Preprocessor = @This();

allocator: Allocator,
include_dirs: []const []const u8,
frameworks: []const []const u8,
sdk_path: []const u8,
defines: std.StringHashMap(Define),
files: std.StringHashMap(File),

const Error = error{
    StreamTooLong,
} || Allocator.Error || fs.File.OpenError || fs.Dir.OpenError || fs.Dir.RealPathError || fs.File.ReadError;

pub const Define = struct {
    pub const Kind = union(enum) {
        constant: []const u8,
    };
    name: []const u8,
    kind: Kind,
};

pub const Options = struct {
    allocator: Allocator,

    path: []const u8,

    include_dirs: []const []const u8 = &.{},
    frameworks: []const []const u8 = &.{},
    sdk_path: []const u8 = &.{},
    defines: []const Define = &.{},
};
pub fn run(options: Options) Error!void {
    var defines = std.StringHashMap(Define).init(options.allocator);
    for (options.defines) |define| {
        std.debug.assert(meta.activeTag(define.kind) == .constant);
        try defines.put(define.name, define);
    }

    var self = Preprocessor{
        .allocator = options.allocator,
        .include_dirs = options.include_dirs,
        .defines = defines,
        .sdk_path = options.sdk_path,
        .frameworks = options.frameworks,
        .files = std.StringHashMap(File).init(options.allocator),
    };

    if (try self.loadFile(options.path, null)) |root| {
        defer self.allocator.free(root.path);
        defer self.allocator.free(root.contents);

        try self.processFile(root);
    }
}

fn processFile(self: *Preprocessor, file: File) Error!void {
    try self.files.put(file.path, file);

    var tokenizer = Tokenizer.init(file);
    while (tokenizer.next()) |token| {
        std.debug.print("{} from \n{s}\n", .{ token.tag, file.contents[token.loc.start..token.loc.end] });
        switch (token.tag) {
            .keyword_include, .keyword_import => {
                try self.includeFile(&tokenizer);
            },
            .invalid => {
                std.debug.print("Invalid\n", .{});
                break;
            },
            else => {},
        }
    }
}

fn includeFile(self: *Preprocessor, tokenizer: *Tokenizer) Error!void {
    if (tokenizer.next()) |token| {
        std.debug.assert(token.tag == .include_path);

        const slice = tokenizer.contents[token.loc.start .. token.loc.end - 1];
        const first = slice[0];
        var working_path: ?[]const u8 = null;
        if (first == '<') {
            working_path = fs.path.dirname(tokenizer.path);
        }
        const path = slice[1..];
        if (try self.loadFile(path, working_path)) |child| {
            try self.processFile(child);
        }
    } else {
        unreachable;
    }
}

pub const File = struct {
    path: []const u8,
    contents: [:0]const u8,
};
pub fn loadFile(self: *Preprocessor, path: []const u8, working_path: ?[]const u8) Error!?File {
    var absolute_path: []const u8 = undefined;
    var file: ?fs.File = null;
    if (fs.path.isAbsolute(path)) {
        absolute_path = try self.allocator.dupe(u8, path);
        file = try fs.openFileAbsolute(absolute_path, .{});
    } else {
        // If we have a working path then use it. This is typically usd by quote includes but is also used when openning the first file.
        if (working_path) |actual| blk: {
            var cwd = try fs.openDirAbsolute(actual, .{});
            defer cwd.close();

            file = cwd.openFile(path, .{}) catch |err| {
                if (err == fs.File.OpenError.FileNotFound) {
                    break :blk;
                }
                return err;
            };
            absolute_path = try cwd.realpathAlloc(self.allocator, path);
        }

        // Test to see if this is path to one of our included frameworks
        if (file == null) {
            if (mem.indexOf(u8, path, "/")) |index| {
                const path_framework = path[0..index];
                std.debug.print("{s}\n", .{path_framework});

                outer: for (self.frameworks) |framework| {
                    if (!mem.eql(u8, path_framework, framework)) continue;

                    absolute_path = try std.fmt.allocPrint(
                        self.allocator,
                        "{s}/System/Library/Frameworks/{s}.framework/Headers/{s}",
                        .{
                            self.sdk_path,
                            framework,
                            path[index + 1 ..],
                        },
                    );
                    errdefer self.allocator.free(absolute_path);
                    file = fs.openFileAbsolute(absolute_path, .{}) catch |err| {
                        if (err == fs.File.OpenError.FileNotFound) {
                            self.allocator.free(absolute_path);
                            continue :outer;
                        }
                        return err;
                    };
                    break;
                }
            }
        }

        // Test the include dirs
        if (file == null) {
            const cwd = fs.cwd();
            outer: for (self.include_dirs) |include_dir| {
                var included = blk: {
                    if (fs.path.isAbsolute(include_dir)) {
                        break :blk fs.openDirAbsolute(include_dir, .{}) catch |err| {
                            if (err == fs.Dir.OpenError.FileNotFound) {
                                continue :outer;
                            }
                            return err;
                        };
                    } else {
                        break :blk cwd.openDir(include_dir, .{}) catch |err| {
                            if (err == fs.Dir.OpenError.FileNotFound) {
                                continue :outer;
                            }
                            return err;
                        };
                    }
                };
                defer included.close();

                file = included.openFile(path, .{}) catch |err| {
                    if (err == fs.File.OpenError.FileNotFound) {
                        continue :outer;
                    }
                    return err;
                };
                absolute_path = try included.realpathAlloc(self.allocator, path);
                break :outer;
            }
        }
    }

    if (file) |actual| {
        defer actual.close();

        if (self.files.contains(absolute_path)) {
            return null;
        }

        const max_read = 4 * 1024 * 1024 * 1024; // The preprocessor, tokenizer, and parser limit the size of the files to 4gb.
        const source = try actual.readToEndAllocOptions(self.allocator, max_read, null, 1, 0);
        return .{
            .path = absolute_path,
            .contents = source,
        };
    }

    return null;
}

pub const Tokenizer = struct {
    pub const Token = struct {
        pub const Tag = enum {
            invalid,

            bang,
            @"and",
            @"or",
            l_paren,
            r_paren,
            stringify,
            concat,

            keyword_define,
            keyword_if,
            keyword_ifdef,
            keyword_ifndef,
            keyword_else,
            keyword_elif,
            keyword_endif,
            keyword_include,
            keyword_import,
            keyword_defined,
            keyword_undef,
            keyword_pragma,

            include_path,
            identifier,

            blob,

            pub const keywords = std.StaticStringMap(Tag).initComptime(.{
                .{ "define", .keyword_define },
                .{ "if", .keyword_if },
                .{ "ifdef", .keyword_ifdef },
                .{ "ifndef", .keyword_ifndef },
                .{ "else", .keyword_else },
                .{ "elif", .keyword_elif },
                .{ "endif", .keyword_endif },
                .{ "include", .keyword_include },
                .{ "import", .keyword_import },
                .{ "defined", .keyword_defined },
                .{ "undef", .keyword_undef },
                .{ "pragma", .keyword_pragma },
            });
        };
        pub const Loc = struct {
            start: u32,
            end: u32,
        };

        tag: Tag,
        loc: Loc,
    };

    offset: u32,
    contents: [:0]const u8,
    path: []const u8,
    state: State,

    pub fn init(file: File) Tokenizer {
        return .{
            .offset = 0,
            .contents = file.contents,
            .path = file.path,
            .state = .start,
        };
    }

    const State = enum {
        start,
        keyword,
        eat_rest_of_line,
        include_start,
        include,
        ifdef_start,
        ifdef,
    };

    pub fn next(self: *Tokenizer) ?Token {
        var result = Token{
            .tag = .invalid,
            .loc = .{
                .start = self.offset,
                .end = undefined,
            },
        };
        while (true) : (self.offset += 1) {
            const c = self.contents[self.offset];

            switch (self.state) {
                .start => switch (c) {
                    0 => {
                        return null;
                    },
                    ' ', '\t', '\n' => {},
                    '#' => {
                        self.state = .keyword;
                        if (self.offset > 0) {
                            result.tag = .blob;
                            result.loc.end = self.offset - 1;
                            return result;
                        }
                    },
                    else => {
                        self.state = .eat_rest_of_line;
                    },
                },
                .keyword => switch (c) {
                    ' ', '\n' => {
                        const identifier = self.contents[result.loc.start + 1 .. self.offset];
                        if (Token.Tag.keywords.get(identifier)) |keyword| {
                            result.tag = keyword;
                            switch (keyword) {
                                .keyword_include, .keyword_import => {
                                    self.state = .include_start;
                                },
                                .keyword_ifdef => {
                                    self.state = .ifdef_start;
                                },
                                else => {
                                    self.state = .eat_rest_of_line;
                                },
                            }
                        }
                        self.offset += 1;
                        break;
                    },
                    else => {},
                },
                .eat_rest_of_line => switch (c) {
                    '\n' => {
                        self.state = .start;
                    },
                    0 => {
                        return null;
                    },
                    else => {},
                },
                .include_start => switch (c) {
                    '<', '"' => {
                        self.state = .include;
                        result.loc.start = self.offset;
                    },
                    ' ' => {},
                    else => {
                        result.tag = .invalid;
                        self.offset += 1;
                        break;
                    },
                },
                .include => switch (c) {
                    '>', '"' => {
                        result.tag = .include_path;
                        self.offset += 1;
                        self.state = .eat_rest_of_line;
                        break;
                    },
                    else => {},
                },
                .ifdef_start => switch (c) {
                    '\n', 0, '0'...'9' => {
                        result.tag = .invalid;
                        self.offset += 1;
                        break;
                    },
                    ' ' => {},
                    else => {
                        self.state = .ifdef;
                        result.loc.start = self.offset;
                    },
                },
                .ifdef => switch (c) {
                    ' ', '\n', 0 => {
                        self.state = .eat_rest_of_line;
                        result.tag = .identifier;
                        break;
                    },
                    else => {},
                },
            }
        }
        result.loc.end = self.offset;
        std.debug.print("foo {}", .{result.loc});
        return result;
    }
};

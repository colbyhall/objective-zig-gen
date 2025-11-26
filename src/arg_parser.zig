const std = @import("std");
const fmt = std.fmt;
const mem = std.mem;
const fs = std.fs;
const process = std.process;

const Allocator = mem.Allocator;
const ArrayList = std.ArrayList;
const StringHashMap = std.StringHashMapUnmanaged;

pub const ArgParser = union(enum) {
    const Self = @This();

    pub const Option = struct {
        const Param = enum { none, string };
        const ParamValue = union(Param) {
            none: void,
            string: []const u8,
        };

        name: []const u8,
        description: []const u8,
        alias: ?[]const u8 = null,
        param: Param = .none,
    };
    pub const Parsed = struct {
        path: []const u8,
        options: StringHashMap(Option.ParamValue),
    };

    parsed: Parsed,
    help: []const u8,
    @"error": []const u8,

    pub fn run(gpa: Allocator, comptime options: []const Option) !Self {
        var args = process.args();
        defer args.deinit();
        var cwd = fs.cwd();

        std.debug.assert(args.skip()); // Skip past the executable in the argument

        var file: []const u8 = undefined;
        var option_map: StringHashMap(Option.ParamValue) = .empty;

        var index: i32 = 1;
        outer: while (args.next()) |arg| {
            defer index += 1;

            // The help option or the spec json file must be the first argument
            if (index == 1) {
                if (mem.eql(u8, arg, "-h") or mem.eql(u8, arg, "-help")) {
                    var help_buffer: ArrayList(u8) = .empty;
                    const writer = help_buffer.writer(gpa);

                    try writer.print("objective-zig-gen [-h | -help] <path/to/manifest.json> [<options>]\n\n", .{});
                    try writer.print("Options:\n\n", .{});
                    for (options) |o| {
                        const name = if (o.alias) |alias|
                            try fmt.allocPrint(gpa, "{s} | {s}", .{ o.name, alias })
                        else
                            try fmt.allocPrint(gpa, "{s}", .{o.name});

                        try writer.print("   {s: <26}   {s}\n", .{ name, o.description });
                    }

                    return .{
                        .help = try help_buffer.toOwnedSlice(gpa),
                    };
                }

                if (!mem.endsWith(u8, arg, ".json")) {
                    return .{
                        .@"error" = "File provided is not a json file.\n",
                    };
                }

                if (fs.path.isAbsolute(arg)) {
                    // Catch the invalid file error early so zig cc doesn't have to
                    fs.accessAbsolute(arg, .{}) catch |err| {
                        return .{
                            .@"error" = try fmt.allocPrint(
                                gpa,
                                "Failed to access file at path \'{s}\' due to {s}.\n",
                                .{ arg, @errorName(err) },
                            ),
                        };
                    };

                    const new_path = try gpa.alloc(u8, arg.len);
                    @memcpy(new_path, arg);
                    file = new_path;
                } else {
                    file = cwd.realpathAlloc(gpa, arg) catch |err| {
                        return .{
                            .@"error" = try fmt.allocPrint(
                                gpa,
                                "Failed to access file at path \'{s}\' due to {s}.\n",
                                .{ arg, @errorName(err) },
                            ),
                        };
                    };
                }

                continue;
            }

            if (!mem.startsWith(u8, arg, "-")) {
                return .{
                    .@"error" = try fmt.allocPrint(
                        gpa,
                        "Argument does not start with '-'. See -h for usage.\n",
                        .{},
                    ),
                };
            }

            // Parse the arguments and match them to the comptime provided data.
            const name = arg[1..];
            for (options) |option| {
                // Check to see if 'name' is this option
                const name_is_name = mem.eql(u8, name, option.name);
                if (option.alias) |alias| {
                    if (!name_is_name and !mem.eql(u8, name, alias)) {
                        continue;
                    }
                } else {
                    if (!name_is_name) {
                        continue;
                    }
                }

                // If we have no param then insert the arg into the option_map
                if (option.param == .none) {
                    try option_map.put(gpa, option.name, .{ .none = {} });
                    continue :outer;
                }

                // If we allow a param peek ahead to see if there is a possible param being passed to this argument.
                var peek_args = args;
                var param = peek_args.next();
                if (param) |p| {
                    if (mem.startsWith(u8, p, "-")) {
                        param = null;
                    }
                }

                // Consume the peek arg that is the param
                if (param != null) {
                    _ = args.next();
                }

                switch (option.param) {
                    .string => {
                        const result = try gpa.alloc(u8, param.?.len);
                        @memcpy(result, param.?);
                        try option_map.put(gpa, option.name, .{ .string = result });
                    },
                    else => unreachable,
                }
            }

            return .{
                .@"error" = try fmt.allocPrint(
                    gpa,
                    "Unknown command '{s}'. See usage using -h.\n",
                    .{name},
                ),
            };
        }

        if (index == 1) {
            return .{ .@"error" = "No manifest path was provided\n" };
        }

        return .{
            .parsed = .{
                .path = file,
                .options = option_map,
            },
        };
    }
};

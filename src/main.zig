const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const fmt = std.fmt;
const ascii = std.ascii;
const meta = std.meta;

const ArgParser = @import("arg_parser.zig").ArgParser;

pub const Framework = struct {
    name: []const u8,
    output_file: []const u8,

    // Optional fields
    dependencies: []const []const u8 = &.{},
    remove_prefix: []const u8 = &.{},
    header_override: ?[]const u8 = null,
};
const Manifest = []const Framework;

fn parseJsonWithCustomErrorHandling(
    comptime T: type,
    allocator: Allocator,
    complete_input: []const u8,
    file: []const u8,
) !?std.json.Parsed(T) {
    var scanner = std.json.Scanner.initCompleteInput(allocator, complete_input);
    var diagnostics = std.json.Diagnostics{};
    scanner.enableDiagnostics(&diagnostics);

    const result = std.json.parseFromTokenSource(T, allocator, &scanner, .{}) catch |err| {
        const lines_above_error_to_show = 10;
        const lines_below_error_to_show = 10;

        const offset = diagnostics.getByteOffset();

        // Reverse iterate through the complete_input at offset looking for newlines
        var previous_line_start: usize = offset;
        var previous_newline_count: u32 = 0;
        while (previous_line_start > 0) {
            previous_line_start -= 1;

            if (complete_input[previous_line_start] == '\n') {
                previous_newline_count += 1;
                if (previous_newline_count == lines_above_error_to_show) {
                    // If we've hit this branch we've stopped at an index that is
                    // a newline. Move forward to the beginning of the line.
                    previous_line_start += 1;
                    break;
                }
            }
        }

        // This will eventually be sliced down to just the parts we want to output to the user.
        // Start by removing everything before previous_line_start.
        var blob = complete_input[previous_line_start..];

        // We're going to write out multiple lines of input over multiple lines of code so go ahead
        // and lock stderr and print out to it manually as std.debug.print locks internally.
        std.Progress.lockStdErr();
        defer std.Progress.unlockStdErr();
        const stderr = std.io.getStdErr();

        try stderr.writer().print(
            "Failed to parse json file '{s}' into '{s}'.\nError at line {} column {}.\n\n",
            .{
                file,
                @typeName(T),
                diagnostics.getLine(),
                diagnostics.getColumn(),
            },
        );

        // Search for the next newline. If one can't be found then we're at the EOF
        var next_newline = mem.indexOf(u8, blob, "\n");
        var line = if (next_newline) |newline| blob[0..newline] else blob;
        if (next_newline) |newline| {
            blob = blob[newline + 1 ..];
        }
        var line_number = diagnostics.getLine() - previous_newline_count;

        while (line_number < diagnostics.getLine() + lines_below_error_to_show) {
            try stderr.writer().print("   {: >7} {s}\n", .{ line_number, line });

            if (line_number == diagnostics.getLine()) {
                const padding = 10;
                for (0..padding + diagnostics.getColumn()) |_| {
                    try stderr.writer().print("-", .{});
                }
                try stderr.writer().print("^ {s}\n", .{@errorName(err)});
            }

            // If the next_newline is null then its the EOF and we need to quit
            if (next_newline == null) {
                break;
            }
            next_newline = mem.indexOf(u8, blob, "\n");

            line_number += 1;
            if (next_newline) |newline| {
                line = blob[0..newline];
                blob = blob[newline + 1 ..];
            } else {
                line = blob;
                // Check to see if the EOF line is empty and if so don't print it.
                if (line.len == 0) {
                    break;
                }
            }
        }

        try stderr.writer().print("\n", .{});

        return null;
    };

    return result;
}

pub fn acquireSDKPath(allocator: Allocator) ![]const u8 {
    const args = [_][]const u8{
        "xcrun",
        "--show-sdk-path",
    };
    const result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &args,
    });
    // Replace the newline with a sentinel
    result.stdout[result.stdout.len - 1] = 0;
    return result.stdout[0 .. result.stdout.len - 1];
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const args = try ArgParser.run(allocator, &.{});
    switch (args) {
        .parsed => |result| {
            // Read in the manifest file and deserialize it.
            var possible_manifest: ?std.json.Parsed(Manifest) = null;
            {
                const manifest_file = try std.fs.openFileAbsolute(result.path, .{});
                defer manifest_file.close();

                const contents = try manifest_file.readToEndAlloc(allocator, 2 * 1024 * 1024);
                possible_manifest = try parseJsonWithCustomErrorHandling(
                    Manifest,
                    allocator,
                    contents,
                    result.path,
                );
            }

            if (possible_manifest == null) {
                return;
            }

            const manifest = possible_manifest.?;
            defer manifest.deinit();

            if (manifest.value.len == 0) {
                std.log.err("Found no frameworks in manifest '{s}'.", .{result.path});
                return;
            }

            // First thing to do is to validate that any framework dependency is listed in the manifest
            var frameworks = std.StringHashMap(*const Framework).init(allocator);
            defer frameworks.deinit();
            for (manifest.value) |*framework| {
                try frameworks.put(framework.name, framework);
            }
            for (manifest.value) |framework| {
                for (framework.dependencies) |dependency| {
                    if (!frameworks.contains(dependency)) {
                        std.log.err("Framework '{s}' has '{s}' listed as a dependency but it isn't in the manifest.", .{ framework.name, dependency });
                        return;
                    }
                }
            }

            const sdk_path = try acquireSDKPath(allocator);
            const frameworks_path = try std.fs.path.join(
                allocator,
                &.{
                    sdk_path,
                    "System/Library/Frameworks/",
                },
            );
            defer allocator.free(frameworks_path);

            std.log.info("Parsed manifest and found {} frameworks.", .{manifest.value.len});
            for (manifest.value, 0..) |framework, index| {
                std.log.info("  {}. {s}", .{ index + 1, framework.name });
            }

            var thread_pool: std.Thread.Pool = undefined;
            try thread_pool.init(.{ .allocator = allocator });

            const results = try allocator.alloc(Registry, manifest.value.len);
            var parse_framework_work = std.Thread.WaitGroup{};
            for (manifest.value, 0..) |*framework, index| {
                thread_pool.spawnWg(
                    &parse_framework_work,
                    parseFramework,
                    .{
                        .{
                            .gpa = allocator,
                            .arena = allocator,
                            .sdk_path = sdk_path,
                            .framework = framework,
                            .result = &results[index],
                        },
                    },
                );
            }
            thread_pool.waitAndWork(&parse_framework_work);

            const cwd = std.fs.cwd();

            const output_path = "output";
            cwd.makeDir(output_path) catch |err| {
                if (err != std.fs.Dir.MakeError.PathAlreadyExists) {
                    return err;
                }
            };

            var output = try cwd.openDir(output_path, .{});
            defer output.close();

            // Copy about the objc runtime to the output directory.
            {
                var objc_file = try output.createFile("objc.zig", .{});
                defer objc_file.close();
                _ = try objc_file.write(@embedFile("objc.zig"));
            }

            var render_framework_work = std.Thread.WaitGroup{};
            for (results) |*r| {
                thread_pool.spawnWg(&render_framework_work, renderFramework, .{
                    .{
                        .gpa = allocator,
                        .output = output,
                        .frameworks = &frameworks,
                        .registry = r,
                    },
                });
            }
            thread_pool.waitAndWork(&render_framework_work);

            {
                var root_file = try output.createFile("root.zig", .{});
                defer root_file.close();

                const writer = root_file.writer();
                _ = try writer.write("// THIS FILE IS AUTOGENERATED. MODIFICATIONS WILL NOT BE MAINTAINED.\n\n");
                _ = try writer.write("pub usingnamespace @import(\"objc.zig\"); // Export the objective c runtime to root. \n");

                for (manifest.value) |f| {
                    try writer.print("pub const {s} = @import(\"{s}.zig\");\n", .{
                        f.output_file,
                        f.output_file,
                    });
                }
            }

            _ = try std.process.Child.run(.{
                .allocator = allocator,
                .argv = &.{
                    "zig",
                    "fmt",
                    "output_path",
                },
            });
        },
        .help, .@"error" => |msg| std.debug.print("{s}", .{msg}),
        .exit => {},
    }
}

const c = @cImport(
    @cInclude("clang-c/Index.h"),
);

const Type = union(enum) {
    const Named = struct {
        const Typedef = struct {
            child: ?*Type,

            fn asNamed(self: *@This()) *Type.Named {
                const tag_offset = @offsetOf(Type.Named, "tag");
                const tag: *Tag = @fieldParentPtr("typedef", self);
                return @ptrFromInt(@intFromPtr(tag) - tag_offset);
            }
        };
        const Field = struct {
            type: *Type,

            fn asNamed(self: *@This()) *Type.Named {
                const tag_offset = @offsetOf(Type.Named, "tag");
                const tag: *Tag = @fieldParentPtr("field", self);
                return @ptrFromInt(@intFromPtr(tag) - tag_offset);
            }
        };
        const Union = struct {
            fields: std.ArrayList(*Type.Named.Field),

            fn asNamed(self: *@This()) *Type.Named {
                const tag_offset = @offsetOf(Type.Named, "tag");
                const tag: *Tag = @fieldParentPtr("union", self);
                return @ptrFromInt(@intFromPtr(tag) - tag_offset);
            }
        };
        const Struct = struct {
            @"packed": u1 = 0,
            fields: std.ArrayList(*Type.Named.Field),

            fn asNamed(self: *@This()) *Type.Named {
                const tag_offset = @offsetOf(Type.Named, "tag");
                const tag: *Tag = @fieldParentPtr("struct", self);
                return @ptrFromInt(@intFromPtr(tag) - tag_offset);
            }
        };
        const Enum = struct {
            const Value = struct {
                name: []const u8,
                value: i64,
            };
            backing: *Type,
            values: std.ArrayList(Value),

            fn asNamed(self: *@This()) *Type.Named {
                const tag_offset = @offsetOf(Type.Named, "tag");
                const tag: *Tag = @fieldParentPtr("enum", self);
                return @ptrFromInt(@intFromPtr(tag) - tag_offset);
            }
        };
        const Param = struct {
            type: *Type,

            fn asNamed(self: *@This()) *Type.Named {
                const tag_offset = @offsetOf(Type.Named, "tag");
                const tag: *Tag = @fieldParentPtr("param", self);
                return @ptrFromInt(@intFromPtr(tag) - tag_offset);
            }
        };
        const Function = struct {
            result: ?*Type,
            params: std.ArrayList(*Param),

            fn asNamed(self: *@This()) *Type.Named {
                const tag_offset = @offsetOf(Type.Named, "tag");
                const tag: *Tag = @fieldParentPtr("function", self);
                return @ptrFromInt(@intFromPtr(tag) - tag_offset);
            }
        };
        const Method = struct {
            result: ?*Type,
            params: std.ArrayList(*Param),

            fn asNamed(self: *@This()) *Type.Named {
                const tag_offset = @offsetOf(Type.Named, "tag");
                const tag: *Tag = @fieldParentPtr("method", self);
                return @ptrFromInt(@intFromPtr(tag) - tag_offset);
            }
        };
        const Protocol = struct {
            inherits: std.ArrayList(*Named),
            methods: std.ArrayList(*Method),

            fn asNamed(self: *@This()) *Type.Named {
                const tag_offset = @offsetOf(Type.Named, "tag");
                const tag: *Tag = @fieldParentPtr("protocol", self);
                return @ptrFromInt(@intFromPtr(tag) - tag_offset);
            }
        };
        const Interface = struct {
            type_parameters: std.ArrayList([]const u8),

            super: ?*Named,
            protocols: std.ArrayList(*Protocol),
            methods: std.ArrayList(*Method),

            fn asNamed(self: *@This()) *Type.Named {
                const tag_offset = @offsetOf(Type.Named, "tag");
                const tag: *Tag = @fieldParentPtr("interface", self);
                return @ptrFromInt(@intFromPtr(tag) - tag_offset);
            }
        };
        const Class = struct {
            protocol: ?*Protocol,

            fn asNamed(self: *@This()) *Type.Named {
                const tag_offset = @offsetOf(Type.Named, "tag");
                const tag: *Tag = @fieldParentPtr("class", self);
                return @ptrFromInt(@intFromPtr(tag) - tag_offset);
            }
        };
        const TypeReference = struct {
            type_parameters: std.ArrayList([]const u8),

            fn asNamed(self: *@This()) *Type.Named {
                const tag_offset = @offsetOf(Type.Named, "tag");
                const tag: *Tag = @fieldParentPtr("type_reference", self);
                return @ptrFromInt(@intFromPtr(tag) - tag_offset);
            }
        };
        const Tag = union(enum) {
            typedef: Typedef,
            field: Field,
            @"union": Union,
            @"struct": Struct,
            @"enum": Enum,
            param: Param,
            function: Function,
            method: Method,
            protocol: Protocol,
            interface: Interface,
            class: Class,
            type_reference: TypeReference,
        };

        const Origin = union(enum) {
            framework: []const u8,
            runtime,
        };

        name: []const u8,
        cursor: c.CXCursor,
        origin: Origin,
        tag: Tag,

        fn asType(self: *@This()) *Type {
            return @fieldParentPtr("named", self);
        }
    };

    const Int = struct {
        signed: u1,
        size: u7,
    };
    const Float = struct {
        size: u4,
    };
    const Array = struct {
        element: *Type,
        length: u64,
    };
    const Pointer = struct {
        @"const": u1,
        nullable: u1,
        underlying: *Type,
    };
    const FunctionProto = struct {
        result: *Type,
        params: []const *Type,
    };

    va_list,
    instancetype,
    void,
    objc_class,
    objc_sel,
    objc_id,
    base_protocol,

    named: Named,

    int: Int,
    float: Float,
    pointer: Pointer,
    block_pointer: Pointer,
    array: Array,
    function_proto: FunctionProto,
};

const Registry = struct {
    owner: *const Framework,

    order: std.ArrayList(Order),
    typedefs: std.StringArrayHashMap(*Type.Named),
    unions: std.StringArrayHashMap(*Type.Named),
    structs: std.StringArrayHashMap(*Type.Named),
    functions: std.StringArrayHashMap(*Type.Named),
    enums: std.StringArrayHashMap(*Type.Named),
    protocols: std.StringArrayHashMap(*Type.Named),
    classes: std.StringArrayHashMap(*Type.Named),
    interfaces: std.StringArrayHashMap(*Type.Named),

    const Order = struct {
        tag: meta.Tag(Type.Named.Tag),
        name: []const u8,
    };

    const Error = error{
        MultipleTypes,
    } || Allocator.Error;
    fn init(owner: *const Framework, allocator: Allocator) @This() {
        return .{
            .owner = owner,
            .typedefs = std.StringArrayHashMap(*Type.Named).init(allocator),
            .unions = std.StringArrayHashMap(*Type.Named).init(allocator),
            .structs = std.StringArrayHashMap(*Type.Named).init(allocator),
            .functions = std.StringArrayHashMap(*Type.Named).init(allocator),
            .enums = std.StringArrayHashMap(*Type.Named).init(allocator),
            .protocols = std.StringArrayHashMap(*Type.Named).init(allocator),
            .classes = std.StringArrayHashMap(*Type.Named).init(allocator),
            .interfaces = std.StringArrayHashMap(*Type.Named).init(allocator),

            .order = std.ArrayList(Order).init(allocator),
        };
    }

    fn getMap(self: *@This(), tag: meta.Tag(Type.Named.Tag)) *std.StringArrayHashMap(*Type.Named) {
        const map = switch (tag) {
            .typedef => &self.typedefs,
            .@"union" => &self.unions,
            .@"struct" => &self.structs,
            .function => &self.functions,
            .@"enum" => &self.enums,
            .protocol => &self.protocols,
            .class => &self.classes,
            .interface => &self.interfaces,
            else => @panic("Type is not supported by Registry."),
        };

        return map;
    }

    fn insert(self: *@This(), named: *Type.Named) void {
        const tag = meta.activeTag(named.tag);
        const map = self.getMap(tag);

        if (!map.contains(named.name)) {
            self.order.append(.{ .tag = tag, .name = named.name }) catch {
                @panic("OOM");
            };
        }

        map.put(named.name, named) catch {
            @panic("OOM");
        };
    }

    fn lookup(self: *@This(), tag: meta.Tag(Type.Named.Tag), name: []const u8) ?*Type.Named {
        return self.getMap(tag).get(name);
    }

    fn lookupElaborated(self: *@This(), name: []const u8) ?*Type.Named {
        var lookup_name = name;

        const Preference = enum { none, @"enum", @"struct", @"union" };
        var preference = Preference.none;

        // Remove struct or union prefix
        if (mem.indexOf(u8, lookup_name, "enum ")) |index| {
            lookup_name = lookup_name[index + 5 ..];
            preference = .@"enum";
        } else if (mem.indexOf(u8, lookup_name, "struct ")) |index| {
            lookup_name = lookup_name[index + 7 ..];
            preference = .@"struct";
        } else if (mem.indexOf(u8, lookup_name, "union ")) |index| {
            lookup_name = lookup_name[index + 6 ..];
            preference = .@"union";
        }

        switch (preference) {
            .none => {
                if (self.typedefs.get(lookup_name)) |u| {
                    return u;
                }
                if (self.unions.get(lookup_name)) |u| {
                    return u;
                }
                if (self.structs.get(lookup_name)) |u| {
                    return u;
                }
                if (self.enums.get(lookup_name)) |u| {
                    return u;
                }
                if (self.protocols.get(lookup_name)) |u| {
                    return u;
                }
                if (self.classes.get(lookup_name)) |u| {
                    return u;
                }
                if (self.interfaces.get(lookup_name)) |u| {
                    return u;
                }
            },
            .@"enum" => {
                if (self.enums.get(lookup_name)) |u| {
                    return u;
                }
            },
            .@"struct" => {
                if (self.structs.get(lookup_name)) |u| {
                    return u;
                }
            },
            .@"union" => {
                if (self.unions.get(lookup_name)) |u| {
                    return u;
                }
            },
        }

        return null;
    }
};

const Builder = struct {
    gpa: Allocator,
    arena: Allocator,

    stack: std.ArrayList(*Type.Named),
    registry: Registry,

    fn init(owner: *const Framework, gpa: Allocator, arena: Allocator) @This() {
        return .{
            .gpa = gpa,
            .arena = arena,

            .stack = std.ArrayList(*Type.Named).init(gpa),
            .registry = Registry.init(owner, gpa),
        };
    }

    fn allocType(self: *@This()) *Type {
        return self.arena.create(Type) catch {
            @panic("OOM");
        };
    }

    fn allocName(self: *@This(), name: []const u8) []const u8 {
        return self.arena.dupe(u8, name) catch {
            @panic("OOM");
        };
    }

    fn push(self: *@This(), @"type": *Type.Named) void {
        self.stack.append(@"type") catch {
            @panic("OOM");
        };
    }

    fn pop(self: *@This()) void {
        _ = self.stack.pop();
    }

    fn reset(self: *@This()) void {
        self.stack.clearRetainingCapacity();
    }

    fn parent(self: @This()) ?*Type.Named {
        return self.stack.getLastOrNull();
    }

    fn analyzeType(self: *@This(), origin: Type.Named.Origin, @"type": c.CXType) *Type {
        const result = self.allocType();
        switch (@"type".kind) {
            c.CXType_Void => {
                result.* = .{
                    .void = {},
                };
            },
            c.CXType_Float16 => {
                result.* = .{
                    .float = .{
                        .size = 2,
                    },
                };
            },
            c.CXType_Float => {
                result.* = .{
                    .float = .{
                        .size = 4,
                    },
                };
            },
            c.CXType_Double, c.CXType_LongDouble => {
                result.* = .{
                    .float = .{
                        .size = 8,
                    },
                };
            },
            c.CXType_SChar, c.CXType_Char_S => {
                result.* = .{
                    .int = .{
                        .signed = 1,
                        .size = 1,
                    },
                };
            },
            c.CXType_UChar => {
                result.* = .{
                    .int = .{
                        .signed = 0,
                        .size = 1,
                    },
                };
            },
            c.CXType_Short => {
                result.* = .{
                    .int = .{
                        .signed = 1,
                        .size = 2,
                    },
                };
            },
            c.CXType_UShort => {
                result.* = .{
                    .int = .{
                        .signed = 0,
                        .size = 2,
                    },
                };
            },
            c.CXType_Int => {
                result.* = .{
                    .int = .{
                        .signed = 1,
                        .size = 4,
                    },
                };
            },
            c.CXType_UInt => {
                result.* = .{
                    .int = .{
                        .signed = 0,
                        .size = 4,
                    },
                };
            },
            c.CXType_LongLong, c.CXType_Long => {
                result.* = .{
                    .int = .{
                        .signed = 1,
                        .size = 8,
                    },
                };
            },
            c.CXType_ULongLong, c.CXType_ULong => {
                result.* = .{
                    .int = .{
                        .signed = 0,
                        .size = 8,
                    },
                };
            },
            c.CXType_ConstantArray => {
                const length = c.clang_getArraySize(@"type");
                const element = c.clang_getArrayElementType(@"type");
                result.* = .{
                    .array = .{
                        .element = self.analyzeType(origin, element),
                        .length = @as(u64, @intCast(length)),
                    },
                };
            },
            c.CXType_Elaborated => {
                const underlying = c.clang_Type_getNamedType(@"type");
                const underlying_name = c.clang_getTypeSpelling(underlying);
                defer c.clang_disposeString(underlying_name);

                var name = self.allocName(mem.sliceTo(c.clang_getCString(underlying_name), 0));

                if (mem.containsAtLeast(u8, name, 1, "unnamed at")) {
                    const offset = mem.indexOf(u8, name, ".h:").?;
                    const end = name[offset + 3 ..];
                    const next = mem.indexOf(u8, end, ":").?;
                    const line = end[0..next];
                    const paren = mem.indexOf(u8, end, ")").?;
                    const column = end[next + 1 .. paren];
                    name = fmt.allocPrintZ(self.arena, "anon{s}{s}", .{ line, column }) catch {
                        @panic("OOM");
                    };
                }

                if (mem.eql(u8, name, "__builtin_va_list")) {
                    result.* = .{
                        .va_list = {},
                    };
                } else if (self.registry.lookupElaborated(name)) |u| {
                    return u.asType();
                } else {
                    if (mem.indexOf(u8, name, "enum ")) |index| {
                        name = name[index + 5 ..];
                    } else if (mem.indexOf(u8, name, "struct ")) |index| {
                        name = name[index + 7 ..];
                    } else if (mem.indexOf(u8, name, "union ")) |index| {
                        name = name[index + 6 ..];
                    }

                    result.* = .{
                        .named = .{
                            .name = self.allocName(name),
                            .cursor = c.clang_getNullCursor(),
                            .origin = origin,
                            .tag = .{
                                .type_reference = .{
                                    .type_parameters = std.ArrayList([]const u8).init(self.gpa),
                                },
                            },
                        },
                    };
                }
            },
            c.CXType_Pointer, c.CXType_ObjCObjectPointer => {
                const @"const" = c.clang_isConstQualifiedType(@"type");
                const nullable = c.clang_Type_getNullability(@"type") != c.CXTypeNullability_NonNull;
                const underlying = self.analyzeType(origin, c.clang_getPointeeType(@"type"));
                result.* = .{
                    .pointer = .{
                        .@"const" = @intCast(@"const"),
                        .nullable = if (nullable) 1 else 0,
                        .underlying = underlying,
                    },
                };
            },
            c.CXType_BlockPointer => {
                const underlying = self.analyzeType(origin, c.clang_getPointeeType(@"type"));
                result.* = .{
                    .block_pointer = .{
                        .@"const" = 0,
                        .nullable = 0,
                        .underlying = underlying,
                    },
                };
            },
            c.CXType_FunctionProto => {
                const result_type = self.analyzeType(origin, c.clang_getResultType(@"type"));
                const num_args: usize = @intCast(c.clang_getNumArgTypes(@"type"));
                const params = self.arena.alloc(*Type, num_args) catch {
                    @panic("OOM");
                };
                for (0..num_args) |index| {
                    params[index] = self.analyzeType(origin, c.clang_getArgType(@"type", @intCast(index)));
                }
                result.* = .{
                    .function_proto = .{
                        .result = result_type,
                        .params = params,
                    },
                };
            },
            c.CXType_Typedef => {
                const name_spelling = c.clang_getTypeSpelling(@"type");
                const name = self.allocName(mem.sliceTo(c.clang_getCString(name_spelling), 0));
                if (self.registry.lookupElaborated(name)) |u| {
                    return u.asType();
                } else {
                    if (mem.eql(u8, name, "instancetype")) {
                        result.* = .{
                            .instancetype = {},
                        };
                    } else {
                        std.log.err("Failed to find {s}", .{name});
                        unreachable;
                    }
                }
            },
            c.CXType_IncompleteArray => {
                const underlying = self.analyzeType(origin, c.clang_getArrayElementType(@"type"));
                result.* = .{
                    .pointer = .{
                        .@"const" = 0,
                        .nullable = 0,
                        .underlying = underlying,
                    },
                };
            },
            c.CXType_ObjCClass => {
                result.* = .{ .objc_class = {} };
            },
            c.CXType_ObjCSel => {
                result.* = .{ .objc_sel = {} };
            },
            c.CXType_ObjCId => {
                result.* = .{ .objc_id = {} };
            },
            c.CXType_ObjCObject, c.CXType_ObjCTypeParam => {
                result.* = .{ .void = {} };
            },
            c.CXType_ObjCInterface => {
                const name_spelling = c.clang_getTypeSpelling(@"type");
                const name = self.allocName(mem.sliceTo(c.clang_getCString(name_spelling), 0));
                if (self.registry.lookupElaborated(name)) |u| {
                    return u.asType();
                } else {
                    if (mem.eql(u8, name, "Protocol")) {
                        result.* = .{
                            .base_protocol = {},
                        };
                    } else {
                        result.* = .{
                            .named = .{
                                .name = name,
                                .origin = origin,
                                .cursor = c.clang_getNullCursor(),
                                .tag = .{
                                    .class = .{
                                        .protocol = null,
                                    },
                                },
                            },
                        };
                        // std.log.err("Failed to find {s}", .{name});
                        // unreachable;
                    }
                }
            },
            else => {
                const type_kind_spelling = c.clang_getTypeKindSpelling(@"type".kind);
                defer c.clang_disposeString(type_kind_spelling);
                const type_spelling = c.clang_getTypeSpelling(@"type");
                defer c.clang_disposeString(type_spelling);

                std.log.err("Unhandled type of {s} named {s}", .{ c.clang_getCString(type_kind_spelling), c.clang_getCString(type_spelling) });
                unreachable;
            },
        }
        return result;
    }
};

fn parseFramework(
    args: struct {
        gpa: Allocator,
        arena: Allocator,
        sdk_path: []const u8,
        framework: *const Framework,
        result: *Registry,
    },
) void {
    const path = blk: {
        if (args.framework.header_override) |header| {
            break :blk fmt.allocPrintZ(
                args.gpa,
                "{s}/System/Library/Frameworks/{s}.framework/Headers/{s}",
                .{ args.sdk_path, args.framework.name, header },
            );
        }
        break :blk fmt.allocPrintZ(args.gpa, "{s}/System/Library/Frameworks/{s}.framework/Headers/{s}.h", .{
            args.sdk_path,
            args.framework.name,
            args.framework.name,
        });
    } catch {
        @panic("OOM");
    };

    const index = c.clang_createIndex(0, 0);
    defer c.clang_disposeIndex(index);
    const translate_args = [_][*c]const u8{
        "-x",
        "objective-c",
        "-isysroot",
        args.sdk_path.ptr,
    };
    var unit: c.CXTranslationUnit = undefined;
    const err = c.clang_parseTranslationUnit2(
        index,
        @as([*c]u8, @ptrCast(path)),
        @as([*c]const [*c]const u8, @ptrCast(translate_args[0..])),
        translate_args.len,
        null,
        0,
        c.CXTranslationUnit_KeepGoing,
        &unit,
    );
    defer c.clang_disposeTranslationUnit(unit);

    if (err != c.CXError_Success) {
        std.log.err("Failed to parse {s} due to error code {}", .{ path, err });
        unreachable;
    }

    const cursor = c.clang_getTranslationUnitCursor(unit);
    var builder = Builder.init(args.framework, args.gpa, args.arena);
    // Add this type because it was missing for some reason.
    {
        const _u128_t = builder.allocType();
        _u128_t.* = .{
            .int = .{
                .signed = 0,
                .size = 16,
            },
        };
        const typedef = builder.allocType();
        typedef.* = .{
            .named = .{
                .name = "__uint128_t",
                .cursor = c.clang_getNullCursor(),
                .origin = .{
                    .runtime = {},
                },
                .tag = .{
                    .typedef = .{
                        .child = _u128_t,
                    },
                },
            },
        };
        builder.registry.insert(&typedef.named);
    }
    _ = c.clang_visitChildren(cursor, visitor, &builder);
    args.result.* = builder.registry;
}

fn visitor(cursor: c.CXCursor, parent_cursor: c.CXCursor, client_data: c.CXClientData) callconv(.C) c.CXChildVisitResult {
    const builder: *Builder = @alignCast(@ptrCast(client_data));

    const location = c.clang_getCursorLocation(cursor);
    var file: c.CXFile = undefined;
    var line: c_uint = undefined;
    var column: c_uint = undefined;
    c.clang_getFileLocation(location, &file, &line, &column, null);

    while (builder.stack.items.len > 0) {
        const parent = builder.parent().?;
        if (c.clang_equalCursors(parent.cursor, parent_cursor) == 0) {
            builder.pop();
        } else {
            break;
        }
    }

    if (c.clang_getCursorKind(parent_cursor) == c.CXCursor_TranslationUnit) {
        std.debug.assert(builder.stack.items.len == 0);
    }

    const origin: Type.Named.Origin = blk: {
        if (file != null) {
            const file_name = c.clang_getFileName(file);
            defer c.clang_disposeString(file_name);

            const path = mem.sliceTo(c.clang_getCString(file_name), 0);
            if (mem.indexOf(u8, path, ".framework")) |eon| {
                var name = path[0..eon];
                if (mem.lastIndexOf(u8, name, "/")) |start| {
                    name = name[start + 1 ..];
                }
                break :blk .{ .framework = builder.allocName(name) };
            }
        }
        break :blk .{ .runtime = {} };
    };
    const name_spelling = c.clang_getCursorSpelling(cursor);
    var name = mem.sliceTo(c.clang_getCString(name_spelling), 0);
    if (c.clang_Cursor_isAnonymous(cursor) > 0) {
        name = fmt.allocPrintZ(builder.arena, "anon{}{}", .{ line, column }) catch {
            @panic("OOM");
        };
    }
    defer c.clang_disposeString(name_spelling);

    const kind = c.clang_getCursorKind(cursor);
    switch (kind) {
        c.CXCursor_TypedefDecl => {
            const child_type = c.clang_getTypedefDeclUnderlyingType(cursor);
            const child = builder.analyzeType(origin, child_type);

            const typedef = builder.allocType();
            typedef.* = .{
                .named = .{
                    .name = builder.allocName(name),
                    .cursor = cursor,
                    .origin = origin,
                    .tag = .{
                        .typedef = .{
                            .child = child,
                        },
                    },
                },
            };
            builder.registry.insert(&typedef.named);

            return c.CXChildVisit_Continue;
        },
        c.CXCursor_UnionDecl => {
            const union_decl = builder.allocType();
            union_decl.* = .{
                .named = .{
                    .name = builder.allocName(name),
                    .cursor = cursor,
                    .origin = origin,
                    .tag = .{
                        .@"union" = .{
                            .fields = std.ArrayList(*Type.Named.Field).init(builder.gpa),
                        },
                    },
                },
            };
            builder.registry.insert(&union_decl.named);
            builder.push(&union_decl.named);

            return c.CXChildVisit_Recurse;
        },
        c.CXCursor_StructDecl => {
            const struct_decl = builder.allocType();
            struct_decl.* = .{
                .named = .{
                    .name = builder.allocName(name),
                    .cursor = cursor,
                    .origin = origin,
                    .tag = .{
                        .@"struct" = .{
                            .fields = std.ArrayList(*Type.Named.Field).init(builder.gpa),
                        },
                    },
                },
            };
            builder.registry.insert(&struct_decl.named);
            builder.push(&struct_decl.named);

            return c.CXChildVisit_Recurse;
        },
        c.CXCursor_PackedAttr => {
            const parent = builder.parent().?;

            switch (parent.tag) {
                .@"struct" => |*s| {
                    s.@"packed" = 1;
                },
                else => {
                    std.log.err("Unhandled parent tag {s} of {}", .{ parent.name, meta.activeTag(parent.tag) });
                    unreachable;
                },
            }
        },
        c.CXCursor_FieldDecl => {
            const field_inner = builder.analyzeType(origin, c.clang_getCursorType(cursor));
            const field = builder.allocType();
            field.* = .{
                .named = .{
                    .name = builder.allocName(name),
                    .cursor = cursor,
                    .origin = origin,
                    .tag = .{
                        .field = .{
                            .type = field_inner,
                        },
                    },
                },
            };

            const parent = builder.parent().?;
            switch (parent.tag) {
                .@"union" => |*u| {
                    u.fields.append(&field.named.tag.field) catch {
                        @panic("OOM");
                    };
                },
                .@"struct" => |*s| {
                    s.fields.append(&field.named.tag.field) catch {
                        @panic("OOM");
                    };
                },
                else => unreachable,
            }
        },
        c.CXCursor_FunctionDecl => {
            const result = builder.analyzeType(origin, c.clang_getCursorResultType(cursor));
            const function_decl = builder.allocType();
            function_decl.* = .{
                .named = .{
                    .name = builder.allocName(name),
                    .cursor = cursor,
                    .origin = origin,
                    .tag = .{
                        .function = .{
                            .params = std.ArrayList(*Type.Named.Param).init(builder.gpa),
                            .result = result,
                        },
                    },
                },
            };
            builder.registry.insert(&function_decl.named);
            builder.push(&function_decl.named);

            return c.CXChildVisit_Recurse;
        },
        c.CXCursor_ParmDecl => {
            const param_inner = builder.analyzeType(origin, c.clang_getCursorType(cursor));
            const param = builder.allocType();
            param.* = .{
                .named = .{
                    .name = builder.allocName(name),
                    .cursor = cursor,
                    .origin = origin,
                    .tag = .{
                        .param = .{
                            .type = param_inner,
                        },
                    },
                },
            };

            const parent = builder.parent().?;
            switch (parent.tag) {
                .function => |*f| {
                    f.params.append(&param.named.tag.param) catch {
                        @panic("OOM");
                    };
                },
                .method => |*p| {
                    p.params.append(&param.named.tag.param) catch {
                        @panic("OOM");
                    };
                },
                else => unreachable,
            }
        },
        c.CXCursor_TypeRef => {
            if (builder.parent()) |parent| {
                switch (parent.tag) {
                    .function, .method => {},
                    .interface => |i| {
                        for (i.type_parameters.items) |param| {
                            if (mem.eql(u8, param, name)) {
                                i.super.?.tag.type_reference.type_parameters.append(builder.allocName(name)) catch {
                                    @panic("OOM");
                                };
                                break;
                            }
                        }
                    },
                    else => {
                        unreachable;
                    },
                }
            }
        },
        c.CXCursor_EnumDecl => {
            const backing = builder.analyzeType(origin, c.clang_getEnumDeclIntegerType(cursor));
            const enum_decl = builder.allocType();
            enum_decl.* = .{
                .named = .{
                    .name = builder.allocName(name),
                    .cursor = cursor,
                    .origin = origin,
                    .tag = .{
                        .@"enum" = .{
                            .backing = backing,
                            .values = std.ArrayList(Type.Named.Enum.Value).init(builder.gpa),
                        },
                    },
                },
            };
            builder.push(&enum_decl.named);
            builder.registry.insert(&enum_decl.named);

            return c.CXChildVisit_Recurse;
        },
        c.CXCursor_EnumConstantDecl => {
            const value = c.clang_getEnumConstantDeclValue(cursor);

            const parent = builder.parent().?;
            switch (parent.tag) {
                .@"enum" => |*e| {
                    e.values.append(.{
                        .name = builder.allocName(name),
                        .value = @intCast(value),
                    }) catch {
                        @panic("OOM");
                    };
                },
                else => unreachable,
            }
            return c.CXChildVisit_Continue;
        },
        c.CXCursor_ObjCProtocolDecl => {
            const protocol = builder.allocType();
            protocol.* = .{
                .named = .{
                    .name = builder.allocName(name),
                    .cursor = cursor,
                    .origin = origin,
                    .tag = .{
                        .protocol = .{
                            .inherits = std.ArrayList(*Type.Named).init(builder.gpa),
                            .methods = std.ArrayList(*Type.Named.Method).init(builder.gpa),
                        },
                    },
                },
            };
            builder.registry.insert(&protocol.named);
            builder.push(&protocol.named);

            return c.CXChildVisit_Recurse;
        },
        c.CXCursor_ObjCInstanceMethodDecl, c.CXCursor_ObjCClassMethodDecl => {
            const result = builder.analyzeType(origin, c.clang_getCursorResultType(cursor));
            const method = builder.allocType();
            method.* = .{
                .named = .{
                    .name = builder.allocName(name),
                    .cursor = cursor,
                    .origin = origin,
                    .tag = .{
                        .method = .{
                            .result = result,
                            .params = std.ArrayList(*Type.Named.Param).init(builder.gpa),
                        },
                    },
                },
            };
            defer builder.push(&method.named);

            const parent = builder.parent().?;
            switch (parent.tag) {
                .protocol => |*p| {
                    p.methods.append(&method.named.tag.method) catch {
                        @panic("OOM");
                    };
                },
                .interface => |*i| {
                    i.methods.append(&method.named.tag.method) catch {
                        @panic("OOM");
                    };
                },
                else => {
                    std.log.err("Unhandled parent for ObjCMethod {}", .{meta.activeTag(parent.tag)});
                    unreachable;
                },
            }

            return c.CXChildVisit_Recurse;
        },
        c.CXCursor_ObjCPropertyDecl => {
            return c.CXChildVisit_Continue;
        },
        c.CXCursor_ObjCClassRef => {
            const class = builder.allocType();
            class.* = .{
                .named = .{
                    .name = builder.allocName(name),
                    .cursor = cursor,
                    .origin = origin,
                    .tag = .{
                        .class = .{
                            .protocol = null,
                        },
                    },
                },
            };
            builder.registry.insert(&class.named);
            builder.push(&class.named);

            return c.CXChildVisit_Recurse;
        },
        c.CXCursor_ObjCInterfaceDecl => {
            const interface = builder.allocType();
            interface.* = .{
                .named = .{
                    .name = builder.allocName(name),
                    .cursor = cursor,
                    .origin = origin,
                    .tag = .{
                        .interface = .{
                            .type_parameters = std.ArrayList([]const u8).init(builder.gpa),
                            .super = null,
                            .protocols = std.ArrayList(*Type.Named.Protocol).init(builder.gpa),
                            .methods = std.ArrayList(*Type.Named.Method).init(builder.gpa),
                        },
                    },
                },
            };
            builder.registry.insert(&interface.named);
            builder.push(&interface.named);

            return c.CXChildVisit_Recurse;
        },
        c.CXCursor_ObjCProtocolRef => {
            if (builder.parent()) |parent| {
                switch (parent.tag) {
                    .protocol => |*i| {
                        var super: *Type.Named = undefined;
                        if (builder.registry.protocols.get(name)) |s| {
                            super = s;
                        } else {
                            const ref = builder.allocType();
                            ref.* = .{
                                .named = .{
                                    .name = builder.allocName(name),
                                    .cursor = cursor,
                                    .origin = origin,
                                    .tag = .{ .class = .{ .protocol = null } },
                                },
                            };
                            super = &ref.named;
                        }
                        i.inherits.append(super) catch {
                            @panic("OOM");
                        };
                    },
                    .interface => {},
                    else => {},
                }
            }
        },
        c.CXCursor_ObjCIvarDecl => {},
        c.CXCursor_ObjCSuperClassRef => {
            const super = builder.allocType();
            super.* = .{
                .named = .{
                    .name = builder.allocName(name),
                    .cursor = cursor,
                    .origin = origin,
                    .tag = .{
                        .type_reference = .{
                            .type_parameters = std.ArrayList([]const u8).init(builder.gpa),
                        },
                    },
                },
            };
            const parent = builder.parent().?;
            switch (parent.tag) {
                .interface => |*i| {
                    i.super = &super.named;
                },
                else => unreachable,
            }
        },
        c.CXCursor_TemplateTypeParameter => {
            const parent = builder.parent().?;
            switch (parent.tag) {
                .interface => |*i| {
                    i.type_parameters.append(builder.allocName(name)) catch {
                        @panic("OOM");
                    };
                },
                else => unreachable,
            }
        },
        c.CXCursor_ObjCCategoryDecl,
        c.CXCursor_AlignedAttr,
        => {
            // TODO: IMPLEMENT ME
        },
        // TODO: Evaluate if we need this for enum constants.
        c.CXCursor_IntegerLiteral,
        c.CXCursor_UnaryOperator,
        c.CXCursor_BinaryOperator,
        c.CXCursor_ParenExpr,
        c.CXCursor_FlagEnum,
        c.CXCursor_DeclRefExpr,
        // We're not going to implement function bodies
        c.CXCursor_CompoundStmt,
        // TODO: Evaluate if we should do var decls
        c.CXCursor_VarDecl,
        // TODO: Evaluate if this is necessary
        c.CXCursor_ConstAttr,
        // Ignore completely
        c.CXCursor_AsmStmt,
        c.CXCursor_AsmLabelAttr,
        c.CXCursor_UnexposedAttr,
        c.CXCursor_UnexposedExpr,
        c.CXCursor_WarnUnusedResultAttr,
        c.CXCursor_ObjCBoxable,
        c.CXCursor_VisibilityAttr,
        c.CXCursor_ObjCRootClass,
        c.CXCursor_ObjCDesignatedInitializer,
        c.CXCursor_UnexposedDecl,
        c.CXCursor_NSReturnsRetained,
        c.CXCursor_PureAttr,
        c.CXCursor_ObjCException,
        c.CXCursor_ObjCReturnsInnerPointer,
        c.CXCursor_ObjCExplicitProtocolImpl,
        c.CXCursor_IBActionAttr,
        c.CXCursor_ObjCRequiresSuper,
        c.CXCursor_ObjCSubclassingRestricted,
        => {},
        else => {
            std.log.err(
                "Unhandled kind: {s} of name {s}. Parent kind: {s} of name {s}",
                .{
                    c.clang_getCString(c.clang_getCursorKindSpelling(kind)),
                    c.clang_getCString(name_spelling),
                    c.clang_getCString(c.clang_getCursorKindSpelling(c.clang_getCursorKind(parent_cursor))),
                    c.clang_getCString(c.clang_getCursorSpelling(parent_cursor)),
                },
            );
            unreachable;
        },
    }

    return c.CXChildVisit_Continue;
}

const Renderer = struct {
    writer: std.fs.File.Writer,
    frameworks: *const std.StringHashMap(*const Framework),
    registry: *Registry,

    const keyword_remap = std.StaticStringMap([]const u8).initComptime(.{
        .{ "error", "@\"error\"" },
        .{ "align", "@\"align\"" },
        .{ "resume", "@\"resume\"" },
        .{ "suspend", "@\"suspend\"" },
        .{ "type", "@\"type\"" },
    });

    fn init(
        writer: std.fs.File.Writer,
        frameworks: *const std.StringHashMap(*const Framework),
        registry: *Registry,
    ) @This() {
        return .{
            .writer = writer,
            .frameworks = frameworks,
            .registry = registry,
        };
    }

    fn render(self: *@This(), comptime format: []const u8, args: anytype) void {
        self.writer.print(format, args) catch {
            @panic("Failed to write to output file.");
        };
    }

    fn renderFrameworkDecl(self: *@This(), named: *Type.Named) void {
        switch (named.origin) {
            .framework => |f| if (mem.eql(u8, f, self.registry.owner.name)) {
                self.renderNamedDecl(named);
            },
            else => {},
        }
    }

    fn renderNamedName(self: *@This(), named: *Type.Named) void {
        switch (named.origin) {
            .runtime => self.render("objc.{s}", .{named.name}),
            .framework => |f| {
                if (self.frameworks.get(f)) |framework| {
                    var found = false;
                    if (mem.eql(u8, f, self.registry.owner.name)) {
                        found = true;
                    } else {
                        for (self.registry.owner.dependencies) |d| {
                            if (mem.eql(u8, d, f)) {
                                found = true;
                                break;
                            }
                        }
                    }

                    if (!found) {
                        if (self.frameworks.contains(f)) {
                            std.log.err(
                                "{s} depends on framework {s} but is not listed as a dependency to {s} in the manifest.\n",
                                .{
                                    self.registry.owner.name,
                                    f,
                                    self.registry.owner.name,
                                },
                            );
                        }
                        // Let the user know that the framework is also not in the manifest to ease debugging.
                        else {
                            std.log.err(
                                "{s} depends on framework {s} but is not listed as a dependency to {s} in the manifest. {s} is not listed in the manifest at all.\n",
                                .{
                                    self.registry.owner.name,
                                    f,
                                    self.registry.owner.name,
                                    f,
                                },
                            );
                        }
                        unreachable;
                    }

                    var result = named.name;
                    if (framework.remove_prefix.len > 0) {
                        if (mem.startsWith(u8, named.name, framework.remove_prefix)) {
                            result = result[framework.remove_prefix.len..];
                        }
                    }
                    if (framework != self.registry.owner) {
                        self.render("{s}.", .{framework.output_file});
                    }
                    self.render("{s}", .{result});
                } else {
                    std.log.err(
                        "{s} depends on framework {s} but is not listed as a dependency to {s} in the manifest. {s} is not listed in the manifest at all.\n",
                        .{
                            self.registry.owner.name,
                            f,
                            self.registry.owner.name,
                            f,
                        },
                    );
                    unreachable;
                }
            },
        }
    }

    fn renderNamedDecl(self: *@This(), named: *Type.Named) void {
        switch (named.tag) {
            .@"struct" => |s| {
                self.render("pub const ", .{});
                self.renderNamedName(named);
                self.render(" = extern ", .{});
                if (s.@"packed" > 0) {
                    self.render("packed ", .{});
                }
                self.render("struct {{", .{});

                if (s.fields.items.len > 0) {
                    self.render("\n", .{});
                    self.renderFieldDecls(s.fields.items);
                }

                self.render("}};\n\n", .{});
            },
            .@"enum" => |s| {
                self.render("pub const ", .{});
                self.renderNamedName(named);
                self.render(" = enum(", .{});
                self.renderTypeAsIdentifier(s.backing);
                self.render(") {{", .{});

                if (s.values.items.len > 0) {
                    self.render("\n", .{});

                    for (s.values.items) |v| {
                        var name = v.name;
                        var last = name;

                        // Remove the enum type prefix from the entires. This assumes the enumerations are in
                        // PascalCase. By assuming the case we can remove words instead of raw characters.
                        //
                        // NOTE: This has issues with the anon enums and any enumerations that arent in PascalCase
                        var index: u32 = 0;
                        while (index < named.name.len and name.len > 0) : (index += 1) {
                            const a = name[0];
                            const b = named.name[index];

                            if (a != b) {
                                if (ascii.isUpper(a)) {
                                    last = name;
                                }
                                break;
                            }
                            if (ascii.isUpper(b)) {
                                last = name;
                            }
                            name = name[1..];

                            if (index == named.name.len - 1) {
                                last = name;
                            }
                        }

                        self.render("    {s} = {},\n", .{ last, v.value });
                    }
                }

                self.render("}};\n\n", .{});
            },
            .@"union" => |s| {
                self.render("pub const ", .{});
                self.renderNamedName(named);
                self.render(" = extern union {{", .{});

                if (s.fields.items.len > 0) {
                    self.render("\n", .{});
                    self.renderFieldDecls(s.fields.items);
                }

                self.render("}};\n\n", .{});
            },
            .typedef => |t| {
                // Skip C types that have typedefs to add them to the C global namespace.
                switch (t.child.?.*) {
                    .named => |n| {
                        switch (n.tag) {
                            .@"struct", .@"union", .@"enum" => return,
                            else => {},
                        }
                    },
                    else => {},
                }
                self.render("pub const ", .{});
                self.renderNamedName(named);
                self.render(" = ", .{});
                self.renderTypeAsIdentifier(t.child.?);
                self.render(";\n\n", .{});
            },
            .protocol => |p| {
                self.render("/// https://developer.apple.com/documentation/{s}/{s}?language=objc\n", .{ named.origin.framework, named.name });
                self.render("pub const ", .{});
                self.renderNamedName(named);
                self.render(" = opaque {{", .{});
                self.render("\n", .{});

                self.render(
                    "    pub const InternalInfo = objc.ExternProtocol(@This(), &.{{",
                    .{},
                );

                for (p.inherits.items) |n| {
                    self.renderNamedName(n);
                    self.render(", ", .{});
                }
                self.render("}});\n", .{});

                self.render("    pub const as = InternalInfo.as;\n", .{});
                self.render("    pub const retain = InternalInfo.retain;\n", .{});
                self.render("    pub const release = InternalInfo.release;\n", .{});
                self.render("    pub const autorelease = InternalInfo.autorelease;\n", .{});

                if (p.methods.items.len > 0) {
                    self.render("\n", .{});

                    for (p.methods.items) |m| {
                        self.renderNamedDecl(m.asNamed());
                        self.render("\n", .{});
                    }
                }

                self.render("}};\n\n", .{});
            },
            .interface => |i| {
                self.render("/// https://developer.apple.com/documentation/{s}/{s}?language=objc\n", .{ named.origin.framework, named.name });
                self.render("pub const ", .{});
                self.renderNamedName(named);
                self.render(" = opaque {{", .{});
                self.render("\n", .{});

                self.render(
                    "    pub const InternalInfo = objc.ExternalClass(\"{s}\", @This(), ",
                    .{named.name},
                );
                if (i.super) |super| {
                    self.renderNamedName(super);
                } else {
                    self.render("objc.NSObject", .{});
                }
                self.render(", &.{{", .{});
                for (i.protocols.items) |inh| {
                    self.renderTypeAsIdentifier(inh.asNamed().asType());
                    self.render(", ", .{});
                }

                self.render("}});\n", .{});

                self.render("    pub const as = InternalInfo.as;\n", .{});
                self.render("    pub const retain = InternalInfo.retain;\n", .{});
                self.render("    pub const release = InternalInfo.release;\n", .{});
                self.render("    pub const autorelease = InternalInfo.autorelease;\n", .{});
                self.render("    pub const new = InternalInfo.new;\n", .{});
                self.render("    pub const alloc = InternalInfo.alloc;\n", .{});
                self.render("    pub const allocInit = InternalInfo.allocInit;\n", .{});

                if (i.methods.items.len > 0) {
                    self.render("\n", .{});

                    for (i.methods.items) |m| {
                        self.renderNamedDecl(m.asNamed());
                        self.render("\n", .{});
                    }
                }

                self.render("}};\n\n", .{});
            },
            .method => |m| {
                self.render("    pub fn ", .{});
                self.renderTypeAsIdentifier(named.asType());
                self.render("(self: *@This()", .{});

                if (m.params.items.len > 0) {
                    self.render(", ", .{});
                    for (m.params.items, 0..) |param, index| {
                        self.renderNameAvoidKeywords(param.asNamed().name);
                        self.render(": ", .{});
                        self.renderTypeAsIdentifier(param.type);
                        if (m.params.items.len > 3 or index < m.params.items.len - 1) {
                            self.render(", ", .{});
                        }
                    }
                }

                self.render(") ", .{});
                self.renderTypeAsIdentifier(m.result.?);
                self.render(" {{\n", .{});
                self.render(
                    "        return objc.msgSend(self, \"{s}\", ",
                    .{named.name},
                );
                self.renderTypeAsIdentifier(m.result.?);
                self.render(", .{{", .{});
                for (m.params.items, 0..) |param, index| {
                    self.renderNameAvoidKeywords(param.asNamed().name);
                    if (m.params.items.len > 3 or index < m.params.items.len - 1) {
                        self.render(", ", .{});
                    }
                }
                self.render("}});\n    }}\n", .{});
            },
            .function => |f| {
                self.render("pub extern \"{s}\" fn ", .{named.origin.framework});
                self.renderNamedName(named);
                self.render("(", .{});
                for (f.params.items, 0..) |param, index| {
                    if (param.asNamed().name.len > 0) {
                        self.renderNameAvoidKeywords(param.asNamed().name);
                        self.render(": ", .{});
                    }

                    self.renderTypeAsIdentifier(param.type);
                    if (f.params.items.len > 3 or index < f.params.items.len - 1) {
                        self.render(", ", .{});
                    }
                }
                self.render(") callconv(.C) ", .{});
                self.renderTypeAsIdentifier(f.result.?);
                self.render(";\n\n", .{});
            },
            .type_reference,
            .class,
            => {},
            else => unreachable,
        }
    }

    fn renderNameAvoidKeywords(self: *@This(), name: []const u8) void {
        // Change the name of fields that conflict with zig keywords
        var result = name;
        if (keyword_remap.get(name)) |remap| {
            result = remap;
        }
        self.render("{s}", .{result});
    }

    fn renderFieldDecls(self: *@This(), fields: []const *Type.Named.Field) void {
        for (fields) |f| {
            const n = f.asNamed();

            self.render("    ", .{});
            self.renderNameAvoidKeywords(n.name);
            self.render(": ", .{});
            self.renderTypeAsIdentifier(f.type);
            self.render(",\n", .{});
        }
    }

    fn renderTypeAsIdentifier(self: *@This(), @"type": *Type) void {
        switch (@"type".*) {
            .objc_id => self.render("*objc.Id", .{}),
            .objc_class => self.render("*objc.Class", .{}),
            .objc_sel => self.render("*objc.SEL", .{}),
            .instancetype => self.render("*@This()", .{}),
            .void => self.render("void", .{}),
            .int => |i| {
                const num_bits: u32 = @as(u32, i.size) * 8;
                if (i.signed > 0) {
                    self.render("i{}", .{num_bits});
                } else {
                    self.render("u{}", .{num_bits});
                }
            },
            .float => |f| {
                const num_bits: u32 = @as(u32, f.size) * 8;
                self.render("f{}", .{num_bits});
            },
            .pointer, .block_pointer => |p| {
                if (p.nullable > 0) {
                    self.render("?", .{});
                }
                self.render("*", .{});
                if (p.@"const" > 0 or meta.activeTag(p.underlying.*) == .function_proto) {
                    self.render("const ", .{});
                }
                if (meta.activeTag(p.underlying.*) == .void) {
                    self.render("anyopaque", .{});
                } else {
                    self.renderTypeAsIdentifier(p.underlying);
                }
            },
            .array => |a| {
                self.render("[{}] ", .{a.length});
                self.renderTypeAsIdentifier(a.element);
            },
            .function_proto => |f| {
                self.render("fn(", .{});
                for (f.params, 0..) |p, index| {
                    self.renderTypeAsIdentifier(p);
                    if (f.params.len > 3 or index < f.params.len - 1) {
                        self.render(", ", .{});
                    }
                }
                self.render(") callconv(.C) ", .{});
                self.renderTypeAsIdentifier(f.result);
            },
            .named => |*n| switch (n.tag) {
                .method => {
                    var name = n.name;

                    if (keyword_remap.get(name)) |remap| {
                        self.render("{s}", .{remap});
                    } else {
                        const colon_count = mem.count(u8, name, ":");
                        if (colon_count > 0) {
                            var index: usize = 0;
                            while (index < colon_count) : (index += 1) {
                                const next_colon = mem.indexOf(u8, name, ":").?;
                                const current = name[0..next_colon];
                                if (index > 0 and current.len > 0) {
                                    // Capitalize the first letter of the word to match zig coding style.
                                    _ = self.writer.writeByte(ascii.toUpper(current[0])) catch {
                                        unreachable;
                                    };
                                    _ = self.writer.write(current[1..]) catch {
                                        unreachable;
                                    };
                                } else {
                                    _ = self.writer.write(current) catch {
                                        unreachable;
                                    };
                                }
                                name = name[next_colon + 1 ..];
                            }
                        } else {
                            _ = self.writer.write(name) catch {
                                unreachable;
                            };
                        }
                    }
                },
                else => {
                    self.renderNamedName(n);
                },
            },
            else => {
                std.log.err("Unhandled type kind {}.", .{meta.activeTag(@"type".*)});
                unreachable;
            },
        }
    }
};

fn renderFramework(args: struct {
    gpa: Allocator,
    output: std.fs.Dir,
    frameworks: *const std.StringHashMap(*const Framework),
    registry: *Registry,
}) void {
    const path = fmt.allocPrint(args.gpa, "{s}.zig", .{args.registry.owner.output_file}) catch {
        @panic("OOM");
    };
    var output_file = args.output.createFile(path, .{}) catch {
        @panic("Failed to create output file");
    };
    defer output_file.close();

    var self = Renderer.init(output_file.writer(), args.frameworks, args.registry);

    self.render("// THIS FILE IS AUTOGENERATED. MODIFICATIONS WILL NOT BE MAINTAINED.\n\n", .{});
    self.render("const std = @import(\"std\");\n", .{});
    self.render("const objc = @import(\"objc.zig\"); // Objective-C Runtime in zig.\n", .{});
    for (args.registry.owner.dependencies) |d| {
        const dep = args.frameworks.get(d).?;
        self.render("const {s} = @import(\"{s}.zig\"); // Framework dependency {s}.\n", .{ dep.output_file, dep.output_file, dep.name });
    }
    self.render("\n", .{});

    for (self.registry.order.items) |o| {
        const ref = self.registry.lookup(o.tag, o.name);
        self.renderFrameworkDecl(ref.?);
    }
}

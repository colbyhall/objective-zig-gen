const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const fmt = std.fmt;

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
            var frameworks = std.StringHashMap(Framework).init(allocator);
            defer frameworks.deinit();
            for (manifest.value) |framework| {
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

            var parse_framework_work = std.Thread.WaitGroup{};
            for (manifest.value) |framework| {
                thread_pool.spawnWg(
                    &parse_framework_work,
                    parseFramework,
                    .{
                        .{
                            .allocator = allocator,
                            .sdk_path = sdk_path,
                            .framework = framework,
                        },
                    },
                );
            }
            thread_pool.waitAndWork(&parse_framework_work);
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
        };
        const Field = struct {
            type: *Type,
        };
        const Union = struct {
            fields: std.ArrayList(*Type.Named.Field),
        };
        const Struct = struct {
            @"packed": u1 = 0,
            fields: std.ArrayList(*Type.Named.Field),
        };
        const Enum = struct {
            const Value = struct {
                name: []const u8,
                value: i64,
            };
            backing: *Type,
            values: std.ArrayList(Value),
        };
        const Param = struct {
            type: *Type,
        };
        const Function = struct {
            result: ?*Type,
            params: std.ArrayList(*Param),
        };
        const Method = struct {
            result: ?*Type,
            params: std.ArrayList(*Param),
        };
        const Protocol = struct {
            super: ?*Protocol,
            methods: std.ArrayList(*Method),
        };
        const Interface = struct {
            type_parameters: std.ArrayList([]const u8),

            super: ?*Named,
            protocols: std.ArrayList(*Protocol),
            methods: std.ArrayList(*Method),
        };
        const Class = struct {
            protocol: ?*Protocol,
        };
        const TypeReference = struct {
            type_parameters: std.ArrayList([]const u8),
        };

        name: []const u8,
        cursor: c.CXCursor,
        tag: union(enum) {
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
        },

        fn asType(self: *@This()) *Type {
            // HACK: This needs to be based on type info. I haven't even tested if this is right.
            const parent = @intFromPtr(self) - 8;
            return @ptrFromInt(parent);
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
    const Reference = struct {
        const Origin = union(enum) {
            framework: []const u8,
            runtime,
        };

        type: *Type.Named,
        origin: Origin,
    };

    typedefs: std.StringArrayHashMap(Reference),
    unions: std.StringArrayHashMap(Reference),
    structs: std.StringArrayHashMap(Reference),
    functions: std.StringArrayHashMap(Reference),
    enums: std.StringArrayHashMap(Reference),
    protocols: std.StringArrayHashMap(Reference),
    classes: std.StringArrayHashMap(Reference),
    interfaces: std.StringArrayHashMap(Reference),

    const Error = error{
        MultipleTypes,
    } || Allocator.Error;
    fn init(allocator: Allocator) @This() {
        return .{
            .typedefs = std.StringArrayHashMap(Reference).init(allocator),
            .unions = std.StringArrayHashMap(Reference).init(allocator),
            .structs = std.StringArrayHashMap(Reference).init(allocator),
            .functions = std.StringArrayHashMap(Reference).init(allocator),
            .enums = std.StringArrayHashMap(Reference).init(allocator),
            .protocols = std.StringArrayHashMap(Reference).init(allocator),
            .classes = std.StringArrayHashMap(Reference).init(allocator),
            .interfaces = std.StringArrayHashMap(Reference).init(allocator),
        };
    }

    fn insert(self: *@This(), ref: Reference) void {
        const map = switch (ref.type.tag) {
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

        map.put(ref.type.name, ref) catch {
            @panic("OOM");
        };
    }

    fn lookup(self: @This(), name: []const u8) ?*Type.Named {
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
                    return u.type;
                }
                if (self.unions.get(lookup_name)) |u| {
                    return u.type;
                }
                if (self.structs.get(lookup_name)) |u| {
                    return u.type;
                }
                if (self.enums.get(lookup_name)) |u| {
                    return u.type;
                }
                if (self.protocols.get(lookup_name)) |u| {
                    return u.type;
                }
                if (self.classes.get(lookup_name)) |u| {
                    return u.type;
                }
                if (self.interfaces.get(lookup_name)) |u| {
                    return u.type;
                }
            },
            .@"enum" => {
                if (self.enums.get(lookup_name)) |u| {
                    return u.type;
                }
            },
            .@"struct" => {
                if (self.structs.get(lookup_name)) |u| {
                    return u.type;
                }
            },
            .@"union" => {
                if (self.unions.get(lookup_name)) |u| {
                    return u.type;
                }
            },
        }

        return null;
    }
};

const Builder = struct {
    gpa: Allocator,
    stack: std.ArrayList(*Type.Named),
    registry: Registry,
    arena: std.heap.ArenaAllocator,

    fn init(gpa: Allocator) @This() {
        return .{
            .gpa = gpa,
            .stack = std.ArrayList(*Type.Named).init(gpa),
            .registry = Registry.init(gpa),
            .arena = std.heap.ArenaAllocator.init(gpa),
        };
    }

    fn allocType(self: *@This()) *Type {
        return self.arena.allocator().create(Type) catch {
            @panic("OOM");
        };
    }

    fn allocName(self: *@This(), name: []const u8) []const u8 {
        return self.arena.allocator().dupe(u8, name) catch {
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

    fn supersuper(self: @This()) ?*Type.Named {
        if (self.stack.items.len > 1) {
            return self.stack.items[self.stack.items.len - 2];
        }
        return null;
    }

    fn analyzeType(self: *@This(), @"type": c.CXType) *Type {
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
                        .element = self.analyzeType(element),
                        .length = @as(u64, @intCast(length)),
                    },
                };
            },
            c.CXType_Elaborated => {
                const underlying = c.clang_Type_getNamedType(@"type");
                const underlying_name = c.clang_getTypeSpelling(underlying);
                defer c.clang_disposeString(underlying_name);

                const name = self.allocName(mem.sliceTo(c.clang_getCString(underlying_name), 0));
                if (mem.eql(u8, name, "__builtin_va_list")) {
                    result.* = .{
                        .va_list = {},
                    };
                } else if (self.registry.lookup(name)) |u| {
                    return u.asType();
                } else {
                    result.* = .{
                        .named = .{
                            .name = self.allocName(mem.sliceTo(c.clang_getCString(underlying_name), 0)),
                            .cursor = c.clang_getNullCursor(),
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
                const underlying = self.analyzeType(c.clang_getPointeeType(@"type"));
                result.* = .{
                    .pointer = .{
                        .underlying = underlying,
                    },
                };
            },
            c.CXType_BlockPointer => {
                const underlying = self.analyzeType(c.clang_getPointeeType(@"type"));
                result.* = .{
                    .block_pointer = .{
                        .underlying = underlying,
                    },
                };
            },
            c.CXType_FunctionProto => {
                const result_type = self.analyzeType(c.clang_getResultType(@"type"));
                const num_args: usize = @intCast(c.clang_getNumArgTypes(@"type"));
                const params = self.arena.allocator().alloc(*Type, num_args) catch {
                    @panic("OOM");
                };
                for (0..num_args) |index| {
                    params[index] = self.analyzeType(c.clang_getArgType(@"type", @intCast(index)));
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
                if (self.registry.lookup(name)) |u| {
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
                const underlying = self.analyzeType(c.clang_getArrayElementType(@"type"));
                result.* = .{
                    .pointer = .{
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
                std.debug.print("TODO: ObjCObject\n", .{});
            },
            c.CXType_ObjCInterface => {
                const name_spelling = c.clang_getTypeSpelling(@"type");
                const name = self.allocName(mem.sliceTo(c.clang_getCString(name_spelling), 0));
                if (self.registry.lookup(name)) |u| {
                    return u.asType();
                } else {
                    if (mem.eql(u8, name, "Protocol")) {
                        result.* = .{
                            .base_protocol = {},
                        };
                    } else {
                        std.log.err("Failed to find {s}", .{name});
                        unreachable;
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
        allocator: Allocator,
        sdk_path: []const u8,
        framework: Framework,
    },
) void {
    const path = blk: {
        if (args.framework.header_override) |header| {
            break :blk fmt.allocPrintZ(
                args.allocator,
                "{s}/System/Library/Frameworks/{s}.framework/Headers/{s}",
                .{ args.sdk_path, args.framework.name, header },
            );
        }
        break :blk fmt.allocPrintZ(args.allocator, "{s}/System/Library/Frameworks/{s}.framework/Headers/{s}.h", .{
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
    var builder = Builder.init(args.allocator);
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
                .tag = .{
                    .typedef = .{
                        .child = _u128_t,
                    },
                },
            },
        };
        builder.registry.insert(.{
            .type = &typedef.named,
            .origin = .{
                .runtime = {},
            },
        });
    }
    _ = c.clang_visitChildren(cursor, visitor, &builder);
}

fn visitor(cursor: c.CXCursor, parent_cursor: c.CXCursor, client_data: c.CXClientData) callconv(.C) c.CXChildVisitResult {
    const builder: *Builder = @alignCast(@ptrCast(client_data));

    std.debug.print("\n", .{});

    const location = c.clang_getCursorLocation(cursor);
    var file: c.CXFile = undefined;
    var line: c_uint = undefined;
    var column: c_uint = undefined;
    c.clang_getFileLocation(location, &file, &line, &column, null);

    while (builder.stack.items.len > 0) {
        const parent = builder.parent().?;
        if (c.clang_equalCursors(parent.cursor, parent_cursor) == 0) {
            std.debug.print("Popping: {s}\n", .{parent.name});
            builder.pop();
        } else {
            break;
        }
    }

    if (c.clang_getCursorKind(parent_cursor) == c.CXCursor_TranslationUnit) {
        std.debug.assert(builder.stack.items.len == 0);
    }

    const origin: Registry.Reference.Origin = blk: {
        if (file != null) {
            const file_name = c.clang_getFileName(file);
            defer c.clang_disposeString(file_name);
            std.debug.print("{s} line: {} col: {}\n", .{ c.clang_getCString(file_name), line, column });

            const path = mem.sliceTo(c.clang_getCString(file_name), 0);
            if (mem.indexOf(u8, path, ".framework")) |eon| {
                var name = path[0..eon];
                if (mem.lastIndexOf(u8, name, "/")) |start| {
                    name = name[start + 1 ..];
                }
                break :blk .{ .framework = name };
            }
        }
        break :blk .{ .runtime = {} };
    };
    const name_spelling = c.clang_getCursorSpelling(cursor);
    const name = mem.sliceTo(c.clang_getCString(name_spelling), 0);
    defer c.clang_disposeString(name_spelling);

    {
        const kind_spelling = c.clang_getCursorKindSpelling(c.clang_getCursorKind(cursor));
        const parent_kind_spelling = c.clang_getCursorKindSpelling(c.clang_getCursorKind(parent_cursor));
        std.debug.print("Cursor: {s}, Parent Cursor: {s}\n", .{ c.clang_getCString(kind_spelling), c.clang_getCString(parent_kind_spelling) });
    }

    const kind = c.clang_getCursorKind(cursor);
    switch (kind) {
        c.CXCursor_TypedefDecl => {
            std.debug.print("typedef {s}\n", .{name});

            const child_type = c.clang_getTypedefDeclUnderlyingType(cursor);
            const child = builder.analyzeType(child_type);

            const typedef = builder.allocType();
            typedef.* = .{
                .named = .{
                    .name = builder.allocName(name),
                    .cursor = cursor,
                    .tag = .{
                        .typedef = .{
                            .child = child,
                        },
                    },
                },
            };
            builder.registry.insert(.{
                .type = &typedef.named,
                .origin = origin,
            });

            return c.CXChildVisit_Continue;
        },
        c.CXCursor_UnionDecl => {
            std.debug.print("union {s}\n", .{name});

            const union_decl = builder.allocType();
            union_decl.* = .{
                .named = .{
                    .name = builder.allocName(name),
                    .cursor = cursor,
                    .tag = .{
                        .@"union" = .{
                            .fields = std.ArrayList(*Type.Named.Field).init(builder.gpa),
                        },
                    },
                },
            };
            builder.registry.insert(.{
                .type = &union_decl.named,
                .origin = origin,
            });
            builder.push(&union_decl.named);

            return c.CXChildVisit_Recurse;
        },
        c.CXCursor_StructDecl => {
            std.debug.print("struct: {s}\n", .{name});

            const struct_decl = builder.allocType();
            struct_decl.* = .{
                .named = .{
                    .name = builder.allocName(name),
                    .cursor = cursor,
                    .tag = .{
                        .@"struct" = .{
                            .fields = std.ArrayList(*Type.Named.Field).init(builder.gpa),
                        },
                    },
                },
            };
            builder.registry.insert(.{
                .type = &struct_decl.named,
                .origin = origin,
            });
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
                    std.log.err("Unhandled parent tag {s} of {}", .{ parent.name, std.meta.activeTag(parent.tag) });
                    unreachable;
                },
            }
        },
        c.CXCursor_FieldDecl => {
            std.debug.print("\tField: {s}\n", .{name});

            const field_inner = builder.analyzeType(c.clang_getCursorType(cursor));
            const field = builder.allocType();
            field.* = .{
                .named = .{
                    .name = builder.allocName(name),
                    .cursor = cursor,
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
            std.debug.print("fn {s}\n", .{name});

            const result = builder.analyzeType(c.clang_getCursorResultType(cursor));
            const function_decl = builder.allocType();
            function_decl.* = .{
                .named = .{
                    .name = builder.allocName(name),
                    .cursor = cursor,
                    .tag = .{
                        .function = .{
                            .params = std.ArrayList(*Type.Named.Param).init(builder.gpa),
                            .result = result,
                        },
                    },
                },
            };
            builder.registry.insert(.{
                .type = &function_decl.named,
                .origin = origin,
            });
            builder.push(&function_decl.named);

            return c.CXChildVisit_Recurse;
        },
        c.CXCursor_ParmDecl => {
            std.debug.print("\tParam: {s}\n", .{name});

            const param_inner = builder.analyzeType(c.clang_getCursorType(cursor));
            const param = builder.allocType();
            param.* = .{
                .named = .{
                    .name = builder.allocName(name),
                    .cursor = cursor,
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
            std.debug.print("TypeRef: {s}\n", .{name});

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
            std.debug.print("Enum: {s}\n", .{name});

            const backing = builder.analyzeType(c.clang_getEnumDeclIntegerType(cursor));
            const enum_decl = builder.allocType();
            enum_decl.* = .{
                .named = .{
                    .name = builder.allocName(name),
                    .cursor = cursor,
                    .tag = .{
                        .@"enum" = .{
                            .backing = backing,
                            .values = std.ArrayList(Type.Named.Enum.Value).init(builder.gpa),
                        },
                    },
                },
            };
            builder.push(&enum_decl.named);
            builder.registry.insert(.{
                .type = &enum_decl.named,
                .origin = origin,
            });

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
            std.debug.print("Protocol {s}\n", .{name});

            const protocol = builder.allocType();
            protocol.* = .{
                .named = .{
                    .name = builder.allocName(name),
                    .cursor = cursor,
                    .tag = .{
                        .protocol = .{
                            .super = null,
                            .methods = std.ArrayList(*Type.Named.Method).init(builder.gpa),
                        },
                    },
                },
            };
            builder.registry.insert(.{
                .type = &protocol.named,
                .origin = origin,
            });
            builder.push(&protocol.named);

            return c.CXChildVisit_Recurse;
        },
        c.CXCursor_ObjCInstanceMethodDecl, c.CXCursor_ObjCClassMethodDecl => {
            std.debug.print("\tfn {s}\n", .{name});

            const result = builder.analyzeType(c.clang_getCursorResultType(cursor));
            const method = builder.allocType();
            method.* = .{
                .named = .{
                    .name = builder.allocName(name),
                    .cursor = cursor,

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
                .@"struct" => {
                    std.debug.print("{s}\n", .{parent.name});
                    unreachable;
                },
                else => {
                    std.debug.print("{}\n", .{std.meta.activeTag(parent.tag)});
                    unreachable;
                },
            }

            return c.CXChildVisit_Recurse;
        },
        c.CXCursor_ObjCPropertyDecl => {
            return c.CXChildVisit_Continue;
        },
        c.CXCursor_ObjCClassRef => {
            std.debug.print("Class {s}\n", .{name});

            const class = builder.allocType();
            class.* = .{
                .named = .{
                    .name = builder.allocName(name),
                    .cursor = cursor,
                    .tag = .{
                        .class = .{
                            .protocol = null,
                        },
                    },
                },
            };
            builder.registry.insert(.{
                .type = &class.named,
                .origin = origin,
            });
            builder.push(&class.named);

            return c.CXChildVisit_Recurse;
        },
        c.CXCursor_ObjCInterfaceDecl => {
            std.debug.print("Interface {s}\n", .{name});

            const interface = builder.allocType();
            interface.* = .{
                .named = .{
                    .name = builder.allocName(name),
                    .cursor = cursor,
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
            builder.registry.insert(.{
                .type = &interface.named,
                .origin = origin,
            });
            builder.push(&interface.named);

            return c.CXChildVisit_Recurse;
        },
        c.CXCursor_ObjCProtocolRef => {
            std.debug.print("Protocol Ref: {s}\n", .{name});
        },
        c.CXCursor_ObjCIvarDecl => {
            std.debug.print("Ivar: {s}\n", .{name});
        },
        c.CXCursor_ObjCSuperClassRef => {
            const num_template_args = c.clang_Type_getNumTemplateArguments(c.clang_getCursorType(cursor));
            std.debug.print("SuperClassRef: {s} has {} args.\n", .{ name, num_template_args });

            const super = builder.allocType();
            super.* = .{
                .named = .{
                    .name = builder.allocName(name),
                    .cursor = cursor,
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
            std.debug.print("Template Type Parameter: {s}\n", .{name});

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

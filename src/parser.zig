const std = @import("std");
const meta = std.meta;
const mem = std.mem;
const Allocator = mem.Allocator;
const fmt = std.fmt;

const c = @cImport(
    @cInclude("clang-c/Index.h"),
);
const root = @import("root.zig");
const Framework = root.Framework;

pub const Type = union(enum) {
    pub const Named = struct {
        pub const Typedef = struct {
            child: ?*Type,

            pub fn asNamed(self: *@This()) *Type.Named {
                const tag_offset = @offsetOf(Type.Named, "tag");
                const tag: *Tag = @fieldParentPtr("typedef", self);
                return @ptrFromInt(@intFromPtr(tag) - tag_offset);
            }
        };
        pub const Field = struct {
            type: *Type,

            pub fn asNamed(self: *Field) *Type.Named {
                const tag_offset = @offsetOf(Type.Named, "tag");
                const tag: *Tag = @fieldParentPtr("field", self);
                return @ptrFromInt(@intFromPtr(tag) - tag_offset);
            }
        };
        pub const Union = struct {
            fields: std.StringArrayHashMap(*Field),

            pub fn asNamed(self: *@This()) *Type.Named {
                const tag_offset = @offsetOf(Type.Named, "tag");
                const tag: *Tag = @fieldParentPtr("union", self);
                return @ptrFromInt(@intFromPtr(tag) - tag_offset);
            }
        };
        pub const Struct = struct {
            @"packed": u1 = 0,
            fields: std.StringArrayHashMap(*Field),

            pub fn asNamed(self: *@This()) *Type.Named {
                const tag_offset = @offsetOf(Type.Named, "tag");
                const tag: *Tag = @fieldParentPtr("struct", self);
                return @ptrFromInt(@intFromPtr(tag) - tag_offset);
            }
        };
        pub const Enum = struct {
            pub const Value = struct {
                name: []const u8,
                value: i64,
            };
            backing: *Type,
            values: std.ArrayList(Value),

            pub fn asNamed(self: *@This()) *Type.Named {
                const tag_offset = @offsetOf(Type.Named, "tag");
                const tag: *Tag = @fieldParentPtr("enum", self);
                return @ptrFromInt(@intFromPtr(tag) - tag_offset);
            }
        };
        pub const Param = struct {
            type: *Type,

            pub fn asNamed(self: *@This()) *Type.Named {
                const tag_offset = @offsetOf(Type.Named, "tag");
                const tag: *Tag = @fieldParentPtr("param", self);
                return @ptrFromInt(@intFromPtr(tag) - tag_offset);
            }
        };
        pub const Function = struct {
            result: ?*Type,
            params: std.ArrayList(*Param),

            pub fn asNamed(self: *@This()) *Type.Named {
                const tag_offset = @offsetOf(Type.Named, "tag");
                const tag: *Tag = @fieldParentPtr("function", self);
                return @ptrFromInt(@intFromPtr(tag) - tag_offset);
            }
        };
        pub const Method = struct {
            result: ?*Type,
            params: std.ArrayList(*Param),
            kind: enum { instance, class },

            pub fn asNamed(self: *@This()) *Type.Named {
                const tag_offset = @offsetOf(Type.Named, "tag");
                const tag: *Tag = @fieldParentPtr("method", self);
                return @ptrFromInt(@intFromPtr(tag) - tag_offset);
            }
        };
        pub const Protocol = struct {
            inherits: std.ArrayList(*Named),
            methods: std.ArrayList(*Method),

            pub fn asNamed(self: *@This()) *Type.Named {
                const tag_offset = @offsetOf(Type.Named, "tag");
                const tag: *Tag = @fieldParentPtr("protocol", self);
                return @ptrFromInt(@intFromPtr(tag) - tag_offset);
            }
        };
        pub const Interface = struct {
            type_parameters: std.ArrayList([]const u8),

            super: ?*Named,
            protocols: std.ArrayList(*Named),
            methods: std.ArrayList(*Method),

            pub fn asNamed(self: *@This()) *Type.Named {
                const tag_offset = @offsetOf(Type.Named, "tag");
                const tag: *Tag = @fieldParentPtr("interface", self);
                return @ptrFromInt(@intFromPtr(tag) - tag_offset);
            }
        };
        pub const Class = struct {
            pub fn asNamed(self: *@This()) *Type.Named {
                const tag_offset = @offsetOf(Type.Named, "tag");
                const tag: *Tag = @fieldParentPtr("class", self);
                return @ptrFromInt(@intFromPtr(tag) - tag_offset);
            }
        };
        pub const Identifier = struct {
            type_parameters: std.ArrayList([]const u8),

            pub fn asNamed(self: *@This()) *Type.Named {
                const tag_offset = @offsetOf(Type.Named, "tag");
                const tag: *Tag = @fieldParentPtr("type_reference", self);
                return @ptrFromInt(@intFromPtr(tag) - tag_offset);
            }
        };
        pub const Tag = union(enum) {
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
            identifier: Identifier,
            type_param,
        };

        pub const Origin = union(enum) {
            framework: []const u8,
            runtime,
        };

        name: []const u8,

        parent: ?*Named,
        children: std.ArrayList(*Named),

        cursor: c.CXCursor,
        origin: Origin,

        tag: Tag,

        pub fn asType(self: *@This()) *Type {
            return @fieldParentPtr("named", self);
        }
    };

    pub const Int = struct {
        signed: u1,
        size: u7,
    };
    pub const Float = struct {
        size: u4,
    };
    pub const Array = struct {
        element: *Type,
        length: u64,
    };
    pub const Pointer = struct {
        @"const": u1,
        nullable: u1,
        underlying: *Type,
    };
    pub const FunctionProto = struct {
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

pub const Registry = struct {
    const Self = @This();

    owner: *const Framework,

    order: std.ArrayList(Order),
    typedefs: std.StringHashMap(*Type.Named),
    unions: std.StringHashMap(*Type.Named),
    structs: std.StringHashMap(*Type.Named),
    functions: std.StringHashMap(*Type.Named),
    enums: std.StringHashMap(*Type.Named),
    protocols: std.StringHashMap(*Type.Named),
    interfaces: std.StringHashMap(*Type.Named),

    pub const Order = struct {
        tag: meta.Tag(Type.Named.Tag),
        name: []const u8,
    };

    pub const Error = error{
        MultipleTypes,
    } || Allocator.Error;
    pub fn init(owner: *const Framework, allocator: Allocator) @This() {
        return .{
            .owner = owner,

            .order = std.ArrayList(Order).init(allocator),
            .typedefs = std.StringHashMap(*Type.Named).init(allocator),
            .unions = std.StringHashMap(*Type.Named).init(allocator),
            .structs = std.StringHashMap(*Type.Named).init(allocator),
            .functions = std.StringHashMap(*Type.Named).init(allocator),
            .enums = std.StringHashMap(*Type.Named).init(allocator),
            .protocols = std.StringHashMap(*Type.Named).init(allocator),
            .interfaces = std.StringHashMap(*Type.Named).init(allocator),
        };
    }

    pub fn getMap(self: *Self, tag: meta.Tag(Type.Named.Tag)) *std.StringHashMap(*Type.Named) {
        const map = switch (tag) {
            .typedef => &self.typedefs,
            .@"union" => &self.unions,
            .@"struct" => &self.structs,
            .function => &self.functions,
            .@"enum" => &self.enums,
            .protocol => &self.protocols,
            .interface => &self.interfaces,
            else => @panic("Type is not supported by Registry."),
        };

        return map;
    }

    pub fn insert(self: *Self, named: *Type.Named) void {
        const tag = meta.activeTag(named.tag);
        const map = self.getMap(tag);

        if (!map.contains(named.name) and named.parent == null) {
            self.order.append(.{ .tag = tag, .name = named.name }) catch {
                @panic("OOM");
            };
        }

        if (!map.contains(named.name)) {
            map.put(named.name, named) catch {
                @panic("OOM");
            };
        }
    }

    pub fn lookup(self: *Self, tag: meta.Tag(Type.Named.Tag), name: []const u8) ?*Type.Named {
        return self.getMap(tag).get(name);
    }

    pub fn lookupElaborated(self: *Self, name: []const u8) ?*Type.Named {
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
        } else if (mem.indexOf(u8, lookup_name, "const ")) |index| {
            lookup_name = lookup_name[index + 6 ..];
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
    node: std.Progress.Node,

    fn init(owner: *const Framework, gpa: Allocator, arena: Allocator, node: std.Progress.Node) @This() {
        return .{
            .gpa = gpa,
            .arena = arena,

            .stack = std.ArrayList(*Type.Named).init(gpa),
            .registry = Registry.init(owner, gpa),
            .node = node,
        };
    }

    fn allocType(self: *@This()) *Type {
        self.node.completeOne();
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
                            .parent = null,
                            .children = std.ArrayList(*Type.Named).init(self.gpa),
                            .cursor = c.clang_getNullCursor(),
                            .origin = origin,
                            .tag = .{
                                .identifier = .{
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
                if (mem.eql(u8, name, "instancetype")) {
                    result.* = .{
                        .instancetype = {},
                    };
                } else {
                    result.* = .{
                        .named = .{
                            .name = self.allocName(name),
                            .parent = null,
                            .children = std.ArrayList(*Type.Named).init(self.gpa),
                            .cursor = c.clang_getNullCursor(),
                            .origin = origin,
                            .tag = .{
                                .identifier = .{
                                    .type_parameters = std.ArrayList([]const u8).init(self.gpa),
                                },
                            },
                        },
                    };
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
            c.CXType_ObjCObject => {
                result.* = .{ .void = {} };
            },
            c.CXType_ObjCTypeParam => {
                const name_spelling = c.clang_getTypeSpelling(@"type");
                const name = self.allocName(mem.sliceTo(c.clang_getCString(name_spelling), 0));
                if (mem.containsAtLeast(u8, name, 1, "<")) {
                    result.* = .{ .objc_id = {} };
                } else {
                    result.* = .{
                        .named = .{
                            .name = name,
                            .parent = null,
                            .children = std.ArrayList(*Type.Named).init(self.gpa),
                            .origin = origin,
                            .cursor = c.clang_getNullCursor(),
                            .tag = .{
                                .type_param = {},
                            },
                        },
                    };
                }
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
                                .parent = null,
                                .children = std.ArrayList(*Type.Named).init(self.gpa),
                                .origin = origin,
                                .cursor = c.clang_getNullCursor(),
                                .tag = .{
                                    .identifier = .{
                                        .type_parameters = std.ArrayList([]const u8).init(self.gpa),
                                    },
                                },
                            },
                        };
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

pub fn parse(
    args: struct {
        gpa: Allocator,
        arena: Allocator,
        sdk_path: []const u8,
        framework: *const Framework,
        result: *Registry,
        progress: std.Progress.Node,
    },
) void {
    const progress = args.progress.start(args.framework.name, 0);
    defer progress.end();

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
    var builder = Builder.init(args.framework, args.gpa, args.arena, progress);
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
                .parent = null,
                .children = std.ArrayList(*Type.Named).init(builder.gpa),
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
            if (builder.registry.lookup(.typedef, name)) |typedef| {
                typedef.cursor = cursor;
                builder.push(typedef);
            } else {
                const child_type = c.clang_getTypedefDeclUnderlyingType(cursor);
                const child = builder.analyzeType(origin, child_type);

                const typedef = builder.allocType();
                typedef.* = .{
                    .named = .{
                        .name = builder.allocName(name),
                        .parent = builder.parent(),
                        .children = std.ArrayList(*Type.Named).init(builder.gpa),
                        .cursor = cursor,
                        .origin = origin,
                        .tag = .{
                            .typedef = .{
                                .child = child,
                            },
                        },
                    },
                };

                if (builder.parent()) |parent| {
                    parent.children.append(&typedef.named) catch {
                        @panic("OOM");
                    };
                }
                builder.registry.insert(&typedef.named);
                builder.push(&typedef.named);
            }

            return c.CXChildVisit_Recurse;
        },
        c.CXCursor_UnionDecl => {
            if (builder.registry.lookup(.@"union", name)) |union_decl| {
                union_decl.cursor = cursor;
                builder.push(union_decl);
            } else {
                const union_decl = builder.allocType();
                union_decl.* = .{
                    .named = .{
                        .name = builder.allocName(name),
                        .parent = builder.parent(),
                        .children = std.ArrayList(*Type.Named).init(builder.gpa),
                        .cursor = cursor,
                        .origin = origin,
                        .tag = .{
                            .@"union" = .{
                                .fields = std.StringArrayHashMap(*Type.Named.Field).init(builder.gpa),
                            },
                        },
                    },
                };

                if (builder.parent()) |parent| {
                    parent.children.append(&union_decl.named) catch {
                        @panic("OOM");
                    };
                }
                builder.registry.insert(&union_decl.named);
                builder.push(&union_decl.named);
            }

            return c.CXChildVisit_Recurse;
        },
        c.CXCursor_StructDecl => {
            if (builder.registry.lookup(.@"struct", name)) |struct_decl| {
                struct_decl.cursor = cursor;
                builder.push(struct_decl);
            } else {
                const struct_decl = builder.allocType();
                struct_decl.* = .{
                    .named = .{
                        .name = builder.allocName(name),
                        .parent = builder.parent(),
                        .children = std.ArrayList(*Type.Named).init(builder.gpa),
                        .cursor = cursor,
                        .origin = origin,
                        .tag = .{
                            .@"struct" = .{
                                .fields = std.StringArrayHashMap(*Type.Named.Field).init(builder.gpa),
                            },
                        },
                    },
                };

                if (builder.parent()) |parent| {
                    parent.children.append(&struct_decl.named) catch {
                        @panic("OOM");
                    };
                }

                builder.registry.insert(&struct_decl.named);
                builder.push(&struct_decl.named);
            }

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
            const allocname = builder.allocName(name);
            field.* = .{
                .named = .{
                    .name = allocname,
                    .parent = builder.parent(),
                    .children = std.ArrayList(*Type.Named).init(builder.gpa),
                    .cursor = cursor,
                    .origin = origin,
                    .tag = .{
                        .field = .{
                            .type = field_inner,
                        },
                    },
                },
            };

            if (builder.parent()) |parent| {
                parent.children.append(&field.named) catch {
                    @panic("OOM");
                };
            }

            const parent = builder.parent().?;
            switch (parent.tag) {
                .@"union" => |*u| {
                    if (!u.fields.contains(name)) {
                        u.fields.put(allocname, &field.named.tag.field) catch {
                            @panic("OOM");
                        };
                    }
                },
                .@"struct" => |*s| {
                    if (!s.fields.contains(name)) {
                        s.fields.put(allocname, &field.named.tag.field) catch {
                            @panic("OOM");
                        };
                    }
                },
                else => unreachable,
            }
        },
        c.CXCursor_FunctionDecl => {
            if (c.clang_Cursor_isFunctionInlined(cursor) > 0) {
                return c.CXChildVisit_Continue;
            }

            const result = builder.analyzeType(origin, c.clang_getCursorResultType(cursor));
            const function_decl = builder.allocType();
            function_decl.* = .{
                .named = .{
                    .name = builder.allocName(name),
                    .parent = builder.parent(),
                    .children = std.ArrayList(*Type.Named).init(builder.gpa),
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

            if (builder.parent()) |parent| {
                parent.children.append(&function_decl.named) catch {
                    @panic("OOM");
                };
            }

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
                    .parent = builder.parent(),
                    .children = std.ArrayList(*Type.Named).init(builder.gpa),
                    .cursor = cursor,
                    .origin = origin,
                    .tag = .{
                        .param = .{
                            .type = param_inner,
                        },
                    },
                },
            };

            if (builder.parent()) |parent| {
                parent.children.append(&param.named) catch {
                    @panic("OOM");
                };
            }

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
                .typedef => {},
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
                                i.super.?.tag.identifier.type_parameters.append(builder.allocName(name)) catch {
                                    @panic("OOM");
                                };
                                break;
                            }
                        }
                    },
                    .typedef => |*t| {
                        const inner = builder.allocType();
                        inner.* = .{
                            .named = .{
                                .name = builder.allocName(name),
                                .parent = parent,
                                .children = std.ArrayList(*Type.Named).init(builder.gpa),
                                .cursor = cursor,
                                .origin = origin,
                                .tag = .{
                                    .identifier = .{
                                        .type_parameters = std.ArrayList([]const u8).init(builder.gpa),
                                    },
                                },
                            },
                        };
                        t.child = inner;
                    },
                    else => {
                        unreachable;
                    },
                }
            }
        },
        c.CXCursor_EnumDecl => {
            if (builder.registry.lookup(.@"enum", name)) |enum_decl| {
                enum_decl.cursor = cursor;
                builder.push(enum_decl);
            } else {
                const backing = builder.analyzeType(origin, c.clang_getEnumDeclIntegerType(cursor));
                const enum_decl = builder.allocType();
                enum_decl.* = .{
                    .named = .{
                        .name = builder.allocName(name),
                        .parent = builder.parent(),
                        .children = std.ArrayList(*Type.Named).init(builder.gpa),
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

                if (builder.parent()) |parent| {
                    parent.children.append(&enum_decl.named) catch {
                        @panic("OOM");
                    };
                }
                builder.registry.insert(&enum_decl.named);
                builder.push(&enum_decl.named);
            }

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
                    .parent = builder.parent(),
                    .children = std.ArrayList(*Type.Named).init(builder.gpa),
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

            if (builder.parent()) |parent| {
                parent.children.append(&protocol.named) catch {
                    @panic("OOM");
                };
            }

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
                    .parent = builder.parent(),
                    .children = std.ArrayList(*Type.Named).init(builder.gpa),
                    .cursor = cursor,
                    .origin = origin,
                    .tag = .{
                        .method = .{
                            .result = result,
                            .params = std.ArrayList(*Type.Named.Param).init(builder.gpa),
                            .kind = if (kind == c.CXCursor_ObjCInstanceMethodDecl) .instance else .class,
                        },
                    },
                },
            };
            defer builder.push(&method.named);

            if (builder.parent()) |parent| {
                parent.children.append(&method.named) catch {
                    @panic("OOM");
                };
            }

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
            if (mem.eql(u8, name, "Protocol")) {
                class.* = .{
                    .base_protocol = {},
                };
            } else {
                class.* = .{
                    .named = .{
                        .name = builder.allocName(name),
                        .parent = builder.parent(),
                        .children = std.ArrayList(*Type.Named).init(builder.gpa),
                        .cursor = cursor,
                        .origin = origin,
                        .tag = .{
                            .identifier = .{
                                .type_parameters = std.ArrayList([]const u8).init(builder.gpa),
                            },
                        },
                    },
                };

                if (builder.parent()) |parent| {
                    parent.children.append(&class.named) catch {
                        @panic("OOM");
                    };
                }

                builder.push(&class.named);
            }

            return c.CXChildVisit_Recurse;
        },
        c.CXCursor_ObjCInterfaceDecl => {
            const interface = builder.allocType();
            interface.* = .{
                .named = .{
                    .name = builder.allocName(name),
                    .parent = builder.parent(),
                    .children = std.ArrayList(*Type.Named).init(builder.gpa),
                    .cursor = cursor,
                    .origin = origin,
                    .tag = .{
                        .interface = .{
                            .type_parameters = std.ArrayList([]const u8).init(builder.gpa),
                            .super = null,
                            .protocols = std.ArrayList(*Type.Named).init(builder.gpa),
                            .methods = std.ArrayList(*Type.Named.Method).init(builder.gpa),
                        },
                    },
                },
            };

            if (builder.parent()) |parent| {
                parent.children.append(&interface.named) catch {
                    @panic("OOM");
                };
            }

            builder.registry.insert(&interface.named);
            builder.push(&interface.named);

            return c.CXChildVisit_Recurse;
        },
        c.CXCursor_ObjCProtocolRef => {
            if (builder.parent()) |parent| {
                var super: *Type.Named = undefined;
                if (builder.registry.protocols.get(name)) |s| {
                    super = s;
                } else {
                    const ref = builder.allocType();
                    if (mem.eql(u8, name, "Protocol")) {
                        ref.* = .{
                            .base_protocol = {},
                        };
                    } else {
                        ref.* = .{
                            .named = .{
                                .name = builder.allocName(name),
                                .parent = builder.parent(),
                                .children = std.ArrayList(*Type.Named).init(builder.gpa),
                                .cursor = cursor,
                                .origin = origin,
                                .tag = .{
                                    .identifier = .{
                                        .type_parameters = std.ArrayList([]const u8).init(builder.gpa),
                                    },
                                },
                            },
                        };

                        parent.children.append(&ref.named) catch {
                            @panic("OOM");
                        };
                    }
                    super = &ref.named;
                }

                switch (parent.tag) {
                    .protocol => |*i| {
                        i.inherits.append(super) catch {
                            @panic("OOM");
                        };
                    },
                    .interface => |*i| {
                        i.protocols.append(super) catch {
                            @panic("OOM");
                        };
                    },
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
                    .parent = builder.parent(),
                    .children = std.ArrayList(*Type.Named).init(builder.gpa),
                    .cursor = cursor,
                    .origin = origin,
                    .tag = .{
                        .identifier = .{
                            .type_parameters = std.ArrayList([]const u8).init(builder.gpa),
                        },
                    },
                },
            };

            if (builder.parent()) |parent| {
                parent.children.append(&super.named) catch {
                    @panic("OOM");
                };
            }

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
        c.CXCursor_ObjCIndependentClass,
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

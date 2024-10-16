const Parser = @This();

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
    pub const Decleration = struct {
        pub const Typedef = struct {
            child: ?*Type,

            pub fn asNamed(self: *@This()) *Type.Decleration {
                const tag_offset = @offsetOf(Type.Decleration, "tag");
                const tag: *Tag = @fieldParentPtr("typedef", self);
                return @ptrFromInt(@intFromPtr(tag) - tag_offset);
            }
        };
        pub const Field = struct {
            type: ?*Type,

            pub fn asNamed(self: *Field) *Type.Decleration {
                const tag_offset = @offsetOf(Type.Decleration, "tag");
                const tag: *Tag = @fieldParentPtr("field", self);
                return @ptrFromInt(@intFromPtr(tag) - tag_offset);
            }
        };
        pub const Union = struct {
            fields: std.StringArrayHashMap(*Field),

            pub fn asNamed(self: *@This()) *Type.Decleration {
                const tag_offset = @offsetOf(Type.Decleration, "tag");
                const tag: *Tag = @fieldParentPtr("union", self);
                return @ptrFromInt(@intFromPtr(tag) - tag_offset);
            }
        };
        pub const Struct = struct {
            @"packed": u1 = 0,
            fields: std.StringArrayHashMap(*Field),

            pub fn asNamed(self: *@This()) *Type.Decleration {
                const tag_offset = @offsetOf(Type.Decleration, "tag");
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

            pub fn asNamed(self: *@This()) *Type.Decleration {
                const tag_offset = @offsetOf(Type.Decleration, "tag");
                const tag: *Tag = @fieldParentPtr("enum", self);
                return @ptrFromInt(@intFromPtr(tag) - tag_offset);
            }
        };
        pub const Param = struct {
            type: *Type,

            pub fn asNamed(self: *@This()) *Type.Decleration {
                const tag_offset = @offsetOf(Type.Decleration, "tag");
                const tag: *Tag = @fieldParentPtr("param", self);
                return @ptrFromInt(@intFromPtr(tag) - tag_offset);
            }
        };
        pub const Function = struct {
            result: ?*Type,
            params: std.ArrayList(*Param),

            pub fn asNamed(self: *@This()) *Type.Decleration {
                const tag_offset = @offsetOf(Type.Decleration, "tag");
                const tag: *Tag = @fieldParentPtr("function", self);
                return @ptrFromInt(@intFromPtr(tag) - tag_offset);
            }
        };
        pub const Method = struct {
            result: ?*Type,
            params: std.ArrayList(*Param),
            kind: enum { instance, class },

            pub fn asNamed(self: *@This()) *Type.Decleration {
                const tag_offset = @offsetOf(Type.Decleration, "tag");
                const tag: *Tag = @fieldParentPtr("method", self);
                return @ptrFromInt(@intFromPtr(tag) - tag_offset);
            }
        };
        pub const Protocol = struct {
            inherits: std.ArrayList(*Decleration),
            methods: std.ArrayList(*Method),

            pub fn asNamed(self: *@This()) *Type.Decleration {
                const tag_offset = @offsetOf(Type.Decleration, "tag");
                const tag: *Tag = @fieldParentPtr("protocol", self);
                return @ptrFromInt(@intFromPtr(tag) - tag_offset);
            }
        };
        pub const Interface = struct {
            type_parameters: std.ArrayList([]const u8),

            super: ?*Decleration,
            protocols: std.ArrayList(*Decleration),
            methods: std.ArrayList(*Method),

            pub fn asNamed(self: *@This()) *Type.Decleration {
                const tag_offset = @offsetOf(Type.Decleration, "tag");
                const tag: *Tag = @fieldParentPtr("interface", self);
                return @ptrFromInt(@intFromPtr(tag) - tag_offset);
            }
        };
        // TODO: Move this out of named.
        pub const Identifier = struct {
            type_parameters: std.ArrayList(*Type),

            pub fn asNamed(self: *@This()) *Type.Decleration {
                const tag_offset = @offsetOf(Type.Decleration, "tag");
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

        parent: ?*Decleration,
        children: std.ArrayList(*Decleration),

        cursor: c.CXCursor,
        origin: Origin,

        tag: Tag,

        pub fn asType(self: *@This()) *Type {
            return @fieldParentPtr("decleration", self);
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

    // Objective-C primitives
    instancetype,
    objc_class,
    objc_sel,
    objc_id,
    base_protocol,
    block_pointer: Pointer,

    // C and Objective-C declerations
    decleration: Decleration,

    // C primitives
    va_list,
    void,
    bool,
    char,
    int: Int,
    float: Float,
    pointer: Pointer,
    array: Array,
    function_proto: FunctionProto,

    pub fn print(self: *@This()) void {
        switch (self.*) {
            .va_list => {
                std.debug.print("va_list", .{});
            },
            .instancetype => {
                std.debug.print("instancetype", .{});
            },
            .void => {
                std.debug.print("void", .{});
            },
            .bool => {
                std.debug.print("bool", .{});
            },
            .objc_class => {
                std.debug.print("objc.Class", .{});
            },
            .objc_sel => {
                std.debug.print("objc.Selector", .{});
            },
            .objc_id => {
                std.debug.print("objc.Id", .{});
            },
            .base_protocol => {
                std.debug.print("Protocol", .{});
            },
            .decleration => |n| switch (n.tag) {
                .typedef => |t| {
                    std.debug.print("const {s} = ", .{n.name});
                    t.child.?.print();
                },
                .field => |f| {
                    std.debug.print("{s}: ", .{n.name});
                    f.type.?.print();
                },
                .@"union" => |u| {
                    std.debug.print("const {s} = union {{\n", .{n.name});
                    var iter = u.fields.iterator();
                    while (iter.next()) |field| {
                        std.debug.print("    ", .{});
                        field.value_ptr.*.asNamed().asType().print();
                        std.debug.print(",\n", .{});
                    }
                    std.debug.print("}}", .{});
                },
                .@"struct" => |u| {
                    std.debug.print("const {s} = struct {{\n", .{n.name});
                    var iter = u.fields.iterator();
                    while (iter.next()) |field| {
                        std.debug.print("    ", .{});
                        field.value_ptr.*.asNamed().asType().print();
                        std.debug.print(",\n", .{});
                    }
                    std.debug.print("}}", .{});
                },
                .@"enum" => |e| {
                    std.debug.print("const {s} = enum(", .{n.name});
                    e.backing.print();
                    std.debug.print(") {{\n", .{});
                    for (e.values.items) |v| {
                        std.debug.print("    {s} = {},\n", .{ v.name, v.value });
                    }
                    std.debug.print("}}", .{});
                },
                .param => |p| {
                    std.debug.print("{s}: ", .{n.name});
                    p.type.print();
                },
                .function => |f| {
                    std.debug.print("fn {s}(", .{n.name});
                    for (f.params.items) |p| {
                        p.asNamed().asType().print();
                    }
                    std.debug.print(") ", .{});
                    f.result.?.print();
                },
                .method => |f| {
                    std.debug.print("fn {s}(", .{n.name});
                    for (f.params.items) |p| {
                        p.asNamed().asType().print();
                    }
                    std.debug.print(") ", .{});
                    f.result.?.print();
                },
                .protocol => |p| {
                    std.debug.print("const {s} = protocol(", .{n.name});
                    for (p.inherits.items) |i| {
                        i.asType().print();
                        std.debug.print(", ", .{});
                    }
                    std.debug.print(") {{\n", .{});
                    for (p.methods.items) |m| {
                        std.debug.print("    ", .{});
                        m.asNamed().asType().print();
                        std.debug.print("\n", .{});
                    }
                    std.debug.print("}}", .{});
                },
                .interface => |i| {
                    std.debug.print("const {s} = interface", .{n.name});
                    if (i.type_parameters.items.len > 0) {
                        std.debug.print("<", .{});
                        for (i.type_parameters.items) |a| {
                            std.debug.print("{s}, ", .{a});
                        }
                        std.debug.print(">(", .{});
                    }
                    std.debug.print("(super: {s}", .{i.super.?.name});
                    if (i.protocols.items.len > 0) {
                        std.debug.print(", protocols: ", .{});
                        for (i.protocols.items) |p| {
                            std.debug.print("{s}, ", .{p.name});
                        }
                    }
                    std.debug.print(") {{\n", .{});
                    for (i.methods.items) |m| {
                        std.debug.print("    ", .{});
                        m.asNamed().asType().print();
                        std.debug.print("\n", .{});
                    }
                    std.debug.print("}}", .{});
                },
                .identifier => |i| {
                    std.debug.print("{s}", .{n.name});
                    if (i.type_parameters.items.len > 0) {
                        std.debug.print("(", .{});
                        for (i.type_parameters.items) |t| {
                            t.print();
                        }
                        std.debug.print(")", .{});
                    }
                },
                .type_param => {
                    std.debug.print("{s}", .{n.name});
                },
            },
            .char => {
                std.debug.print("char", .{});
            },
            .int => |i| {
                if (i.signed > 0) {
                    std.debug.print("i", .{});
                } else {
                    std.debug.print("u", .{});
                }
                std.debug.print("{}", .{i.size * 8});
            },
            .float => |f| {
                std.debug.print("f{}", .{f.size * 8});
            },
            .block_pointer, .pointer => |p| {
                if (p.nullable > 0) {
                    std.debug.print("?", .{});
                }
                std.debug.print("*", .{});
                if (p.@"const" > 0) {
                    std.debug.print("const ", .{});
                }
                p.underlying.print();
            },
            .array => |a| {
                std.debug.print("[{}]", .{a.length});
                a.element.print();
            },
            .function_proto => |f| {
                std.debug.print("fn(", .{});
                for (f.params) |p| {
                    p.print();
                    std.debug.print(", ", .{});
                }
                std.debug.print(") ", .{});
                f.result.print();
            },
        }
    }
};

/// Lookup table for declerations
pub const Registry = struct {
    const Self = @This();

    owner: *const Framework,

    order: std.ArrayList(Order),
    typedefs: std.StringHashMap(*Type.Decleration),
    unions: std.StringHashMap(*Type.Decleration),
    structs: std.StringHashMap(*Type.Decleration),
    functions: std.StringHashMap(*Type.Decleration),
    enums: std.StringHashMap(*Type.Decleration),
    protocols: std.StringHashMap(*Type.Decleration),
    interfaces: std.StringHashMap(*Type.Decleration),

    pub const Order = struct {
        tag: meta.Tag(Type.Decleration.Tag),
        name: []const u8,
    };

    pub fn init(owner: *const Framework, allocator: Allocator) @This() {
        return .{
            .owner = owner,

            .order = std.ArrayList(Order).init(allocator),
            .typedefs = std.StringHashMap(*Type.Decleration).init(allocator),
            .unions = std.StringHashMap(*Type.Decleration).init(allocator),
            .structs = std.StringHashMap(*Type.Decleration).init(allocator),
            .functions = std.StringHashMap(*Type.Decleration).init(allocator),
            .enums = std.StringHashMap(*Type.Decleration).init(allocator),
            .protocols = std.StringHashMap(*Type.Decleration).init(allocator),
            .interfaces = std.StringHashMap(*Type.Decleration).init(allocator),
        };
    }

    pub fn getMap(self: *Self, tag: meta.Tag(Type.Decleration.Tag)) *std.StringHashMap(*Type.Decleration) {
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

    pub fn insert(self: *Self, named: *Type.Decleration) !void {
        const tag = meta.activeTag(named.tag);
        const map = self.getMap(tag);

        if (!map.contains(named.name) and named.parent == null) {
            try self.order.append(.{ .tag = tag, .name = named.name });
        }

        if (!map.contains(named.name)) {
            try map.put(named.name, named);
        }
    }

    pub fn lookup(self: *Self, tag: meta.Tag(Type.Decleration.Tag), name: []const u8) ?*Type.Decleration {
        return self.getMap(tag).get(name);
    }

    pub fn lookupElaborated(self: *Self, name: []const u8) ?*Type.Decleration {
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

gpa: Allocator,
arena: Allocator,

stack: std.ArrayList(*Type.Decleration),
registry: Registry,
node: std.Progress.Node,

fn allocType(self: *@This()) !*Type {
    self.node.completeOne();
    return self.arena.create(Type);
}

fn dupeString(self: *@This(), name: []const u8) ![]const u8 {
    return self.arena.dupe(u8, name);
}

fn push(self: *@This(), @"type": *Type.Decleration) !void {
    try self.stack.append(@"type");
}

fn pop(self: *@This()) void {
    _ = self.stack.pop();
}

fn reset(self: *@This()) void {
    self.stack.clearRetainingCapacity();
}

fn getParent(self: @This()) ?*Type.Decleration {
    return self.stack.getLastOrNull();
}

fn analyzeType(self: *@This(), origin: Type.Decleration.Origin, @"type": c.CXType) !*Type {
    const result = try self.allocType();
    switch (@"type".kind) {
        c.CXType_Bool => {
            result.* = .{
                .bool = {},
            };
        },
        c.CXType_ExtVector => {
            // TODO: Vector extension
            result.* = .{
                .void = {},
            };
        },
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
                .char = {},
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
                    .element = try self.analyzeType(origin, element),
                    .length = @as(u64, @intCast(length)),
                },
            };
        },
        c.CXType_Record, c.CXType_Elaborated => {
            const underlying = c.clang_Type_getNamedType(@"type");
            const underlying_name = c.clang_getTypeSpelling(underlying);
            defer c.clang_disposeString(underlying_name);

            var name = try self.dupeString(mem.sliceTo(c.clang_getCString(underlying_name), 0));

            // Determine if this is an anonymous type
            if (mem.containsAtLeast(u8, name, 1, "unnamed at")) {
                const offset = mem.indexOf(u8, name, ".h:").?;
                const end = name[offset + 3 ..];
                const next = mem.indexOf(u8, end, ":").?;
                const line = end[0..next];
                const paren = mem.indexOf(u8, end, ")").?;
                const column = end[next + 1 .. paren];
                // Generate stable name that could reference later rendered anonymous declerations.
                name = try fmt.allocPrintZ(self.arena, "anon{s}{s}", .{ line, column });
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
                    .decleration = .{
                        .name = try self.dupeString(name),
                        .parent = null,
                        .children = std.ArrayList(*Type.Decleration).init(self.gpa),
                        .cursor = c.clang_getNullCursor(),
                        .origin = origin,
                        .tag = .{
                            .identifier = .{
                                .type_parameters = std.ArrayList(*Type).init(self.gpa),
                            },
                        },
                    },
                };
            }
        },
        c.CXType_Pointer, c.CXType_ObjCObjectPointer => {
            const @"const" = c.clang_isConstQualifiedType(c.clang_getPointeeType(@"type"));
            const nullable = c.clang_Type_getNullability(@"type") != c.CXTypeNullability_NonNull;
            const underlying = try self.analyzeType(origin, c.clang_getPointeeType(@"type"));
            result.* = .{
                .pointer = .{
                    .@"const" = @intCast(@"const"),
                    .nullable = if (nullable) 1 else 0,
                    .underlying = underlying,
                },
            };
        },
        c.CXType_BlockPointer => {
            const underlying = try self.analyzeType(origin, c.clang_getPointeeType(@"type"));
            result.* = .{
                .block_pointer = .{
                    .@"const" = 0,
                    .nullable = 0,
                    .underlying = underlying,
                },
            };
        },
        c.CXType_FunctionProto => {
            const result_type = try self.analyzeType(origin, c.clang_getResultType(@"type"));
            const num_args: usize = @intCast(c.clang_getNumArgTypes(@"type"));
            const params = try self.arena.alloc(*Type, num_args);
            for (0..num_args) |index| {
                params[index] = try self.analyzeType(origin, c.clang_getArgType(@"type", @intCast(index)));
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
            const name = try self.dupeString(mem.sliceTo(c.clang_getCString(name_spelling), 0));
            if (mem.eql(u8, name, "instancetype")) {
                result.* = .{
                    .instancetype = {},
                };
            } else {
                result.* = .{
                    .decleration = .{
                        .name = name,
                        .parent = null,
                        .children = std.ArrayList(*Type.Decleration).init(self.gpa),
                        .cursor = c.clang_getNullCursor(),
                        .origin = origin,
                        .tag = .{
                            .identifier = .{
                                .type_parameters = std.ArrayList(*Type).init(self.gpa),
                            },
                        },
                    },
                };
            }
        },
        c.CXType_IncompleteArray => {
            const underlying = try self.analyzeType(origin, c.clang_getArrayElementType(@"type"));
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
            const name_spelling = c.clang_getTypeSpelling(@"type");
            var name = try self.dupeString(mem.sliceTo(c.clang_getCString(name_spelling), 0));

            const kindof = "__kindof ";
            if (mem.startsWith(u8, name, kindof)) {
                name = name[kindof.len..];
            }
            if (mem.indexOf(u8, name, "<")) |i| {
                name = name[0..i];
            }

            var type_parameters = std.ArrayList(*Type).init(self.gpa);
            const type_args_count = c.clang_Type_getNumObjCTypeArgs(@"type");
            if (type_args_count > 0) {
                try type_parameters.ensureTotalCapacity(type_args_count);
                for (0..type_args_count) |i| {
                    const arg = c.clang_Type_getObjCTypeArg(@"type", @intCast(i));
                    type_parameters.insertAssumeCapacity(i, try self.analyzeType(origin, arg));
                }
            }
            result.* = .{
                .decleration = .{
                    .name = name,
                    .parent = null,
                    .children = std.ArrayList(*Type.Decleration).init(self.gpa),
                    .origin = origin,
                    .cursor = c.clang_getNullCursor(),
                    .tag = .{
                        .identifier = .{
                            .type_parameters = type_parameters,
                        },
                    },
                },
            };
        },
        c.CXType_ObjCTypeParam => {
            const name_spelling = c.clang_getTypeSpelling(@"type");
            const name = try self.dupeString(mem.sliceTo(c.clang_getCString(name_spelling), 0));
            if (mem.containsAtLeast(u8, name, 1, "<")) {
                result.* = .{ .objc_id = {} };
            } else {
                result.* = .{
                    .decleration = .{
                        .name = name,
                        .parent = null,
                        .children = std.ArrayList(*Type.Decleration).init(self.gpa),
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
            const name = try self.dupeString(mem.sliceTo(c.clang_getCString(name_spelling), 0));
            if (mem.eql(u8, name, "Protocol")) {
                result.* = .{
                    .base_protocol = {},
                };
            } else {
                result.* = .{
                    .decleration = .{
                        .name = name,
                        .parent = null,
                        .children = std.ArrayList(*Type.Decleration).init(self.gpa),
                        .origin = origin,
                        .cursor = c.clang_getNullCursor(),
                        .tag = .{
                            .identifier = .{
                                .type_parameters = std.ArrayList(*Type).init(self.gpa),
                            },
                        },
                    },
                };
            }
        },
        else => {
            const type_kind_spelling = c.clang_getTypeKindSpelling(@"type".kind);
            defer c.clang_disposeString(type_kind_spelling);
            const type_spelling = c.clang_getTypeSpelling(@"type");
            defer c.clang_disposeString(type_spelling);

            std.log.err("Unhandled type of {s} named {s}", .{ c.clang_getCString(type_kind_spelling), c.clang_getCString(type_spelling) });
            return error.UnhandledBranch;
        },
    }
    return result;
}

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

    // Build the path to the framework we're about to parse.
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
    defer args.gpa.free(path);

    // Parse the main header of the framework using libclang
    const index = c.clang_createIndex(0, 0);
    defer c.clang_disposeIndex(index);
    const translate_args = [_][*c]const u8{
        // Ensure clang parses objective-c
        "-x",
        "objective-c",
        // Set a custom path to the sdk. Not sure why but I had to do this.
        "-isysroot",
        args.sdk_path.ptr,
    };
    var unit: c.CXTranslationUnit = undefined;
    // Use clang_parseTranslationUnit2 so we can get an error code
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

    // If we've found an error then we cant recover.
    if (err != c.CXError_Success) {
        std.log.err("Failed to parse {s} due to error code {}", .{ path, err });
        @panic("Failed to parse Objective-C header");
    }

    // Create the builder which is used to store type info to be rendered later.
    var self = Parser{
        .gpa = args.gpa,
        .arena = args.arena,
        .stack = std.ArrayList(*Type.Decleration).init(args.gpa),
        .registry = Registry.init(args.framework, args.gpa),
        .node = progress,
    };

    // Add this type because it was missing for some reason. Its possibly a clang primitive
    {
        const _u128_t = self.allocType() catch {
            @panic("OOM");
        };
        _u128_t.* = .{
            .int = .{
                .signed = 0,
                .size = 16,
            },
        };
        const typedef = self.allocType() catch {
            @panic("OOM");
        };
        typedef.* = .{
            .decleration = .{
                .name = "__uint128_t",
                .parent = null,
                .children = std.ArrayList(*Type.Decleration).init(self.gpa),
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
        self.registry.insert(&typedef.decleration) catch {
            @panic("OOM");
        };
    }

    // Start traversing the AST of the parsed header. Pass in the builder as the client_data
    const cursor = c.clang_getTranslationUnitCursor(unit);
    _ = c.clang_visitChildren(cursor, visitorOuter, &self);

    // Store the registry as an out param of args to be used later by a rendering job.
    args.result.* = self.registry;
}

const Error = error{UnhandledBranch} || Allocator.Error;

fn visitorInner(self: *Parser, cursor: c.CXCursor, parent_cursor: c.CXCursor) Error!c.CXChildVisitResult {
    // Since we're using the visitor pattern we need to keep track of parents and their
    // children so we can assemble multi node types and handle child declerations. Every
    // registered decleration stores the last cursor to reference them so we can compare
    // against the inputs of this function to pop items off our stack.
    while (self.stack.items.len > 0) {
        const parent = self.getParent().?;
        if (c.clang_equalCursors(parent.cursor, parent_cursor) == 0) {
            self.pop();
        } else {
            break;
        }
    }

    // Items that are in the translation unit should have no parent in the stack.
    if (c.clang_getCursorKind(parent_cursor) == c.CXCursor_TranslationUnit) {
        std.debug.assert(self.stack.items.len == 0);
    }

    // Gather the location of the cursor in terms of the file. We use this to determine the
    // origin if the current node. That could be any framework that the main depends on or an
    // item in the runtime.
    const location = c.clang_getCursorLocation(cursor);
    var file: c.CXFile = undefined;
    var line: c_uint = undefined;
    var column: c_uint = undefined;
    c.clang_getFileLocation(location, &file, &line, &column, null);

    const origin: Type.Decleration.Origin = blk: {
        if (file != null) {
            // Pull the path from the clang file
            const file_name = c.clang_getFileName(file);
            defer c.clang_disposeString(file_name);

            // Parse the framework from the path. All frameworks base directory ends with
            // .framework.
            const path = mem.sliceTo(c.clang_getCString(file_name), 0);
            if (mem.indexOf(u8, path, ".framework")) |eon| {
                var name = path[0..eon];
                if (mem.lastIndexOf(u8, name, "/")) |start| {
                    name = name[start + 1 ..];
                }
                break :blk .{ .framework = try self.dupeString(name) };
            }
        }
        // If we couldn't find a .framework in the path then we're part of the runtime.
        break :blk .{ .runtime = {} };
    };

    // Grab the name of the active cursor. This is typically the name of the type that
    // is being referenced or declared.
    const name_spelling = c.clang_getCursorSpelling(cursor);
    defer c.clang_disposeString(name_spelling);

    // If the cursor is anonymous generate a name based on the line and column number.
    var name = mem.sliceTo(c.clang_getCString(name_spelling), 0);
    // TODO: See if we can get rid of anonymous types and just declare them inline.
    if (c.clang_Cursor_isAnonymous(cursor) > 0) {
        name = try fmt.allocPrintZ(self.arena, "anon{}{}", .{ line, column });
    }

    // Handle all the various node types. There are common patterns between similar ast node kinds.
    // To see what each cursor kind means see
    // https://docs.hdoc.io/hdoc/llvm-project/e4CD2534EBCE47AE4.html
    //
    // Instead of repeating details in each switch statement I'll state the patterns here:
    // a. C type declerations (typedef, struct, union, enum)
    //      1. Query type in registry. If found update the cursor for visitor stack purposes.
    //      Otherwise create a new type and add to registery.
    //      2. Push type on visitor stack and return recurse through child nodes to find definition
    //      information.
    //
    const kind = c.clang_getCursorKind(cursor);
    switch (kind) {
        // Typedef declerations
        c.CXCursor_TypedefDecl => {
            if (self.registry.lookup(.typedef, name)) |typedef| {
                typedef.cursor = cursor;
                try self.push(typedef);
            } else {
                const child_type = c.clang_getTypedefDeclUnderlyingType(cursor);
                const child = try self.analyzeType(origin, child_type);

                const typedef = try self.allocType();
                typedef.* = .{
                    .decleration = .{
                        .name = try self.dupeString(name),
                        .parent = self.getParent(),
                        .children = std.ArrayList(*Type.Decleration).init(self.gpa),
                        .cursor = cursor,
                        .origin = origin,
                        .tag = .{
                            .typedef = .{
                                .child = child,
                            },
                        },
                    },
                };

                if (self.getParent()) |parent| {
                    try parent.children.append(&typedef.decleration);
                }
                try self.registry.insert(&typedef.decleration);
                try self.push(&typedef.decleration);
            }

            // Typedefs can contain further declerations or type references as child nodes.
            return c.CXChildVisit_Recurse;
        },
        // Union declerations
        c.CXCursor_UnionDecl => {
            if (self.registry.lookup(.@"union", name)) |union_decl| {
                union_decl.cursor = cursor;
                try self.push(union_decl);
            } else {
                const union_decl = try self.allocType();
                union_decl.* = .{
                    .decleration = .{
                        .name = try self.dupeString(name),
                        .parent = self.getParent(),
                        .children = std.ArrayList(*Type.Decleration).init(self.gpa),
                        .cursor = cursor,
                        .origin = origin,
                        .tag = .{
                            .@"union" = .{
                                .fields = std.StringArrayHashMap(*Type.Decleration.Field).init(self.gpa),
                            },
                        },
                    },
                };

                if (self.getParent()) |parent| {
                    try parent.children.append(&union_decl.decleration);
                    switch (parent.tag) {
                        .@"struct", .@"union", .protocol, .interface => {},
                        .typedef => |*t| t.child = union_decl,
                        .field => |*f| f.type = union_decl,
                        else => try logUnhandledParentTag(name, "CXCursor_UnionDecl", parent),
                    }
                }

                try self.registry.insert(&union_decl.decleration);
                try self.push(&union_decl.decleration);
            }

            // Unions can have fields. See branch CXCursor_FieldDecl.
            return c.CXChildVisit_Recurse;
        },
        // Struct declerations
        c.CXCursor_StructDecl => {
            if (self.registry.lookup(.@"struct", name)) |struct_decl| {
                struct_decl.cursor = cursor;
                try self.push(struct_decl);
            } else {
                const struct_decl = try self.allocType();
                struct_decl.* = .{
                    .decleration = .{
                        .name = try self.dupeString(name),
                        .parent = self.getParent(),
                        .children = std.ArrayList(*Type.Decleration).init(self.gpa),
                        .cursor = cursor,
                        .origin = origin,
                        .tag = .{
                            .@"struct" = .{
                                .fields = std.StringArrayHashMap(*Type.Decleration.Field).init(self.gpa),
                            },
                        },
                    },
                };

                if (self.getParent()) |parent| {
                    try parent.children.append(&struct_decl.decleration);
                    switch (parent.tag) {
                        .@"struct", .@"union", .protocol, .interface => {},
                        .typedef => |*t| t.child = struct_decl,
                        .field => |*f| f.type = struct_decl,
                        else => try logUnhandledParentTag(name, "CXCursor_StructDecl", parent),
                    }
                }

                try self.registry.insert(&struct_decl.decleration);
                try self.push(&struct_decl.decleration);
            }

            // Structs can have fields and attributes. See branch CXCursor_FieldDecl and CXCursor_PackedAttr.
            return c.CXChildVisit_Recurse;
        },
        // Packed attribute for structs. Removes padding betweens struct fields.
        c.CXCursor_PackedAttr => {
            const parent = self.getParent().?;

            switch (parent.tag) {
                .@"struct" => |*s| {
                    s.@"packed" = 1;
                },
                // TODO: Figure out how to do packed array type.
                .field => {},
                else => try logUnhandledParentTag(name, "CXCursor_PackedAttr", parent),
            }
        },
        // Field for a struct or union decl.
        c.CXCursor_FieldDecl => {
            // Analyze the current cursor type and then wrap with Type.Decl.Field
            const field = try self.allocType();
            const owned_name = try self.dupeString(name);
            field.* = .{
                .decleration = .{
                    .name = owned_name,
                    .parent = self.getParent(),
                    .children = std.ArrayList(*Type.Decleration).init(self.gpa),
                    .cursor = cursor,
                    .origin = origin,
                    .tag = .{
                        .field = .{
                            .type = try self.analyzeType(origin, c.clang_getCursorType(cursor)),
                        },
                    },
                },
            };

            const parent = self.getParent().?;
            try parent.children.append(&field.decleration);
            try self.push(&field.decleration);

            // Append the field to the parent struct or union
            switch (parent.tag) {
                .@"union" => |*u| {
                    if (!u.fields.contains(name)) {
                        try u.fields.put(owned_name, &field.decleration.tag.field);
                    }
                },
                .@"struct" => |*s| {
                    if (!s.fields.contains(name)) {
                        try s.fields.put(owned_name, &field.decleration.tag.field);
                    }
                },
                else => try logUnhandledParentTag(name, "CXCursor_FieldDecl", parent),
            }

            return c.CXChildVisit_Recurse;
        },
        // Function decleration
        c.CXCursor_FunctionDecl => {
            // Skip inline functions as we're currently not supporting parsing statements of expressions.
            if (c.clang_Cursor_isFunctionInlined(cursor) > 0) {
                return c.CXChildVisit_Continue;
            }

            // Analyze the return type from the current cursor. We do this instead of using CXCursor_TypeRef
            // for convenience.
            const result = try self.analyzeType(origin, c.clang_getCursorResultType(cursor));
            const function_decl = try self.allocType();
            function_decl.* = .{
                .decleration = .{
                    .name = try self.dupeString(name),
                    .parent = self.getParent(),
                    .children = std.ArrayList(*Type.Decleration).init(self.gpa),
                    .cursor = cursor,
                    .origin = origin,
                    .tag = .{
                        .function = .{
                            .params = std.ArrayList(*Type.Decleration.Param).init(self.gpa),
                            .result = result,
                        },
                    },
                },
            };

            if (self.getParent()) |parent| {
                try parent.children.append(&function_decl.decleration);
            }

            try self.registry.insert(&function_decl.decleration);
            try self.push(&function_decl.decleration);

            // Recurse to discover the functions parameters.
            return c.CXChildVisit_Recurse;
        },
        // Parm decl is any form of function/method parameter. This is very similar to CXCursor_FieldDecl.
        c.CXCursor_ParmDecl => {
            // Analyze the current cursor type for the given param.
            const param_inner = try self.analyzeType(origin, c.clang_getCursorType(cursor));
            const param = try self.allocType();
            param.* = .{
                .decleration = .{
                    .name = try self.dupeString(name),
                    .parent = self.getParent(),
                    .children = std.ArrayList(*Type.Decleration).init(self.gpa),
                    .cursor = cursor,
                    .origin = origin,
                    .tag = .{
                        .param = .{
                            .type = param_inner,
                        },
                    },
                },
            };

            const parent = self.getParent().?;
            try parent.children.append(&param.decleration);

            // Append to function or method.
            switch (parent.tag) {
                .function => |*f| {
                    try f.params.append(&param.decleration.tag.param);
                },
                .method => |*p| {
                    try p.params.append(&param.decleration.tag.param);
                },
                // TODO: Figure out why typedef is needed for this to run successfully.
                .typedef => {},
                // TODO: Figure out if we can ignore this for fields. It seems to be related to fields with function ptrs.
                .field => {},
                else => try logUnhandledParentTag(name, "CXCursor_ParmDecl", parent),
            }
        },
        // This is used by types that need to reference others (Typedefs, interface type parameters).
        c.CXCursor_TypeRef => {
            if (self.getParent()) |parent| {
                switch (parent.tag) {
                    .function, .method => {},
                    .interface => |i| {
                        for (i.type_parameters.items) |param| {
                            if (mem.eql(u8, param, name)) {
                                // TODO: Refactor type references to use Identifier.
                                try i.super.?.tag.identifier.type_parameters.append(
                                    try self.analyzeType(
                                        origin,
                                        c.clang_getCursorType(cursor),
                                    ),
                                );
                                break;
                            }
                        }
                    },
                    .field => {},
                    .typedef => |*t| {
                        const inner = try self.allocType();
                        inner.* = .{
                            .decleration = .{
                                .name = try self.dupeString(name),
                                .parent = parent,
                                .children = std.ArrayList(*Type.Decleration).init(self.gpa),
                                .cursor = cursor,
                                .origin = origin,
                                .tag = .{
                                    .identifier = .{
                                        .type_parameters = std.ArrayList(*Type).init(self.gpa),
                                    },
                                },
                            },
                        };
                        t.child = inner;
                    },
                    else => try logUnhandledParentTag(name, "CXCursor_TypeRef", parent),
                }
            }
        },
        c.CXCursor_EnumDecl => {
            if (self.registry.lookup(.@"enum", name)) |enum_decl| {
                enum_decl.cursor = cursor;
                try self.push(enum_decl);
            } else {
                // Analyze the backing type from the current cursor.
                const backing = try self.analyzeType(origin, c.clang_getEnumDeclIntegerType(cursor));
                const enum_decl = try self.allocType();
                enum_decl.* = .{
                    .decleration = .{
                        .name = try self.dupeString(name),
                        .parent = self.getParent(),
                        .children = std.ArrayList(*Type.Decleration).init(self.gpa),
                        .cursor = cursor,
                        .origin = origin,
                        .tag = .{
                            .@"enum" = .{
                                .backing = backing,
                                .values = std.ArrayList(Type.Decleration.Enum.Value).init(self.gpa),
                            },
                        },
                    },
                };

                if (self.getParent()) |parent| {
                    try parent.children.append(&enum_decl.decleration);
                    switch (parent.tag) {
                        .@"struct", .@"union", .protocol, .interface => {},
                        .typedef => |*t| t.child = enum_decl,
                        .field => |*f| f.type = enum_decl,
                        else => try logUnhandledParentTag(name, "CXCursor_EnumDecl", parent),
                    }
                }

                try self.registry.insert(&enum_decl.decleration);
                try self.push(&enum_decl.decleration);
            }

            // Recurse to find the enum constants.
            return c.CXChildVisit_Recurse;
        },
        // Decleration for enum constants
        c.CXCursor_EnumConstantDecl => {
            // Pull the direct value from the ast. This is easier than parsing the expressions but
            // leads to loss of intention when using enums as flags.
            //
            // TODO: Think about if we should use a signed or unsigned value
            const value = c.clang_getEnumConstantDeclValue(cursor);

            const parent = self.getParent().?;
            switch (parent.tag) {
                .@"enum" => |*e| {
                    try e.values.append(.{ .name = try self.dupeString(name), .value = @intCast(value) });
                },
                else => try logUnhandledParentTag(name, "CXCursor_EnumConstantDecl", parent),
            }
        },
        // Objective-C protocol decleration. Protocols are like the common idea of interfaces. Think of
        // something similar to rust traits. They can inherit other protocols and contain methods.
        c.CXCursor_ObjCProtocolDecl => {
            // Protocols are forward declared through CXCursor_ObjCClassRef.
            const protocol = try self.allocType();
            protocol.* = .{
                .decleration = .{
                    .name = try self.dupeString(name),
                    .parent = self.getParent(),
                    .children = std.ArrayList(*Type.Decleration).init(self.gpa),
                    .cursor = cursor,
                    .origin = origin,
                    .tag = .{
                        .protocol = .{
                            .inherits = std.ArrayList(*Type.Decleration).init(self.gpa),
                            .methods = std.ArrayList(*Type.Decleration.Method).init(self.gpa),
                        },
                    },
                },
            };

            if (self.getParent()) |parent| {
                try parent.children.append(&protocol.decleration);
            }

            try self.registry.insert(&protocol.decleration);
            try self.push(&protocol.decleration);

            // Recurse to find protocol supers and methods.
            return c.CXChildVisit_Recurse;
        },
        // Objective-C method declerations. Class and instance methods are parsed the same. There are some slight
        // differences with how they interact with the runtime when we render them.
        c.CXCursor_ObjCInstanceMethodDecl, c.CXCursor_ObjCClassMethodDecl => {
            // Analyze the result type instead of using CXCursor_TypeRef. Same reasoning as CXCursor_FunctionDecl.
            const result = try self.analyzeType(origin, c.clang_getCursorResultType(cursor));
            const method = try self.allocType();
            method.* = .{
                .decleration = .{
                    .name = try self.dupeString(name),
                    .parent = self.getParent(),
                    .children = std.ArrayList(*Type.Decleration).init(self.gpa),
                    .cursor = cursor,
                    .origin = origin,
                    .tag = .{
                        .method = .{
                            .result = result,
                            .params = std.ArrayList(*Type.Decleration.Param).init(self.gpa),
                            // We store which kind of method because it changes how we interact with the runtime.
                            .kind = if (kind == c.CXCursor_ObjCInstanceMethodDecl) .instance else .class,
                        },
                    },
                },
            };

            const parent = self.getParent().?;
            try parent.children.append(&method.decleration);

            // Only protocols and interfaces can have objc methods.
            switch (parent.tag) {
                .protocol => |*p| try p.methods.append(&method.decleration.tag.method),
                .interface => |*i| try i.methods.append(&method.decleration.tag.method),
                else => try logUnhandledParentTag(
                    name,
                    "CXCursor_ObjCInstanceMethodDecl or CXCursor_ObjCClassMethodDecl",
                    parent,
                ),
            }

            try self.push(&method.decleration);

            // Recurse so we can find the parameters for the methods.
            return c.CXChildVisit_Recurse;
        },
        // Objective-C property decl. Ignore for now as the ast seems to give us methods to access these.
        c.CXCursor_ObjCPropertyDecl => {},
        // A reference to any Objective-C protocol or interface.
        c.CXCursor_ObjCClassRef => {
            const class = try self.allocType();
            // Parse out the name to determine if this is the base protocol. This is currently ingrained in
            // our type system.
            if (mem.eql(u8, name, "Protocol")) {
                class.* = .{ .base_protocol = {} };
            } else {
                // If we have a parent store this type off as an identifier like all other type references.
                if (self.getParent()) |parent| {
                    class.* = .{
                        .decleration = .{
                            .name = try self.dupeString(name),
                            .parent = self.getParent(),
                            .children = std.ArrayList(*Type.Decleration).init(self.gpa),
                            .cursor = cursor,
                            .origin = origin,
                            .tag = .{
                                .identifier = .{
                                    .type_parameters = std.ArrayList(*Type).init(self.gpa),
                                },
                            },
                        },
                    };

                    try parent.children.append(&class.decleration);
                    try self.push(&class.decleration);
                }
                // If we have no parent and our parent is a category decl handled the visitor stack so all
                // children of the category are appened to the original.
                else if (c.clang_getCursorKind(parent_cursor) == c.CXCursor_ObjCCategoryDecl) {
                    // Lookup the interface in the registry. It must have already been declared before the
                    // category.
                    const new_parent = self.registry.lookup(.interface, name).?;
                    // Update the cursor for the visitor stack
                    new_parent.cursor = parent_cursor;
                    try self.push(new_parent);
                }
            }

            // Recurse as there can be type params or methods given this is an identifier reference for a
            // category.
            return c.CXChildVisit_Recurse;
        },
        // Objective-C Interface decleration. These are concrete types in the Objective-C type system. They
        // can implement protocols and have a super interface. They can also have generic params.
        c.CXCursor_ObjCInterfaceDecl => {
            const interface = try self.allocType();
            interface.* = .{
                .decleration = .{
                    .name = try self.dupeString(name),
                    .parent = self.getParent(),
                    .children = std.ArrayList(*Type.Decleration).init(self.gpa),
                    .cursor = cursor,
                    .origin = origin,
                    .tag = .{
                        .interface = .{
                            .type_parameters = std.ArrayList([]const u8).init(self.gpa),
                            .super = null,
                            .protocols = std.ArrayList(*Type.Decleration).init(self.gpa),
                            .methods = std.ArrayList(*Type.Decleration.Method).init(self.gpa),
                        },
                    },
                },
            };

            if (self.getParent()) |parent| {
                try parent.children.append(&interface.decleration);
            }

            try self.registry.insert(&interface.decleration);
            try self.push(&interface.decleration);

            // Recurse to find type params and methods.
            return c.CXChildVisit_Recurse;
        },
        // This node is explicitly used for protocol supers and interface protocols.
        c.CXCursor_ObjCProtocolRef => {
            if (self.getParent()) |parent| {
                const ref = try self.allocType();

                // Parse the name to determine if this is the base protocol type.
                if (mem.eql(u8, name, "Protocol")) {
                    ref.* = .{ .base_protocol = {} };
                } else {
                    // Create an identifier that points to the protocol being referenced.
                    ref.* = .{
                        .decleration = .{
                            .name = try self.dupeString(name),
                            .parent = self.getParent(),
                            .children = std.ArrayList(*Type.Decleration).init(self.gpa),
                            .cursor = cursor,
                            .origin = origin,
                            .tag = .{
                                .identifier = .{
                                    .type_parameters = std.ArrayList(*Type).init(self.gpa),
                                },
                            },
                        },
                    };

                    try parent.children.append(&ref.decleration);
                }

                // Append to the correct parent.
                switch (parent.tag) {
                    .function, .method => {},
                    .protocol => |*i| try i.inherits.append(&ref.decleration),
                    .interface => |*i| try i.protocols.append(&ref.decleration),
                    .typedef => |*t| {
                        const inner = try self.allocType();
                        inner.* = .{
                            .decleration = .{
                                .name = try self.dupeString(name),
                                .parent = parent,
                                .children = std.ArrayList(*Type.Decleration).init(self.gpa),
                                .cursor = cursor,
                                .origin = origin,
                                .tag = .{
                                    .identifier = .{
                                        .type_parameters = std.ArrayList(*Type).init(self.gpa),
                                    },
                                },
                            },
                        };
                        t.child = inner;
                    },
                    else => try logUnhandledParentTag(name, "CXCursor_ObjCProtocolRef", parent),
                }
            }
        },
        // Ignore instance vars as the ast gives us methods to access them.
        c.CXCursor_ObjCIvarDecl => {},
        // This node is explicitly for interface superes.
        c.CXCursor_ObjCSuperClassRef => {
            // Parse the super and then append to the identifier.
            const super = try self.analyzeType(origin, c.clang_getCursorType(cursor));

            if (self.getParent()) |parent| {
                try parent.children.append(&super.decleration);
            }

            const parent = self.getParent().?;
            switch (parent.tag) {
                .interface => |*i| i.super = &super.decleration,
                else => try logUnhandledParentTag(name, "CXCursor_ObjCSuperClassRef", parent),
            }
        },
        // Template type parameter for interface decleration
        c.CXCursor_TemplateTypeParameter => {
            const parent = self.getParent().?;
            switch (parent.tag) {
                .interface => |*i| {
                    // Remove possible duplications.
                    for (i.type_parameters.items) |t| {
                        if (mem.eql(u8, t, name)) {
                            return c.CXChildVisit_Continue;
                        }
                    }

                    try i.type_parameters.append(try self.dupeString(name));
                },
                else => try logUnhandledParentTag(name, "CXCursor_TemplateTypeParameter", parent),
            }
        },
        // Objective-C category decleration. Categories are a way for methods to be added to interfaces
        // outside of the decleration.
        c.CXCursor_ObjCCategoryDecl => {
            // These are handled in a funny way. See CXCursor_ClassRef.
            return c.CXChildVisit_Recurse;
        },
        c.CXCursor_AlignedAttr,
        => {
            // TODO: IMPLEMENT ME
        },
        // Ignore expression nodes for enum constants.
        c.CXCursor_IntegerLiteral,
        c.CXCursor_UnaryOperator,
        c.CXCursor_BinaryOperator,
        c.CXCursor_ParenExpr,
        c.CXCursor_FlagEnum,
        c.CXCursor_DeclRefExpr,
        // We're not going to implement statments or expressions
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
        c.CXCursor_NSConsumesSelf,
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
            return error.UnhandledBranch;
        },
    }

    return c.CXChildVisit_Continue;
}

fn visitorOuter(
    cursor: c.CXCursor,
    parent_cursor: c.CXCursor,
    client_data: c.CXClientData,
) callconv(.C) c.CXChildVisitResult {
    // Conver the client_data back into the builder
    const self: *Parser = @alignCast(@ptrCast(client_data));
    return self.visitorInner(cursor, parent_cursor) catch |err| {
        const location = c.clang_getCursorLocation(cursor);
        var file: c.CXFile = undefined;
        var line: c_uint = undefined;
        var column: c_uint = undefined;
        c.clang_getFileLocation(location, &file, &line, &column, null);

        if (file != null) {
            const file_name = c.clang_getFileName(file);
            defer c.clang_disposeString(file_name);

            std.log.err("Error {s} while parsing ast node:", .{@errorName(err)});
            std.log.err("Cursor pointing to {s} Line: {}, Column: {}", .{ c.clang_getCString(file_name), line, column });
        }
        @panic("Error in visitorInner");
    };
}

fn logUnhandledParentTag(name: []const u8, comptime kind: []const u8, parent: *Type.Decleration) Error!void {
    std.log.err("For {s} of {s}, unhandled parent {s} of {}", .{ name, kind, parent.name, meta.activeTag(parent.tag) });
    return error.UnhandledBranch;
}

fn logCursorLocation(cursor: c.CXCursor) void {
    const location = c.clang_getCursorLocation(cursor);
    var file: c.CXFile = undefined;
    var line: c_uint = undefined;
    var column: c_uint = undefined;
    c.clang_getFileLocation(location, &file, &line, &column, null);

    if (file != null) {
        const file_name = c.clang_getFileName(file);
        defer c.clang_disposeString(file_name);

        std.debug.print("Cursor location. File: {s}, Line: {}, Column: {}\n", .{ c.clang_getCString(file_name), line, column });
    } else {
        std.debug.print("Cursor location could not be retrieved.\n", .{});
    }
}

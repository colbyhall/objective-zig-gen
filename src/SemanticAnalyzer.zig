const std = @import("std");
const Self = @This();

const ASTNode = @import("ASTNode.zig");

arena: std.heap.ArenaAllocator,
declerations: std.StringArrayHashMap(Decleration),
framework: []const u8,

pub const Type = union(enum) {
    void,
    uint64_t,
    pointer: struct {
        type: *Type,
        nullable: u1,
        @"const": u1,
    },
    declared: []const u8,
    block: struct {
        return_type: *Type,
        params: []const Type,
    },

    fn print(self: Type) void {
        switch (self) {
            .void => std.debug.print("void", .{}),
            .uint64_t => std.debug.print("uint164_t", .{}),
            .pointer => |p| {
                if (p.nullable > 0) {
                    std.debug.print("?", .{});
                }
                std.debug.print("*", .{});
                if (p.@"const" > 0) {
                    std.debug.print("const ", .{});
                }
                p.type.print();
            },
            .declared => |d| std.debug.print("{s}", .{d}),
            .block => |b| {
                std.debug.print("* const fn(", .{});
                for (b.params, 0..) |param, index| {
                    param.print();
                    if (index < b.params.len - 1) {
                        std.debug.print(", ", .{});
                    }
                    std.debug.print(") ", .{});
                    b.return_type.print();
                }
            },
        }
    }
};

pub const Method = struct {
    name: []const u8,
    params: []const Param,
    return_type: Type,

    pub const Param = struct {
        name: []const u8,
        type: Type,
    };
};

pub const Property = struct {
    name: []const u8,
    type: Type,
};

pub const Protocol = struct {
    inherits: []const []const u8,
    children: []const Child,

    pub const Child = union(enum) {
        method: Method,
        property: Property,
    };
};

pub const Interface = struct {
    protocols: []const []const u8,
    super: []const u8,
    children: []const Child,

    pub const Child = union(enum) {
        method: Method,
        property: Property,
    };
};

pub const Decleration = struct {
    name: []const u8,
    framework: []const u8,

    type: union(enum) {
        protocol: Protocol,
        interface: Interface,
    },
};

pub const Error = error{
    OutOfMemory,
    UnexpectedKind,
};
pub const Options = struct {
    allocator: std.mem.Allocator,
    framework: []const u8,
};
pub fn run(ast: ASTNode, options: Self.Options) Error!Self {
    if (ast.kind != .TranslationUnitDecl) {
        return error.UnexpectedKind;
    }

    var self = Self{
        .arena = std.heap.ArenaAllocator.init(options.allocator),
        .declerations = std.StringArrayHashMap(Decleration).init(options.allocator),
        .framework = options.framework,
    };

    for (ast.inner) |node| {
        if (node.isImplicit) {
            continue;
        }

        if (node.gatherFramework() == null) {
            continue;
        }

        var decl: ?Decleration = null;
        switch (node.kind) {
            .TypedefDecl => {},
            .ObjCInterfaceDecl => {
                // decl = try self.analyzeObjCInterface(node);
            },
            .ObjCProtocolDecl => {
                decl = try self.analyzeObjCProtocol(node);
            },
            .ObjCCategoryDecl => {},
            .VarDecl => {},
            .FunctionDecl => {},
            .RecordDecl => {},
            .EnumDecl => {},
            .EmptyDecl => {},
            else => try unexpectedKind(&.{
                .TypedefDecl,
                .ObjCInterfaceDecl,
                .ObjCProtocolDecl,
                .ObjCCategoryDecl,
                .VarDecl,
                .FunctionDecl,
                .RecordDecl,
                .EnumDecl,
                .EmptyDecl,
            }, node.kind),
        }

        if (decl) |d| {
            try self.declerations.put(d.name, d);
        }
    }

    var iter = self.declerations.iterator();
    while (iter.next()) |e| {
        const value = e.value_ptr;
        std.debug.print("{s}:\n", .{value.name});
        switch (value.type) {
            .protocol => |p| {
                std.debug.print("\tInherits:\n", .{});
                for (p.inherits) |i| {
                    std.debug.print("\t\t{s}\n", .{i});
                }
                std.debug.print("\tChildren:\n", .{});
                for (p.children) |c| {
                    switch (c) {
                        .method => |m| {
                            var name = m.name;
                            if (std.mem.indexOf(u8, name, ":")) |index| {
                                name = name[0..index];
                            }
                            std.debug.print("\t\tfn {s}(", .{name});
                            for (m.params, 0..) |param, index| {
                                std.debug.print("{s}: ", .{param.name});
                                param.type.print();
                                if (index < m.params.len - 1) {
                                    std.debug.print(", ", .{});
                                }
                            }
                            std.debug.print(") ", .{});
                            m.return_type.print();
                            std.debug.print("\n", .{});
                        },
                        .property => |prop| {
                            std.debug.print("\t\t{s}: ", .{prop.name});
                            prop.type.print();
                            std.debug.print("\n", .{});
                        },
                    }
                }
            },
            .interface => |p| {
                std.debug.print("\tProtocols:\n", .{});
                for (p.protocols) |i| {
                    std.debug.print("\t\t{s}\n", .{i});
                }
                std.debug.print("\tSuper: {s}\n", .{p.super});
                std.debug.print("\tChildren:\n", .{});
                for (p.children) |c| {
                    switch (c) {
                        .method => |m| {
                            var name = m.name;
                            if (std.mem.indexOf(u8, name, ":")) |index| {
                                name = name[0..index];
                            }
                            std.debug.print("\t\tfn {s}(", .{name});
                            for (m.params, 0..) |param, index| {
                                std.debug.print("{s}: ", .{param.name});
                                param.type.print();
                                if (index < m.params.len - 1) {
                                    std.debug.print(", ", .{});
                                }
                            }
                            std.debug.print(") ", .{});
                            m.return_type.print();
                            std.debug.print("\n", .{});
                        },
                        .property => |prop| {
                            std.debug.print("\t\t{s}: ", .{prop.name});
                            prop.type.print();
                            std.debug.print("\n", .{});
                        },
                    }
                }
            },
        }
    }

    return self;
}

fn unexpectedKind(comptime expected: []const ASTNode.Kind, found: ASTNode.Kind) Error!void {
    std.debug.print("Expected '{any}', found '{}'.\n", .{ expected, found });
    return error.UnexpectedKind;
}

fn expectsKind(comptime expects: []const ASTNode.Kind, found: ASTNode.Kind) Error!void {
    inline for (expects) |kind| {
        if (kind == found) return;
    }
    return unexpectedKind(expects, found);
}

fn parseType(self: *Self, string: []const u8) !Type {
    std.debug.print("{s}\n", .{string});
    var slice = string;
    var pointer = false;
    var nullable = false;
    var @"const" = false;
    var @"type": ?[]const u8 = null;
    var result: ?Type = null;
    while (slice.len > 0) {
        var value = slice;
        var at_end = true;
        if (std.mem.indexOf(u8, slice, " ")) |index| {
            value = slice[0..index];
            slice = slice[index + 1 ..];
            at_end = false;
        } else {
            slice = &.{};
        }

        if (std.mem.eql(u8, value, "const")) {
            @"const" = true;
        } else if (std.mem.eql(u8, value, "*")) {
            pointer = true;
        } else if (std.mem.eql(u8, value, "_Nonnull")) {
            at_end = true;
        } else if (std.mem.eql(u8, value, "_Nullable")) {
            nullable = true;
            at_end = true;
        } else {
            @"type" = value;
        }

        if (at_end) {
            var new: ?Type = null;
            if (@"type") |t| {
                if (std.meta.stringToEnum(std.meta.Tag(Type), t)) |payload| {
                    new = switch (payload) {
                        .void => .{ .void = {} },
                        .uint64_t => .{ .uint64_t = {} },
                        else => unreachable,
                    };
                } else {
                    new = .{
                        .declared = try self.arena.allocator().dupe(u8, t),
                    };
                }
            }

            if (pointer) {
                const inner: *Type = try self.arena.allocator().create(Type);
                if (new != null) {
                    // If we're defining a pointer type of a type then this must be the first one. Pointers of pointers are handled below
                    std.debug.assert(result == null);
                    inner.* = new.?;
                } else {
                    inner.* = result.?;
                }
                new = .{
                    .pointer = .{
                        .type = inner,
                        .@"const" = if (@"const") 1 else 0,
                        .nullable = if (nullable) 1 else 0,
                    },
                };
            }

            result = new;
            pointer = false;
            @"const" = false;
            nullable = false;
            @"type" = null;
        }
    }

    return result.?;
}

fn analyzeObjCMethod(self: *Self, node: ASTNode) Error!Method {
    try expectsKind(&.{.ObjCMethodDecl}, node.kind);

    var params = try std.ArrayList(Method.Param).initCapacity(self.arena.allocator(), node.inner.len);
    for (node.inner) |child| {
        switch (child.kind) {
            .ParmVarDecl => {},
            .AvailabilityAttr,
            .NSReturnsRetainedAttr,
            .SwiftAttrAttr,
            .SwiftAsyncNameAttr,
            .SwiftNameAttr,
            .ObjCReturnsInnerPointerAttr,
            .ObjCDesignatedInitializerAttr,
            => {
                continue;
            },
            else => try unexpectedKind(&.{
                .ParmVarDecl,
                .AvailabilityAttr,
                .NSReturnsRetainedAttr,
                .SwiftAttrAttr,
                .SwiftNameAttr,
                .SwiftAsyncNameAttr,
                .ObjCReturnsInnerPointerAttr,
                .ObjCDesignatedInitializerAttr,
            }, child.kind),
        }

        const name = child.name;
        const @"type" = try self.parseType(child.type.?.qualType);
        try params.append(.{
            .name = try self.arena.allocator().dupe(u8, name),
            .type = @"type",
        });
    }

    const return_type = try self.parseType(node.returnType.?.qualType);

    return .{
        .name = try self.arena.allocator().dupe(u8, node.name),
        .params = params.items,
        .return_type = return_type,
    };
}

fn analyzeObjCProperty(self: *Self, node: ASTNode) Error!Property {
    try expectsKind(&.{.ObjCPropertyDecl}, node.kind);

    return .{
        .name = try self.arena.allocator().dupe(u8, node.name),
        .type = try self.parseType(node.type.?.qualType),
    };
}

fn analyzeObjCProtocol(self: *Self, node: ASTNode) Error!?Decleration {
    try expectsKind(&.{.ObjCProtocolDecl}, node.kind);

    if (node.inner.len == 0) {
        return null;
    }

    const inherits: [][]const u8 = if (node.protocols.len > 0) try self.arena.allocator().alloc([]const u8, node.protocols.len) else &.{};
    for (node.protocols, 0..) |protocol, index| {
        try expectsKind(&.{.ObjCProtocolDecl}, protocol.kind);
        inherits[index] = try self.arena.allocator().dupe(u8, protocol.name);
    }

    var children = try std.ArrayList(Protocol.Child).initCapacity(self.arena.allocator(), node.inner.len);
    for (node.inner) |child| {
        switch (child.kind) {
            .ObjCMethodDecl => {
                try children.append(.{ .method = try self.analyzeObjCMethod(child) });
            },
            .ObjCPropertyDecl => {
                try children.append(.{ .property = try self.analyzeObjCProperty(child) });
            },
            .VisibilityAttr => {},
            .AvailabilityAttr => {},
            else => try unexpectedKind(&.{
                .ObjCMethodDecl,
                .ObjCPropertyDecl,
                .VisibilityAttr,
                .AvailabilityAttr,
            }, child.kind),
        }
    }

    return .{
        .name = try self.arena.allocator().dupe(u8, node.name),
        .framework = try self.arena.allocator().dupe(u8, node.gatherFramework().?),
        .type = .{
            .protocol = .{
                .inherits = inherits,
                .children = children.items,
            },
        },
    };
}

fn analyzeObjCInterface(self: *Self, node: ASTNode) Error!?Decleration {
    try expectsKind(&.{.ObjCInterfaceDecl}, node.kind);

    if (node.inner.len == 0) {
        return null;
    }

    const protocols: [][]const u8 = if (node.protocols.len > 0) try self.arena.allocator().alloc([]const u8, node.protocols.len) else &.{};
    for (node.protocols, 0..) |protocol, index| {
        try expectsKind(&.{.ObjCProtocolDecl}, protocol.kind);
        protocols[index] = try self.arena.allocator().dupe(u8, protocol.name);
    }

    var children = try std.ArrayList(Interface.Child).initCapacity(self.arena.allocator(), node.inner.len);
    for (node.inner) |child| {
        switch (child.kind) {
            .ObjCMethodDecl => {
                try children.append(.{ .method = try self.analyzeObjCMethod(child) });
            },
            .ObjCPropertyDecl => {
                try children.append(.{ .property = try self.analyzeObjCProperty(child) });
            },
            .ObjCTypeParamDecl => {
                std.debug.print("{s}\n", .{child.name});
            },
            .ObjCIvarDecl => {},
            .RecordDecl => {},
            .VisibilityAttr => {},
            .AvailabilityAttr => {},
            else => try unexpectedKind(&.{
                .ObjCMethodDecl,
                .ObjCPropertyDecl,
                .ObjCTypeParamDecl,
                .VisibilityAttr,
                .AvailabilityAttr,
                .RecordDecl,
                .ObjCIvarDecl,
            }, child.kind),
        }
    }

    return .{
        .name = try self.arena.allocator().dupe(u8, node.name),
        .framework = try self.arena.allocator().dupe(u8, node.gatherFramework().?),
        .type = .{
            .interface = .{
                .protocols = protocols,
                .super = try self.arena.allocator().dupe(u8, node.super.?.name),
                .children = children.items,
            },
        },
    };
}

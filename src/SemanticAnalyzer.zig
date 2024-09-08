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
    declared: struct {
        name: []const u8,
        params: []const Type,
        nullable: u1 = 0,
    },
    block: struct {
        function: *Type,
        nullable: u1,
    },
    function: struct {
        return_type: *Type,
        params: []const Type,
    },
    instance_type,

    fn print(self: Type) void {
        switch (self) {
            .void => std.debug.print("void", .{}),
            .uint64_t => std.debug.print("uint164_t", .{}),
            .instance_type => std.debug.print("@This()*", .{}),
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
            .declared => |d| {
                if (d.nullable > 0) {
                    std.debug.print("?", .{});
                }
                std.debug.print("{s}", .{d.name});
                if (d.params.len > 0) {
                    std.debug.print("(", .{});
                    for (d.params, 0..) |p, index| {
                        p.print();
                        if (index < d.params.len - 1) {
                            std.debug.print(", ", .{});
                        }
                    }
                    std.debug.print(")", .{});
                }
            },
            .block => |b| {
                if (b.nullable > 0) {
                    std.debug.print("?", .{});
                }
                std.debug.print("* const ", .{});
                b.function.print();
            },
            .function => |f| {
                std.debug.print("fn(", .{});
                for (f.params, 0..) |param, index| {
                    param.print();
                    if (index < f.params.len - 1) {
                        std.debug.print(", ", .{});
                    }
                }
                std.debug.print(") ", .{});
                f.return_type.print();
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
            .TypedefDecl => {
                std.debug.print("Typedef: {s}, Framework: {s}\n", .{ node.name, node.gatherFramework().? });
                unreachable;
            },
            .ObjCInterfaceDecl => {
                decl = try self.analyzeObjCInterface(node);
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

    return self;
}

fn print(self: Self) void {
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

const TypeLexer = struct {
    const Token = union(enum) {
        asterisk,
        nonnull,
        nullable,
        open_arrow,
        close_arrow,
        open_paren,
        close_paren,
        up_arrow,
        @"const",
        comma,
        __kindof,
        id,
        identifier: []const u8,

        fn from(slice: *[]const u8, comptime match: []const u8, comptime result: Token) ?Token {
            if (std.mem.startsWith(u8, slice.*, match)) {
                slice.* = slice.*[match.len..];
                return result;
            }
            return null;
        }
    };
    fn run(allocator: std.mem.Allocator, string: []const u8) ![]const Token {
        var slice = string;
        var tokens = std.ArrayList(Token).init(allocator);
        while (slice.len > 0) {
            // Eat whitespace
            const trimmed = std.mem.trimLeft(u8, slice, " ");
            if (trimmed.len != slice.len) {
                slice = trimmed;
                continue;
            }

            var token: Token = undefined;
            if (Token.from(&slice, "*", .{ .asterisk = {} })) |t| {
                token = t;
            } else if (Token.from(&slice, "_Nonnull", .{ .nonnull = {} })) |t| {
                token = t;
            } else if (Token.from(&slice, "_Nullable_result", .{ .nullable = {} })) |t| {
                token = t;
            } else if (Token.from(&slice, "_Nullable", .{ .nullable = {} })) |t| {
                token = t;
            } else if (Token.from(&slice, "<", .{ .open_arrow = {} })) |t| {
                token = t;
            } else if (Token.from(&slice, ">", .{ .close_arrow = {} })) |t| {
                token = t;
            } else if (Token.from(&slice, "(", .{ .open_paren = {} })) |t| {
                token = t;
            } else if (Token.from(&slice, ")", .{ .close_paren = {} })) |t| {
                token = t;
            } else if (Token.from(&slice, "^", .{ .up_arrow = {} })) |t| {
                token = t;
            } else if (Token.from(&slice, ",", .{ .comma = {} })) |t| {
                token = t;
            } else if (Token.from(&slice, "id", .{ .id = {} })) |t| {
                token = t;
            } else if (Token.from(&slice, "__kindof", .{ .__kindof = {} })) |t| {
                token = t;
            } else if (Token.from(&slice, "const", .{ .@"const" = {} })) |t| {
                token = t;
            } else {
                const identifier_end = std.mem.indexOfAny(u8, slice, "*<>()^, ") orelse slice.len;
                token = .{ .identifier = slice[0..identifier_end] };
                slice = slice[identifier_end..];
            }

            try tokens.append(token);
        }
        return tokens.items;
    }
};

const TokenParser = struct {
    allocator: std.mem.Allocator,
    tokens: []const TypeLexer.Token,

    fn run(allocator: std.mem.Allocator, tokens: []const TypeLexer.Token) !Type {
        var self = TokenParser{
            .allocator = allocator,
            .tokens = tokens,
        };
        if (self.peek(.identifier)) |ident| {
            if (std.mem.eql(u8, ident, "NS_SWIFT_UNAVAILABLE_FROM_ASYNC")) {
                self.consume();
                _ = self.expect(.open_paren);
                while (self.peekThenConsume(.close_paren) == null) {
                    self.consume();
                }
            } else if (std.mem.eql(u8, ident, "API_AVAILABLE")) {
                self.consume();
                _ = self.expect(.open_paren);
                _ = self.expect(.identifier);
                _ = self.expect(.open_paren);
                _ = self.expect(.identifier);
                _ = self.expect(.close_paren);
                _ = self.expect(.close_paren);
            } else if (std.mem.eql(u8, ident, "API_UNAVAILABLE")) {
                self.consume();
                _ = self.expect(.open_paren);
                _ = self.expect(.identifier);
                _ = self.expect(.close_paren);
            }
        }
        return try self.parseOuter();
    }

    fn parseOuter(self: *TokenParser) error{OutOfMemory}!Type {
        _ = self.peekThenConsume(.__kindof);
        const result = try self.parseInner();
        if (self.peekThenConsume(.open_paren)) |_| {
            const is_function = self.peekThenConsume(.asterisk) != null;
            if (!is_function) {
                _ = self.expect(.up_arrow);
            }
            const nullable = self.peekThenConsume(.nullable) != null;
            _ = self.peekThenConsume(.nonnull);
            _ = self.expect(.close_paren);

            _ = self.expect(.open_paren);
            var params = std.ArrayList(Type).init(self.allocator);
            while (true) {
                try params.append(try self.parseOuter());

                if (self.peekThenConsume(.close_paren)) |_| {
                    break;
                }
                _ = self.expect(.comma);
            }
            const return_type = try self.allocator.create(Type);
            return_type.* = result;

            const function_type = try self.allocator.create(Type);
            function_type.* = .{
                .function = .{
                    .return_type = return_type,
                    .params = params.items,
                },
            };

            if (is_function) {
                return .{
                    .pointer = .{
                        .type = function_type,
                        .@"const" = 1,
                        .nullable = if (nullable) 1 else 0,
                    },
                };
            } else {
                return .{
                    .block = .{
                        .function = function_type,
                        .nullable = if (nullable) 1 else 0,
                    },
                };
            }
        }
        return result;
    }

    fn parseInner(self: *TokenParser) error{OutOfMemory}!Type {
        var @"const" = false;
        if (self.peekThenConsume(.@"const")) |_| {
            @"const" = true;
        }
        if (self.peekThenConsume(.id)) |_| {
            if (self.peekThenConsume(.open_arrow)) |_| {
                const inner = try self.allocator.create(Type);
                inner.* = try self.parseInner();
                while (self.peekThenConsume(.close_arrow) == null) {
                    _ = self.expect(.comma);
                    _ = self.expect(.identifier);
                }
                const nullable = self.peekThenConsume(.nullable) != null;
                _ = self.peekThenConsume(.nonnull);
                return .{
                    .pointer = .{
                        .type = inner,
                        .@"const" = 0,
                        .nullable = if (nullable) 1 else 0,
                    },
                };
            }
            const inner = try self.allocator.create(Type);
            inner.* = .{
                .void = {},
            };
            const nullable = self.peekThenConsume(.nullable) != null;
            _ = self.peekThenConsume(.nonnull);
            return .{
                .pointer = .{
                    .type = inner,
                    .@"const" = 0,
                    .nullable = if (nullable) 1 else 0,
                },
            };
        } else {
            const identifier = self.expect(.identifier);
            var params = std.ArrayList(Type).init(self.allocator);
            if (self.peekThenConsume(.open_arrow)) |_| {
                while (true) {
                    try params.append(try self.parseOuter());
                    if (self.peekThenConsume(.close_arrow)) |_| {
                        break;
                    }
                    _ = self.expect(.comma);
                }
            }

            var result: Type = undefined;
            if (std.mem.eql(u8, identifier, "void")) {
                result = Type{
                    .void = {},
                };
            } else if (std.mem.eql(u8, identifier, "instancetype")) {
                _ = self.peekThenConsume(.nonnull);
                result = Type{
                    .instance_type = {},
                };
            } else {
                result = Type{
                    .declared = .{
                        .name = try self.allocator.dupe(u8, identifier),
                        .params = params.items,
                    },
                };
            }

            if (self.peekThenConsume(.asterisk)) |_| {
                const nullable = self.peekThenConsume(.nullable) != null;
                _ = self.peekThenConsume(.nonnull); // This is the default state so consume it if its there
                const inner = try self.allocator.create(Type);
                inner.* = result;
                result = Type{
                    .pointer = .{
                        .type = inner,
                        .@"const" = if (@"const") 1 else 0,
                        .nullable = if (nullable) 1 else 0,
                    },
                };
                return result;
            }

            if (std.meta.activeTag(result) == .declared) {
                const nullable = self.peekThenConsume(.nullable) != null;
                _ = self.peekThenConsume(.nonnull);
                result.declared.nullable = if (nullable) 1 else 0;
            }

            return result;
        }
    }

    fn peek(self: *TokenParser, comptime token: std.meta.Tag(TypeLexer.Token)) ?std.meta.TagPayload(TypeLexer.Token, token) {
        if (self.tokens.len == 0) {
            return null;
        }
        const t = self.tokens[0];
        if (std.meta.activeTag(t) == token) {
            return @field(t, @tagName(token));
        }
        return null;
    }

    fn consume(self: *TokenParser) void {
        self.tokens = self.tokens[1..];
    }

    fn peekThenConsume(self: *TokenParser, comptime token: std.meta.Tag(TypeLexer.Token)) ?std.meta.TagPayload(TypeLexer.Token, token) {
        if (self.peek(token)) |p| {
            self.consume();
            return p;
        }
        return null;
    }

    fn expect(self: *TokenParser, comptime token: std.meta.Tag(TypeLexer.Token)) std.meta.TagPayload(TypeLexer.Token, token) {
        if (self.peekThenConsume(token)) |result| {
            return result;
        }

        if (self.tokens.len == 0) {
            std.debug.print("Expected {} found nothing.", .{token});
            unreachable;
        }
        const t = self.tokens[0];
        std.debug.print("Expected {} found {}\n", .{ token, t });
        unreachable;
    }
};

fn parseType(self: *Self, string: []const u8) !Type {
    // std.debug.print("{s} -> ", .{string});
    const tokens = try TypeLexer.run(self.arena.allocator(), string);
    const result = try TokenParser.run(self.arena.allocator(), tokens);
    // result.print();
    // std.debug.print("\n", .{});
    return result;
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
            .SwiftPrivateAttr,
            .FormatArgAttr,
            .UnavailableAttr,
            .FormatAttr,
            .CFReturnsNotRetainedAttr,
            .SwiftAsyncAttr,
            .DeprecatedAttr,
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
                .SwiftPrivateAttr,
                .FormatArgAttr,
                .UnavailableAttr,
                .FormatAttr,
                .CFReturnsNotRetainedAttr,
                .SwiftAsyncAttr,
                .DeprecatedAttr,
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
        //return null;
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
            .SwiftNameAttr => {},
            .SwiftPrivateAttr => {},
            else => try unexpectedKind(&.{
                .ObjCMethodDecl,
                .ObjCPropertyDecl,
                .VisibilityAttr,
                .AvailabilityAttr,
                .SwiftNameAttr,
                .SwiftPrivateAttr,
            }, child.kind),
        }
    }

    return .{
        .name = try self.arena.allocator().dupe(u8, node.name),
        .framework = try self.arena.allocator().dupe(u8, node.gatherFramework() orelse "Foundation"),
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
        // return null;
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
                // std.debug.print("{s}\n", .{child.name});
            },
            .ObjCIvarDecl => {},
            .RecordDecl => {},
            .VisibilityAttr => {},
            .AvailabilityAttr => {},
            .SwiftPrivateAttr => {},
            .SwiftNameAttr => {},
            .ObjCExceptionAttr => {},
            .ArcWeakrefUnavailableAttr => {},
            .ObjCRootClassAttr => {},
            else => try unexpectedKind(&.{
                .ObjCMethodDecl,
                .ObjCPropertyDecl,
                .ObjCTypeParamDecl,
                .VisibilityAttr,
                .AvailabilityAttr,
                .RecordDecl,
                .ObjCIvarDecl,
                .SwiftPrivateAttr,
                .SwiftNameAttr,
                .ObjCExceptionAttr,
                .ArcWeakrefUnavailableAttr,
                .ObjCRootClassAttr,
            }, child.kind),
        }
    }

    return .{
        .name = try self.arena.allocator().dupe(u8, node.name),
        .framework = try self.arena.allocator().dupe(u8, node.gatherFramework() orelse "Foundation"),
        .type = .{
            .interface = .{
                .protocols = protocols,
                .super = try self.arena.allocator().dupe(u8, node.super.?.name),
                .children = children.items,
            },
        },
    };
}

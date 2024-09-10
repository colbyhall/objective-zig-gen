const SemanticAnalyzer = @This();

const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;
const meta = std.meta;
const Tag = meta.Tag;
const TagPayload = meta.TagPayload;

const ASTNode = @import("ASTNode.zig");

arena: ArenaAllocator,
type_table: Type.Table,
framework: []const u8,

pub const Type = union(enum) {
    pub const Int = struct {
        signed: u1,
        size: u3,
    };
    pub const Float = struct {
        size: u3,
    };
    pub const Pointer = struct {
        type: *const Type,
        nullable: u1,
        @"const": u1,
    };
    pub const Identifier = struct {
        name: []const u8,
        params: []const Type = &.{},
        nullable: u1 = 0,
        @"const": u1 = 0,
    };
    pub const Block = struct {
        function: *const Type,
        nullable: u1,
    };
    pub const Function = struct {
        params: []const Type,
        return_type: *const Type,
    };
    pub const Struct = struct {
        const Field = struct {
            name: []const u8,
            type: *const Type,
        };

        name: []const u8,
        fields: []const Field,
    };
    pub const Method = struct {
        const Param = struct {
            name: []const u8,
            type: Type,
        };
        name: []const u8,
        params: []const Param,
        return_type: *const Type,
    };
    pub const Property = struct {
        name: []const u8,
        type: Type,
    };
    pub const Protocol = struct {
        pub const Child = union(enum) {
            method: Method,
            property: Property,
        };
        name: []const u8,
        inherits: []const Type,
        children: []const Child,
    };
    pub const Interface = struct {
        pub const Child = union(enum) {
            method: Method,
            property: Property,
        };
        name: []const u8,
        super: *const Type,
        protocols: []const Type,
        children: []const Child,
    };
    pub const Typedef = struct {
        name: []const u8,
        inner: *const Type,
    };

    void,
    int: Int,
    float: Float,
    pointer: Pointer,
    identifier: Identifier,
    block: Block,
    function: Function,
    instance_type,
    @"struct": Struct,
    protocol: Protocol,
    interface: Interface,
    typedef: Typedef,

    pub const Table = struct {
        pub const Named = struct {
            type: Type,
            origin: union(enum) {
                framework: []const u8,
                runtime,
            },
        };

        // We have to store seperate hash maps per named type category because ObjC protocols and interfaces
        // can share the same name.
        protocols: std.StringArrayHashMap(Named),
        interfaces: std.StringArrayHashMap(Named),
        typedefs: std.StringArrayHashMap(Named),

        pub const Error = error{
            MultipleTypes,
        } || Allocator.Error;

        pub fn init(allocator: Allocator) @This() {
            return .{
                .protocols = std.StringArrayHashMap(Named).init(allocator),
                .interfaces = std.StringArrayHashMap(Named).init(allocator),
                .typedefs = std.StringArrayHashMap(Named).init(allocator),
            };
        }

        pub fn deinit(self: *@This()) void {
            self.protocols.deinit();
            self.interfaces.deinit();
            self.typdefs.deinit();
        }

        pub fn insert(self: *@This(), named: Named) @This().Error!void {
            switch (named.type) {
                .protocol => try self.protocols.put(named.type.protocol.name, named),
                .interface => try self.interfaces.put(named.type.interface.name, named),
                .typedef => try self.typedefs.put(named.type.typedef.name, named),
                else => @panic("Type is not accepted as named type."),
            }
        }

        pub fn lookup(self: @This(), name: []const u8) @This().Error!?Named {
            var result: ?Named = null;
            if (self.protocols.get(name)) |p| {
                result = p;
            }
            if (self.interfaces.get(name)) |i| {
                if (result != null) {
                    return error.MultipleTypes;
                }
                result = i;
            }
            if (self.typedefs.get(name)) |i| {
                if (result != null) {
                    return error.MultipleTypes;
                }
                result = i;
            }
            return result;
        }

        pub fn lookupProtocol(self: @This(), name: []const u8) ?Named {
            return self.protocols.get(name);
        }

        pub fn lookupInterface(self: @This(), name: []const u8) ?Named {
            return self.interfaces.get(name);
        }

        pub fn lookupTypdef(self: @This(), name: []const u8) ?Named {
            return self.typdefs.get(name);
        }
    };

    const Lexer = struct {
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

            // TODO: Switch on result type as most Tokens are of type void.
            fn from(slice: *[]const u8, comptime match: []const u8, comptime result: Token) ?Token {
                if (mem.startsWith(u8, slice.*, match)) {
                    slice.* = slice.*[match.len..];
                    return result;
                }
                return null;
            }
        };

        const StringToToken = struct {
            string: []const u8,
            token: Token,
        };
        const string_to_token_table = [_]StringToToken{
            .{
                .string = "*",
                .token = .{ .asterisk = {} },
            },
            .{
                .string = "_Nonnull",
                .token = .{ .nonnull = {} },
            },
            .{
                .string = "_Nullable_result",
                .token = .{ .nonnull = {} },
            },
            .{
                .string = "_Nullable",
                .token = .{ .nullable = {} },
            },
            .{
                .string = "<",
                .token = .{ .open_arrow = {} },
            },
            .{
                .string = ">",
                .token = .{ .close_arrow = {} },
            },
            .{
                .string = "(",
                .token = .{ .open_paren = {} },
            },
            .{
                .string = ")",
                .token = .{ .close_paren = {} },
            },
            .{
                .string = "^",
                .token = .{ .up_arrow = {} },
            },
            .{
                .string = ",",
                .token = .{ .comma = {} },
            },
            .{
                .string = "id",
                .token = .{ .id = {} },
            },
            .{
                .string = "__kindof",
                .token = .{ .__kindof = {} },
            },
            .{
                .string = "const",
                .token = .{ .@"const" = {} },
            },
        };

        fn run(allocator: Allocator, string: []const u8) ![]const Token {
            var slice = string;
            var tokens = std.ArrayList(Token).init(allocator);
            while (slice.len > 0) {
                // Eat whitespace
                const trimmed = mem.trimLeft(u8, slice, " ");
                if (trimmed.len != slice.len) {
                    slice = trimmed;
                    continue;
                }

                var token: ?Token = null;
                inline for (string_to_token_table) |string_to_token| {
                    if (token == null) {
                        if (Token.from(&slice, string_to_token.string, string_to_token.token)) |t| {
                            token = t;
                        }
                    }
                }
                if (token == null) {
                    const identifier_end = mem.indexOfAny(u8, slice, "*<>()^, ") orelse slice.len;
                    token = .{ .identifier = slice[0..identifier_end] };
                    slice = slice[identifier_end..];
                }

                try tokens.append(token.?);
            }
            return tokens.items;
        }
    };

    const Parser = struct {
        allocator: Allocator,
        tokens: []const Lexer.Token,

        fn run(allocator: Allocator, tokens: []const Lexer.Token) !Type {
            var self = @This(){
                .allocator = allocator,
                .tokens = tokens,
            };
            if (self.peek(.identifier)) |ident| {
                if (mem.eql(u8, ident, "NS_SWIFT_UNAVAILABLE_FROM_ASYNC")) {
                    self.consume();
                    _ = self.expect(.open_paren);
                    while (self.peekThenConsume(.close_paren) == null) {
                        self.consume();
                    }
                } else if (mem.eql(u8, ident, "API_AVAILABLE")) {
                    self.consume();
                    _ = self.expect(.open_paren);
                    _ = self.expect(.identifier);
                    _ = self.expect(.open_paren);
                    _ = self.expect(.identifier);
                    _ = self.expect(.close_paren);
                    _ = self.expect(.close_paren);
                } else if (mem.eql(u8, ident, "API_UNAVAILABLE")) {
                    self.consume();
                    _ = self.expect(.open_paren);
                    _ = self.expect(.identifier);
                    _ = self.expect(.close_paren);
                }
            }
            return try self.parseOuter();
        }

        fn parseOuter(self: *@This()) error{OutOfMemory}!Type {
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

        fn parseInner(self: *@This()) error{OutOfMemory}!Type {
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
                if (mem.eql(u8, identifier, "void")) {
                    result = Type{
                        .void = {},
                    };
                } else if (mem.eql(u8, identifier, "instancetype")) {
                    _ = self.peekThenConsume(.nonnull);
                    result = Type{
                        .instance_type = {},
                    };
                } else {
                    result = Type{
                        .identifier = .{
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

                if (std.meta.activeTag(result) == .identifier) {
                    const nullable = self.peekThenConsume(.nullable) != null;
                    _ = self.peekThenConsume(.nonnull);
                    result.identifier.nullable = if (nullable) 1 else 0;
                }

                return result;
            }
        }

        fn peek(self: *@This(), comptime token: Tag(Lexer.Token)) ?TagPayload(Lexer.Token, token) {
            if (self.tokens.len == 0) {
                return null;
            }
            const t = self.tokens[0];
            if (std.meta.activeTag(t) == token) {
                return @field(t, @tagName(token));
            }
            return null;
        }

        fn consume(self: *@This()) void {
            self.tokens = self.tokens[1..];
        }

        fn peekThenConsume(self: *@This(), comptime token: Tag(Lexer.Token)) ?TagPayload(Lexer.Token, token) {
            if (self.peek(token)) |p| {
                self.consume();
                return p;
            }
            return null;
        }

        fn expect(self: *@This(), comptime token: Tag(Lexer.Token)) TagPayload(Lexer.Token, token) {
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

    fn parseFromString(allocator: Allocator, string: []const u8) !@This() {
        const tokens = try Lexer.run(allocator, string);
        return Parser.run(allocator, tokens);
    }

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

pub const Error = error{
    UnexpectedKind,
} || Type.Table.Error;
pub const Options = struct {
    allocator: Allocator,
    framework: []const u8,
};
pub fn run(ast: ASTNode, options: SemanticAnalyzer.Options) Error!SemanticAnalyzer {
    if (ast.kind != .TranslationUnitDecl) {
        return error.UnexpectedKind;
    }

    var self = SemanticAnalyzer{
        .arena = ArenaAllocator.init(options.allocator),
        .type_table = Type.Table.init(options.allocator),
        .framework = options.framework,
    };

    for (ast.inner) |node| {
        if (node.isImplicit) {
            continue;
        }

        if (node.gatherFramework() == null) {
            continue;
        }

        var entry: ?Type = null;
        switch (node.kind) {
            .TypedefDecl => {
                std.debug.print("Typedef: {s}, Framework: {s}\n", .{ node.name, node.gatherFramework().? });
                entry = try self.analyzeTypedef(node);
            },
            .ObjCInterfaceDecl => {
                entry = try self.analyzeObjCInterface(node);
            },
            .ObjCProtocolDecl => {
                entry = try self.analyzeObjCProtocol(node);
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

        if (entry) |e| {
            try self.type_table.insert(
                .{
                    .origin = .{ .framework = try self.arena.allocator().dupe(u8, node.gatherFramework().?) },
                    .type = e,
                },
            );
        }
    }

    return self;
}

fn print(self: SemanticAnalyzer) void {
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
                            if (mem.indexOf(u8, name, ":")) |index| {
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
                            if (mem.indexOf(u8, name, ":")) |index| {
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

fn analyzeObjCMethod(self: *SemanticAnalyzer, node: ASTNode) Error!Type.Method {
    try expectsKind(&.{.ObjCMethodDecl}, node.kind);

    var params = try std.ArrayList(Type.Method.Param).initCapacity(self.arena.allocator(), node.inner.len);
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
        const @"type" = try Type.parseFromString(self.arena.allocator(), child.type.?.qualType);
        try params.append(.{
            .name = try self.arena.allocator().dupe(u8, name),
            .type = @"type",
        });
    }

    const return_type = try self.arena.allocator().create(Type);
    return_type.* = try Type.parseFromString(self.arena.allocator(), node.returnType.?.qualType);

    return .{
        .name = try self.arena.allocator().dupe(u8, node.name),
        .params = params.items,
        .return_type = return_type,
    };
}

fn analyzeObjCProperty(self: *SemanticAnalyzer, node: ASTNode) Error!Type.Property {
    try expectsKind(&.{.ObjCPropertyDecl}, node.kind);

    return .{
        .name = try self.arena.allocator().dupe(u8, node.name),
        .type = try Type.parseFromString(self.arena.allocator(), node.type.?.qualType),
    };
}

fn analyzeObjCProtocol(self: *SemanticAnalyzer, node: ASTNode) Error!?Type {
    try expectsKind(&.{.ObjCProtocolDecl}, node.kind);

    if (node.previousDecl.len > 0) {
        return null;
    }

    var inherits = try std.ArrayList(Type).initCapacity(self.arena.allocator(), node.protocols.len);
    for (node.protocols) |protocol| {
        try expectsKind(&.{.ObjCProtocolDecl}, protocol.kind);
        try inherits.append(.{ .identifier = .{ .name = try self.arena.allocator().dupe(u8, protocol.name) } });
    }

    var children = try std.ArrayList(Type.Protocol.Child).initCapacity(self.arena.allocator(), node.inner.len);
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
        .protocol = .{
            .name = try self.arena.allocator().dupe(u8, node.name),
            .inherits = inherits.items,
            .children = children.items,
        },
    };
}

fn analyzeObjCInterface(self: *SemanticAnalyzer, node: ASTNode) Error!?Type {
    try expectsKind(&.{.ObjCInterfaceDecl}, node.kind);

    if (node.previousDecl.len > 0) {
        return null;
    }

    var protocols = try std.ArrayList(Type).initCapacity(self.arena.allocator(), node.protocols.len);
    for (node.protocols) |p| {
        try expectsKind(&.{.ObjCProtocolDecl}, p.kind);
        try protocols.append(.{ .identifier = .{ .name = try self.arena.allocator().dupe(u8, p.name) } });
    }

    var children = try std.ArrayList(Type.Interface.Child).initCapacity(self.arena.allocator(), node.inner.len);
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

    const super = try self.arena.allocator().create(Type);
    super.* = .{ .identifier = .{ .name = try self.arena.allocator().dupe(u8, node.super.?.name) } };

    return .{
        .interface = .{
            .name = try self.arena.allocator().dupe(u8, node.name),
            .protocols = protocols.items,
            .super = super,
            .children = children.items,
        },
    };
}

fn analyzeTypedef(self: *SemanticAnalyzer, node: ASTNode) Error!?Type {
    try expectsKind(&.{.TypedefDecl}, node.kind);

    if (node.previousDecl.len > 0) {
        return null;
    }

    const inner = try self.arena.allocator().create(Type);
    for (node.inner) |child| {
        switch (child.kind) {
            .BuiltinType => {
                inner.* = try self.analyzeBuiltinType(child);
            },
            .ElaboratedType => {
                inner.* = try self.analyzeElaboratedType(child);
            },
            .PointerType,
            .ConstantArrayType,
            .ObjCBridgeAttr,
            .SwiftNewTypeAttr,
            .AvailabilityAttr,
            .SwiftNameAttr,
            .ObjCObjectPointerType,
            .BlockPointerType,
            .FunctionProtoType,
            .QualType,
            .SwiftBridgedTypedefAttr,
            .FullComment,
            => {},
            else => try unexpectedKind(&.{
                .BuiltinType,
                .ElaboratedType,
                .PointerType,
                .ConstantArrayType,
                .ObjCBridgeAttr,
                .SwiftNewTypeAttr,
                .AvailabilityAttr,
                .SwiftNameAttr,
                .ObjCObjectPointerType,
                .BlockPointerType,
                .FunctionProtoType,
                .QualType,
                .SwiftBridgedTypedefAttr,
                .FullComment,
            }, child.kind),
        }
    }

    return .{
        .typedef = .{
            .name = try self.arena.allocator().dupe(u8, node.name),
            .inner = inner,
        },
    };
}

fn analyzeBuiltinType(self: *SemanticAnalyzer, node: ASTNode) Error!Type {
    try expectsKind(&.{.BuiltinType}, node.kind);
    return Type.parseFromString(self.arena.allocator(), node.type.?.qualType);
}

fn analyzeElaboratedType(self: *SemanticAnalyzer, node: ASTNode) Error!Type {
    try expectsKind(&.{.ElaboratedType}, node.kind);

    var result: ?Type = null;
    for (node.inner) |child| {
        switch (child.kind) {
            .RecordDecl => {
                result = try self.analyzeRecordDecl(child);
            },
            .TypedefType => {
                std.debug.print("{s}\n", .{child.type.?.qualType});
            },
            .EnumDecl => {},
            .EnumType => {},
            .RecordType => {},
            else => try unexpectedKind(&.{
                .RecordDecl,
                .TypedefType,
                .EnumDecl,
                .EnumType,
                .RecordType,
            }, child.kind),
        }
    }
    return result orelse .{ .void = {} };
}

fn analyzeRecordDecl(self: *SemanticAnalyzer, node: ASTNode) Error!Type {
    try expectsKind(&.{.RecordDecl}, node.kind);
    _ = self;

    for (node.inner) |child| {
        std.debug.print("{s}\n", .{child.name});
    }
    return .{
        .void = {},
    };
}

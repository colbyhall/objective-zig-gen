const std = @import("std");
const ArgParser = @import("arg_parser.zig").ArgParser;

const Framework = struct {
    name: []const u8,
    output_file: []const u8,

    dependencies: []const []const u8 = &.{},
    remove_prefix: []const u8 = &.{},
};
const Manifest = []const Framework;

fn parseJsonWithCustomErrorHandling(
    comptime T: type,
    allocator: std.mem.Allocator,
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
        var next_newline = std.mem.indexOf(u8, blob, "\n");
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
            next_newline = std.mem.indexOf(u8, blob, "\n");

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

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};

    const args = try ArgParser.run(gpa.allocator(), &.{});
    switch (args) {
        .parsed => |result| {
            const file = try std.fs.openFileAbsolute(result.path, .{});
            defer file.close();

            const contents = try file.readToEndAlloc(gpa.allocator(), 2 * 1024 * 1024);
            const possible_manifest = try parseJsonWithCustomErrorHandling(
                Manifest,
                gpa.allocator(),
                contents,
                result.path,
            );

            if (possible_manifest == null) {
                return;
            }

            const manifest = possible_manifest.?;
            defer manifest.deinit();
            for (manifest.value) |framework| {
                std.debug.print("{s}\n", .{framework.name});
            }
        },
        .help, .@"error" => |msg| std.debug.print("{s}", .{msg}),
        .exit => {},
    }
}

const JsonASTNode = struct {
    const Self = @This();

    // Describes the location of an AST node within the source code.
    const Loc = struct {
        offset: ?u32 = null, // Offset in the file where the node begins.
        file: []const u8 = &.{}, // Filename containing the node.
        line: ?u32 = null, // Line number where the node begins.
        presumedLine: ?u32 = null, // Line number after preprocessing.
        col: ?u32 = null, // Column number where the node begins.
        tokLen: ?u32 = null, // Length of the token represented by the node.
        includedFrom: ?*Loc = null, // Location of the include directive for this file.
        spellingLoc: ?*Loc = null, // Original location before macro expansion.
        expansionLoc: ?*Loc = null, // Location where the macro was expanded.
        isMacroArgExpansion: bool = false, // True if this is a macro argument expansion.
    };

    // Represents a range within the source code.
    const Range = struct {
        begin: Loc = .{}, // Start location of the range.
        end: Loc = .{}, // End location of the range.
    };

    // Details about the type associated with an AST node.
    const Type = struct {
        desugaredQualType: []const u8 = &.{}, // Fully desugared type as a string.
        qualType: []const u8, // Qualified type as a string.
        typeAliasDeclId: []const u8 = &.{}, // Identifier for a type alias declaration.
    };
    const Kind = enum {
        Unknown,

        // Declarations
        TranslationUnitDecl,
        TypedefDecl,
        ObjCInterfaceDecl,
        RecordDecl,
        FieldDecl,
        FunctionDecl,
        ParmVarDecl,
        VarDecl,
        EnumDecl,
        EnumConstantDecl,
        IndirectFieldDecl,
        ObjCMethodDecl,
        ObjCPropertyDecl,
        EmptyDecl,
        ObjCProtocolDecl,

        // Statements
        CompoundStmt,
        ReturnStmt,
        IfStmt,
        SwitchStmt,
        CaseStmt,
        DefaultStmt,
        BreakStmt,
        DeclStmt,
        GCCAsmStmt,

        // Expressions
        CStyleCastExpr,
        ParenExpr,
        BinaryOperator,
        ImplicitCastExpr,
        DeclRefExpr,
        IntegerLiteral,
        CallExpr,
        MemberExpr,
        ArraySubscriptExpr,
        ConditionalOperator,
        UnaryOperator,
        InitListExpr,
        UnaryExprOrTypeTraitExpr,
        CompoundAssignOperator,
        FloatingLiteral,
        CharacterLiteral,
        ObjCBoolLiteralExpr,
        ConstantExpr,
        ObjCMessageExpr,

        // Types
        BuiltinType,
        PointerType,
        ObjCObjectPointerType,
        ObjCObjectType,
        RecordType,
        ElaboratedType,
        TypedefType,
        ConstantArrayType,
        ParenType,
        FunctionProtoType,
        AttributedType,
        BlockPointerType,
        DecayedType,
        IncompleteArrayType,
        EnumType,
        QualType,
        ObjCInterfaceType,

        // Attributes
        BuiltinAttr,
        NoThrowAttr,
        ConstAttr,
        PackedAttr,
        AvailabilityAttr,
        AlwaysInlineAttr,
        ColdAttr,
        DisableTailCallsAttr,
        PureAttr,
        AvailableOnlyInDefaultEvalMethodAttr,
        ReturnsTwiceAttr,
        AsmLabelAttr,
        FormatAttr,
        DeprecatedAttr,
        FormatArgAttr,
        EnumExtensibilityAttr,
        CFAuditedTransferAttr,
        CFConsumedAttr,
        SwiftNewTypeAttr,
        FlagEnumAttr,
        ObjCBoxableAttr,
        SwiftNameAttr,
        SwiftAttrAttr,
        VisibilityAttr,
        UnavailableAttr,
        ObjCRootClassAttr,
        ObjCIndependentClassAttr,
        NSReturnsRetainedAttr,
        NonNullAttr,
        RestrictAttr,
        CFReturnsRetainedAttr,
        NotTailCalledAttr,
        ObjCCategoryDecl,
        ObjCTypeParamDecl,
        ObjCIvarDecl,
        NSConsumesSelfAttr,
        ObjCReturnsInnerPointerAttr,
        SentinelAttr,
        ObjCExceptionAttr,
        CFReturnsNotRetainedAttr,
        SwiftErrorAttr,
        NSConsumedAttr,
        SwiftAsyncAttr,
        ArcWeakrefUnavailableAttr,
        UnusedAttr,
        AlignedAttr,
        UsedAttr,
        WeakImportAttr,
        SwiftAsyncNameAttr,
        NSErrorDomainAttr,
        SwiftBridgedTypedefAttr,
        AllocSizeAttr,
        AllocAlignAttr,
        NoEscapeAttr,
        MaxFieldAlignmentAttr,
        WarnUnusedResultAttr,
        ObjCBridgeAttr,
        ObjCBridgeMutableAttr,
        ObjCDesignatedInitializerAttr,
        SwiftPrivateAttr,

        // Comments and Documentation
        FullComment,
        ParagraphComment,
        TextComment,
        BlockCommandComment,
        VerbatimLineComment,
        ParamCommandComment,
    };

    // std attributes of the AST node.
    id: []const u8 = &.{}, // Unique identifier for this node.
    kind: Kind = .Unknown, // Kind of AST node, defined by the Kind enum.
    loc: Loc = .{}, // Location information of this node.
    range: Range = .{}, // Source range covered by this node.
    isImplicit: bool = false, // Indicates if the node is implicitly generated by the compiler.
    implicit: bool = false, // Redundant or alternative flag for implicitness.
    isReferenced: bool = false, // True if this node is referenced elsewhere in the AST.
    isUsed: bool = false, // True if this node is used in the AST.

    // Extended properties for detailed AST analysis and code generation.
    name: []const u8 = &.{}, // Name associated with this node, if applicable.
    mangledName: []const u8 = &.{}, // Mangled name for linkage purposes, if applicable.
    returnType: ?Type = null, // Return type of the node if it represents a function.
    fixedUnderlyingType: ?Type = null, // Fixed underlying type information, typically used for typedefs.
    type: ?Type = null, // General type information for variables, functions, etc.
    tagUsed: []const u8 = &.{}, // Specific tag used in the definition of this node.
    completeDefinition: bool = false, // Indicates if this node fully defines a type or entity.
    inner: []const Self = &.{}, // Array of child nodes for hierarchical tree structures.
    super: ?*Self = null, // Pointer to a superclass or parent node in a hierarchy.
    implementation: ?*Self = null, // Implementation detail of this node, distinct from its declaration.
    protocols: []const Self = &.{}, // Protocols or interfaces that this node implements (Objective-C).
    getter: ?*Self = null, // Getter function if this node represents a property.
    decl: ?*Self = null, // Declaration associated with this node.
    ownedTagDecl: ?*Self = null, // Owned tag declaration specific to certain types of nodes.
    size: ?u32 = null, // Size of the data type, if applicable.
    cc: []const u8 = &.{}, // Calling convention used, if this node is a function.
    storageClass: []const u8 = &.{}, // Storage class specifier (e.g., static, extern).
    variadic: bool = false, // True if the function is variadic.
    @"inline": bool = false, // True if the function is declared inline.
    valueCategory: []const u8 = &.{}, // Category of the value (lvalue, rvalue, xvalue).
    castKind: []const u8 = &.{}, // Type of cast, if this node represents a casting operation.
    opcode: []const u8 = &.{}, // Operation code, if this node is an operator.
    referencedDecl: ?*Self = null, // Reference to another declaration, if applicable.
    value: ?std.json.Value = null, // Literal value, if this node is a literal expression.
    isArrow: bool = false, // True if the access is via arrow (->) operator.
    referencedMemberDecl: []const u8 = &.{}, // Referenced member declaration, for member access expressions.
    isPartOfExplicitCast: bool = false, // True if this node is part of an explicit casting operation.
    isBitfield: bool = false, // True if this field is declared as a bitfield.
    previousDecl: []const u8 = &.{}, // Link to the previous declaration in a redeclaration chain.
    inherited: bool = false, // True if this property or method is inherited.
    message: []const u8 = &.{}, // Custom message associated with the node, often used for diagnostics.
    hasElse: bool = false, // True if an 'if' statement has an 'else' branch.
    isPostfix: bool = true, // True if the operator is postfix in unary operations.
    canOverflow: bool = true, // True if the operation can result in an overflow.
    argType: ?Type = null, // Argument type, if this node represents a function argument.
    computeLHSType: ?Type = null, // Computed type of the left-hand side in assignments.
    computeResultType: ?Type = null, // Computed result type of an expression.
    visibility: []const u8 = &.{}, // Visibility specifier (public, private, protected).
    qualifiers: []const u8 = &.{}, // Qualifiers applied to this node (const, volatile).
    control: []const u8 = &.{}, // Control-specific information (e.g., loop control).
    access: []const u8 = &.{}, // Access level for class members (public, private, protected).
    instance: bool = false, // True if this node is an instance of a class or object.
    readonly: bool = false, // True if the property is read-only.
    copy: bool = false, // True if the property should be copied on assignment.
    atomic: bool = false, // True if the property is atomic.
    init: []const u8 = &.{}, // Initializer expression as a string, if applicable.
    nrvo: bool = false, // Indicates Named Return Value Optimization.
    class: bool = false, // True if this node represents a class.
    interface: ?*Self = null, // Interface implemented by this node, if applicable.
    retain: bool = false, // True if the property should be retained.
    selector: []const u8 = &.{}, // Objective-C selector for methods.
    receiverKind: []const u8 = &.{}, // Type of receiver (instance, class).
    nullability: bool = false, // Indicates if the node supports nullability attributes.
    strong: bool = false, // True if the property is strong (retains ownership).
    assign: bool = false, // True if the property is assigned (does not retain ownership).
    unsafe_unretained: bool = false, // True if the property is marked as unsafe_unretained.
    variance: []const u8 = &.{}, // Variance type for generic or template parameters.
    nonatomic: bool = false, // True if the property is nonatomic.
    parentDeclContextId: []const u8 = &.{}, // Identifier of the parent declaration context.
    readwrite: bool = false, // True if the property is read-write.
    null_resettable: bool = false, // True if the property is null-resettable.
    bounded: bool = false, // True if the type is bounded (has specified range or size).
    weak: bool = false, // True if the property is weak (does not own the object).
    text: []const u8 = &.{}, // Textual representation of the node.
    direction: []const u8 = &.{}, // Direction for parameters (in, out, inout).
    param: []const u8 = &.{}, // Parameter name, if this node represents a parameter.
    paramIdx: ?u32 = null, // Index of the parameter in the function's parameter list.
};

const Analyzer = struct {
    const Self = @This();

    allocator: std.mem.Allocator,
    filter: ?[]const u8,

    const Node = struct {
        name: []const u8,
        type: Type,

        const Type = union(enum) {
            protocol: struct {
                inherits: [][]const u8,
                children: []const Node,
            },
            method: struct {
                params: []const Node,
                return_type: []const u8,
            },
            param: struct {
                type: []const u8,
            },
            property: struct {
                type: []const u8,
            },
            interface: struct {
                super: []const u8,
            },
        };
    };

    const Error = error{
        OutOfMemory,
        UnexpectedKind,
    };
    const Options = struct {
        allocator: std.mem.Allocator,

        filter: ?[]const u8 = null,
    };
    fn run(ast: JsonASTNode, options: Self.Options) Error![]const Node {
        if (ast.kind != .TranslationUnitDecl) {
            return error.UnexpectedKind;
        }

        var self = Self{
            .allocator = options.allocator,
            .filter = options.filter,
        };

        var nodes = try std.ArrayList(Node).initCapacity(options.allocator, ast.inner.len);
        for (ast.inner) |node| {
            if (node.isImplicit) {
                continue;
            }

            if (!self.checkIfAllowed(node)) {
                continue;
            }

            std.debug.print("Type: {}, Name: {s}. Has {} children.\n", .{ node.kind, node.name, node.inner.len });
            switch (node.kind) {
                .TypedefDecl => {},
                .ObjCInterfaceDecl => {},
                .ObjCProtocolDecl => {
                    if (try self.analyzeObjCProtocol(node)) |protocol| {
                        try nodes.append(protocol);
                    }
                },
                .ObjCCategoryDecl => {},
                .VarDecl => {},
                .FunctionDecl => {},
                .RecordDecl => {},
                else => {
                    std.debug.print("Unimplemented kind {} of {s}.\n", .{ node.kind, node.name });
                    return error.UnexpectedKind;
                },
            }
        }

        // for (protocols.items) |p| {
        //     std.debug.print("Protocol '{s}'\n", .{p.name});

        //     std.debug.print("\tInherits:\n", .{});
        //     for (p.inherits) |i| {
        //         std.debug.print("\t\t{s}\n", .{i});
        //     }

        //     if (p.methods.len > 0) {
        //         std.debug.print("\tMethods:\n", .{});
        //         for (p.methods) |m| {
        //             std.debug.print("\t\t{s}\n", .{m.name});

        //             if (m.params.len > 0) {
        //                 std.debug.print("\t\t\tParams:\n", .{});
        //                 for (m.params) |param| {
        //                     std.debug.print("\t\t\t\t{s}: {s}\n", .{ param.name, param.type });
        //                 }
        //             }

        //             std.debug.print("\t\t\tReturn Type: '{s}'\n", .{m.return_type});
        //         }
        //     }

        //     if (p.properties.len > 0) {
        //         std.debug.print("\tProperties:\n", .{});
        //         for (p.properties) |prop| {
        //             std.debug.print("\t\t{s}: {s}\n", .{ prop.name, prop.type });
        //         }
        //     }

        //     std.debug.print("\n", .{});
        // }

        return nodes.items;
    }

    fn passesFilter(self: Self, path: []const u8) bool {
        if (self.filter) |f| {
            return std.mem.containsAtLeast(u8, path, 1, f);
        }

        return true;
    }

    fn checkIfAllowed(self: Self, node: JsonASTNode) bool {
        if (node.loc.includedFrom) |loc| {
            if (!self.passesFilter(loc.file)) {
                return false;
            }
        }
        if (node.loc.spellingLoc) |loc| {
            if (!self.passesFilter(loc.file)) {
                return false;
            }
        }

        return true;
    }

    fn analyzeObjCMethod(self: Self, node: JsonASTNode) Error!Node {
        std.debug.assert(node.kind == .ObjCMethodDecl);

        var params = try std.ArrayList(Node).initCapacity(self.allocator, node.inner.len);
        for (node.inner) |child| {
            switch (child.kind) {
                .ParmVarDecl => {},
                .AvailabilityAttr => {
                    continue;
                },
                else => {
                    std.debug.print("Exepected .ParmVarDecl but found {}\n", .{child.kind});
                    return error.UnexpectedKind;
                },
            }

            try params.append(.{
                .name = child.name,
                .type = .{
                    .param = .{
                        .type = child.type.?.qualType,
                    },
                },
            });
        }

        return .{
            .name = node.name,
            .type = .{
                .method = .{
                    .params = params.items,
                    .return_type = node.returnType.?.qualType,
                },
            },
        };
    }

    fn analyzeObjCProperty(self: Self, node: JsonASTNode) Error!Node {
        std.debug.assert(node.kind == .ObjCPropertyDecl);

        _ = self;
        return .{
            .name = node.name,
            .type = .{
                .property = .{
                    .type = node.type.?.qualType,
                },
            },
        };
    }

    fn analyzeObjCProtocol(self: Self, node: JsonASTNode) Error!?Node {
        std.debug.assert(node.kind == .ObjCProtocolDecl);

        if (node.inner.len == 0) {
            return null;
        }

        const inherits: [][]const u8 = if (node.protocols.len > 0) try self.allocator.alloc([]const u8, node.protocols.len) else &.{};
        for (node.protocols, 0..) |protocol, index| {
            if (protocol.kind != .ObjCProtocolDecl) {
                return error.UnexpectedKind;
            }

            inherits[index] = protocol.name;
        }

        var nodes = try std.ArrayList(Node).initCapacity(self.allocator, node.inner.len);
        for (node.inner) |child| {
            switch (child.kind) {
                .ObjCMethodDecl => {
                    try nodes.append(try self.analyzeObjCMethod(child));
                },
                .ObjCPropertyDecl => {
                    try nodes.append(try self.analyzeObjCProperty(child));
                },
                .VisibilityAttr => {},
                .AvailabilityAttr => {},
                else => return error.UnexpectedKind,
            }
        }

        return .{
            .name = node.name,
            .type = .{
                .protocol = .{
                    .inherits = inherits,
                    .children = nodes.items,
                },
            },
        };
    }
};

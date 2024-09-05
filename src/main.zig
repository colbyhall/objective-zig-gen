const std = @import("std");

const ArgParser = struct {
    const Self = @This();

    mode: enum { files, options },
    files: std.ArrayList([]const u8),
    filter: ?[]const u8,
    include_std: ?bool,
    output_arguments: ?bool,
    output_json: ?bool,

    const Option = struct {
        const Usage = union(enum) {
            text: []const u8,
            func: *const fn (std.mem.Allocator) void,
            none,
        };

        name: []const u8,
        description: []const u8,
        usage: Usage = .none,
        param: enum { none, optional, required } = .none,
        exec: *const fn (self: *Self, param: []const u8) bool,
    };
    const options = [_]Option{
        .{
            .name = "help",
            .description = "Shows the usage of the generator. Accepts an optional parameter to display the usage of a specific option.",
            .usage = Option.Usage{ .func = helpUsage },
            .param = .optional,
            .exec = helpExec,
        },
        .{
            .name = "filter",
            .description = "Filters types based on what path or file they are in.",
            .usage = Option.Usage{ .text = "Usage: -filter=<path>\n" },
            .param = .required,
            .exec = filterExec,
        },
        .{
            .name = "include_std",
            .description = "Includes std types into the generated output.",
            .usage = Option.Usage{ .text = "Usage: -include_std\n" },
            .exec = includeStdExec,
        },
        .{
            .name = "debug_args",
            .description = "Outputs the arguments passed to zig c++",
            .exec = debugArgsExec,
        },
        .{
            .name = "pipe",
            .description = "Writes output from zig c++ to stdout.",
            .exec = pipeExec,
        },
    };

    fn printOptionUsage(self: *Self, option: []const u8) void {
        for (options) |value| {
            if (!std.mem.eql(u8, option, value.name)) {
                continue;
            }

            // We've found the option but it has no usage message.
            if (value.usage == .none) {
                std.debug.print("TODO: Write usage for {s}\n", .{option});
                return;
            }

            const output = switch (value.usage) {
                .text => |text| text,
                .func => |f| {
                    f(self.files.allocator);
                    return;
                },
                else => unreachable,
            };
            std.debug.print("{s}", .{output});
            return;
        }

        std.debug.print("Found no option '{s}'. See -help for an exhaustive list of options.\n", .{option});
    }

    fn run(allocator: std.mem.Allocator) !?Self {
        var self = Self{
            .mode = .files,
            .files = std.ArrayList([]const u8).init(allocator),
            .filter = null,
            .include_std = null,
            .output_arguments = null,
            .output_json = null,
        };

        var cwd = std.fs.cwd();

        var args = std.process.args();
        defer args.deinit();

        std.debug.assert(args.skip()); // Skip past the executale in the argument

        var index: i32 = 0;
        while (args.next()) |arg| {
            defer index += 1;

            // Update the mode of the argument parser
            if (std.mem.startsWith(u8, arg, "-") and self.mode == .files) {
                // NOTE: Special case for -help as its the only option that can be used without a file
                if (self.files.items.len == 0 and !std.mem.startsWith(u8, arg, "-help")) {
                    std.debug.print("Started listing options without listing any files. See usage in -help.\n", .{});
                    return null;
                }
                self.mode = .options;
            }
            if (!std.mem.startsWith(u8, arg, "-") and self.mode == .options) {
                std.debug.print("Argument {} ({s}) did not start with the option prefix. See usage in -help.\n", .{ index + 1, arg });
                return null;
            }

            switch (self.mode) {
                .files => {
                    var path: []const u8 = undefined;
                    if (std.fs.path.isAbsolute(arg)) {
                        // Catch the invalid file error early so zig cc doesn't have to
                        std.fs.accessAbsolute(arg, .{}) catch |err| {
                            std.debug.print("Failed to access file at path \'{s}\' due to {s}.\n", .{ path, @errorName(err) });
                            return null;
                        };

                        const new_path = try allocator.alloc(u8, arg.len);
                        std.mem.copyForwards(u8, new_path, arg);
                        path = new_path;
                    } else {
                        path = cwd.realpathAlloc(allocator, arg) catch |err| {
                            std.debug.print("Failed to access file at path \'{s}\' due to {s}.\n", .{ arg, @errorName(err) });
                            return null;
                        };
                    }

                    try self.files.append(path);
                },
                .options => {
                    // Parse the argument into its parts. Skip the - in the args. This is checked earlier when switching the parser mode.
                    var found = false;
                    const name = arg[1..];
                    for (options) |option| {
                        if (!std.mem.eql(u8, option.name, name)) {
                            continue;
                        }

                        var args_copy = args;
                        var param = args_copy.next();
                        if (param) |p| {
                            if (std.mem.startsWith(u8, p, "-")) {
                                param = null;
                            }
                        }

                        if (param != null and option.param == .none) {
                            std.debug.print("Option '{s}' in argument {} does not require a parameter but one was provided.\n", .{ arg, index + 1 });
                            self.printOptionUsage(name);
                            return null;
                        }
                        if ((param == null or param.?.len == 0) and option.param == .required) {
                            std.debug.print("Option '{s}' in argument {} was not supplied a parameter but one is required.\n", .{ arg, index + 1 });
                            self.printOptionUsage(name);
                            return null;
                        }

                        // Consume the peek arg that is the param
                        if (param != null) {
                            _ = args.next();
                        }

                        if (!option.exec(&self, param orelse "")) {
                            return null;
                        } else {
                            found = true;
                            break;
                        }
                    }

                    if (!found) {
                        std.debug.print("Found no option '{s}'. See usage using -help.\n", .{name});
                    }
                },
            }
        }

        return self;
    }

    fn helpUsage(allocator: std.mem.Allocator) void {
        const exe_path = std.fs.selfExePathAlloc(allocator) catch unreachable;
        std.debug.print("Usage: {s} [files] [options]\n\n", .{exe_path});

        std.debug.print("[options]\n", .{});
        for (options) |option| {
            std.debug.print("  -{s: <16} {s}\n", .{ option.name, option.description });
        }
    }

    fn helpExec(self: *Self, param: []const u8) bool {
        if (param.len > 0) {
            self.printOptionUsage(param);
        } else {
            helpUsage(self.files.allocator);
        }

        return false;
    }

    fn filterExec(self: *Self, param: []const u8) bool {
        self.filter = param;

        return true;
    }

    fn includeStdExec(self: *Self, param: []const u8) bool {
        _ = param;
        self.include_std = true;
        return true;
    }

    fn debugArgsExec(self: *Self, param: []const u8) bool {
        _ = param;
        self.output_arguments = true;
        return true;
    }

    fn pipeExec(self: *Self, param: []const u8) bool {
        _ = param;
        self.output_json = true;
        return true;
    }
};
const Options = struct {
    allocator: std.mem.Allocator,

    filter: ?[]const u8,
    include_std: bool,
    output_arguments: bool,
    output_json: bool,
};
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};

    const parsed_args = try ArgParser.run(gpa.allocator()) orelse return;

    var pool: std.Thread.Pool = undefined;
    try pool.init(.{
        .allocator = gpa.allocator(),
    });
    defer pool.deinit();

    const options = Options{
        .allocator = gpa.allocator(),
        .filter = parsed_args.filter,
        .include_std = parsed_args.include_std orelse false,
        .output_arguments = parsed_args.output_arguments orelse false,
        .output_json = parsed_args.output_json orelse false,
    };

    var parse_files_work = std.Thread.WaitGroup{};
    for (parsed_args.files.items) |path| {
        pool.spawnWg(&parse_files_work, processFile, .{ path, options });
    }
    pool.waitAndWork(&parse_files_work);

    std.debug.print("Finished\n", .{});
}

fn processFile(path: []const u8, options: Options) void {
    processFileInner(path, options) catch unreachable;
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

fn processFileInner(path: []const u8, options: Options) !void {
    var args = std.ArrayList([]const u8).init(options.allocator);
    try args.appendSlice(&[_][]const u8{
        "zig",
        "c++",
        "-Xclang",
        "-ast-dump=json",
        "-fsyntax-only",
    });

    if (!options.include_std) {
        try args.append("-nostdinc");
    }

    try args.append("-x");
    try args.append("objective-c");
    try args.append(path);

    if (options.output_arguments) {
        std.Progress.lockStdErr();
        defer std.Progress.unlockStdErr();

        for (args.items) |arg| {
            try std.io.getStdErr().writer().print("{s} ", .{arg});
        }

        try std.io.getStdErr().writer().print("\n", .{});
    }
    const result = try std.process.Child.run(.{
        .allocator = options.allocator,
        .argv = args.items,
        .max_output_bytes = 1024 * 1024 * 1024,
    });

    if (result.stderr.len > 0) {
        // std.debug.print("{s}\n", .{result.stderr});
    }

    std.debug.print("\n* {s} produced {d:.2} of json\n\n", .{
        path,
        std.fmt.fmtIntSizeBin(result.stdout.len),
    });

    if (options.output_json) {
        try std.io.getStdOut().writer().writeAll(result.stdout);
    }

    var scanner = std.json.Scanner.initCompleteInput(options.allocator, result.stdout);
    var diagnostics = std.json.Diagnostics{};
    scanner.enableDiagnostics(&diagnostics);

    const ast = std.json.parseFromTokenSource(JsonASTNode, options.allocator, &scanner, .{}) catch |err| {
        const lines_above_error_to_show = 10;
        const lines_below_error_to_show = 10;

        const offset = diagnostics.getByteOffset();
        var previous_newline: usize = offset;
        var previous_newline_count: u32 = 0;
        while (previous_newline > 0) {
            previous_newline -= 1;

            if (result.stdout[previous_newline] == '\n') {
                previous_newline_count += 1;
                if (previous_newline_count == lines_above_error_to_show) {
                    break;
                }
            }
        }
        var blob = if (previous_newline > 0) result.stdout[previous_newline + 1 ..] else result.stdout[previous_newline..];

        std.Progress.lockStdErr();
        defer std.Progress.unlockStdErr();

        const stderr = std.io.getStdErr();

        try stderr.writer().print(
            "Failed to parse '{s}' ast json into ASTNode.\nError at line {} column {}.\n\n",
            .{ path, diagnostics.getLine(), diagnostics.getColumn() },
        );

        var next_newline = std.mem.indexOf(u8, blob, "\n") orelse blob.len;
        var line = blob[0..next_newline];
        blob = blob[next_newline + 1 ..];
        var line_number = diagnostics.getLine() - previous_newline_count + 1;
        while (line_number <= diagnostics.getLine() + lines_below_error_to_show) {
            if (line_number == diagnostics.getLine()) {
                try stderr.writer().print(" > {: >7} {s}\n", .{ line_number, line });
                const padding = 10;
                for (0..padding + diagnostics.getColumn()) |_| {
                    try stderr.writer().print(" ", .{});
                }
                try stderr.writer().print("^ {s}\n", .{@errorName(err)});
            } else {
                try stderr.writer().print("   {: >7} {s}\n", .{ line_number, line });
            }

            const current_newline = next_newline;
            next_newline = std.mem.indexOf(u8, blob, "\n") orelse blob.len;
            if (current_newline == next_newline) {
                // break;
            }

            line_number += 1;
            line = blob[0..next_newline];
            blob = blob[next_newline + 1 ..];
        }

        try stderr.writer().print("\n", .{});

        return;
    };
    _ = try Analyzer.run(ast.value, .{
        .allocator = options.allocator,
        .filter = options.filter,
    });
    defer ast.deinit();
}

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

const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;

const ArgParser = @import("arg_parser.zig").ArgParser;
const Analyzer = @import("Analyzer.zig");
const AstNode = @import("AstNode.zig");
const Renderer = @import("Renderer.zig");

pub const Framework = struct {
    name: []const u8,
    output_file: []const u8,

    dependencies: []const []const u8 = &.{},
    remove_prefix: []const u8 = &.{},
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
    // Remove the newline from the output
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

            // First thing to do is to validate that any framework dependency is listed in the manifest
            var frameworks = std.StringHashMap(Framework).init(allocator);
            defer frameworks.deinit();
            for (manifest.value) |framework| {
                try frameworks.put(framework.name, framework);
            }
            for (manifest.value) |framework| {
                for (framework.dependencies) |dependency| {
                    if (!frameworks.contains(dependency)) {
                        std.debug.print("Framework '{s}' has '{s}' listed as a dependency but it isn't in the manifest.\n", .{ framework.name, dependency });
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

            var pool: std.Thread.Pool = undefined;
            try pool.init(.{
                .allocator = allocator,
            });
            defer pool.deinit();

            const analyzers = try allocator.alloc(Analyzer, manifest.value.len);
            var frameworks_semantic_analysis = std.Thread.WaitGroup{};
            for (manifest.value, 0..) |framework, index| {
                const path_to_header = try std.fmt.allocPrint(
                    allocator,
                    "{s}.framework/Headers/{s}.h",
                    .{
                        framework.name,
                        framework.name,
                    },
                );
                // Create the path based on the framework name.
                const path = try std.fs.path.join(allocator, &.{
                    frameworks_path,
                    path_to_header,
                });

                // Check to see if the header of the framework exist.
                std.fs.accessAbsolute(path, .{}) catch |err| {
                    std.debug.print(
                        "Failed to access framework '{s}' header at path '{s}' due to {}",
                        .{
                            framework.name,
                            path,
                            err,
                        },
                    );
                    return;
                };

                pool.spawnWg(
                    &frameworks_semantic_analysis,
                    parseAndAnalyzeFramework,
                    .{
                        .{
                            .allocator = allocator,
                            .framework = framework,
                            .path_to_header = path,
                            .result = &analyzers[index],
                        },
                    },
                );
            }
            pool.waitAndWork(&frameworks_semantic_analysis);

            const cwd = std.fs.cwd();

            // TODO: Add a way to specify the output option.
            const output_path = "output";
            cwd.makeDir(output_path) catch |err| {
                if (err != std.fs.Dir.MakeError.PathAlreadyExists) {
                    return err;
                }
            };

            var output_dir = try cwd.openDir(output_path, .{});
            defer output_dir.close();

            // Copy about the objc runtime to the output directory.
            {
                var objc_file = try output_dir.createFile("objc.zig", .{});
                defer objc_file.close();
                _ = try objc_file.write(@embedFile("objc.zig"));
            }

            var frameworks_render = std.Thread.WaitGroup{};
            for (analyzers) |*analyzer| {
                pool.spawnWg(&frameworks_render, renderFramework, .{
                    .{
                        .allocator = allocator,
                        .output_dir = &output_dir,
                        .frameworks = &frameworks,
                        .analyzer = analyzer,
                    },
                });
            }
            pool.waitAndWork(&frameworks_render);
        },
        .help, .@"error" => |msg| std.debug.print("{s}", .{msg}),
        .exit => {},
    }
}

fn renderFramework(options: Renderer.Options) void {
    Renderer.run(options) catch unreachable;
}

const FrameworkParseAndAnalyzeInfo = struct {
    allocator: Allocator,
    framework: Framework,
    path_to_header: []const u8,
    result: *Analyzer,
};
fn parseAndAnalyzeFramework(info: FrameworkParseAndAnalyzeInfo) void {
    parseAndAnalyzeFrameworkInner(info) catch unreachable;
}
fn parseAndAnalyzeFrameworkInner(info: FrameworkParseAndAnalyzeInfo) !void {
    const args = &[_][]const u8{
        "zig",
        "c++",
        "-fsyntax-only",
        "-Xclang",
        "-ast-dump=json",
        "-nostdinc",
        "-x",
        "objective-c",
        info.path_to_header,
    };

    const ast_json = try std.process.Child.run(.{
        .allocator = info.allocator,
        .argv = args,
        .max_output_bytes = 1024 * 1024 * 1024,
    });
    defer info.allocator.free(ast_json.stdout);
    defer info.allocator.free(ast_json.stderr);

    // HACK: Can't figure out how to not get clang to produce this file.
    std.fs.cwd().deleteFile("a.out") catch {};

    std.debug.print(
        "{s} produced {d:.2} of json.\n",
        .{
            info.framework.name,
            std.fmt.fmtIntSizeBin(ast_json.stdout.len),
        },
    );

    const ast = (try parseJsonWithCustomErrorHandling(
        AstNode,
        info.allocator,
        ast_json.stdout,
        info.path_to_header,
    )).?;
    defer ast.deinit();

    info.result.* = try Analyzer.run(ast.value, .{
        .allocator = info.allocator,
        .framework = info.framework.name,
    });
}

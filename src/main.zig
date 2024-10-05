const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const Progress = std.Progress;
const fs = std.fs;

const root = @import("root.zig");
const Framework = root.Framework;
const Manifest = root.Manifest;
const Renderer = @import("Renderer.zig");

const parser = @import("parser.zig");
const Registry = parser.Registry;
const Type = parser.Type;

const ArgParser = @import("arg_parser.zig").ArgParser;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const args = try ArgParser.run(allocator, &.{});
    switch (args) {
        .parsed => |result| {
            // Read in the manifest file and deserialize it.
            const possible_frameworks = try root.parseJsonWithCustomErrorHandling(
                []const Framework,
                allocator,
                result.path,
            );
            if (possible_frameworks == null) {
                return;
            }

            const frameworks = possible_frameworks.?;
            defer frameworks.deinit();

            if (frameworks.value.len == 0) {
                std.log.err("Found no frameworks in manifest '{s}'.", .{result.path});
                return;
            }

            // Convert the parsed manifest into a map
            var manifest = Manifest.init(allocator);
            defer manifest.deinit();
            for (frameworks.value) |*framework| {
                try manifest.put(framework.name, framework);
            }

            // Check to see if any framework depends ona  framework not listed in the manifest
            for (frameworks.value) |framework| {
                for (framework.dependencies) |dependency| {
                    if (!manifest.contains(dependency)) {
                        std.log.err("Framework '{s}' has '{s}' listed as a dependency but it isn't in the manifest.", .{ framework.name, dependency });
                        return;
                    }
                }
            }

            // Acquire the absolute path to the xcode sdk
            const sdk_path = try root.acquireSDKPath(allocator);
            defer allocator.free(sdk_path);

            // Generate a path to the frameworks directory
            const frameworks_path = try fs.path.join(
                allocator,
                &.{
                    sdk_path,
                    "System/Library/Frameworks/",
                },
            );
            defer allocator.free(frameworks_path);

            // Start the thread pool now that we know we have work to do
            var pool: std.Thread.Pool = undefined;
            try pool.init(.{ .allocator = allocator });
            defer pool.deinit();

            // This progress is for the runtime of the program. Its children are parsing and then analyzing.
            const base_progress = Progress.start(.{ .estimated_total_items = 2 });
            defer base_progress.end();

            // Parse the frameworks listed in the manifest. Parse each framework on its own thread.
            const results = try allocator.alloc(Registry, frameworks.value.len);
            {
                const parse_progress = base_progress.start("Parsing Frameworks", frameworks.value.len);
                defer parse_progress.end();

                var parse_framework_work = std.Thread.WaitGroup{};
                defer pool.waitAndWork(&parse_framework_work);

                for (frameworks.value, 0..) |*framework, index| {
                    pool.spawnWg(
                        &parse_framework_work,
                        parser.parse,
                        .{
                            .{
                                .gpa = allocator,
                                .arena = allocator,
                                .sdk_path = sdk_path,
                                .framework = framework,
                                .result = &results[index],
                                .progress = parse_progress,
                            },
                        },
                    );
                }
            }

            // Merge the results from the parsing. We have to do this because some frameworks only forward declare
            // the interfaces or protocols that they might need.
            for (results) |*a| {
                for (results) |*b| {
                    // Skip merging if we're gonna merge with ourself
                    if (a == b) {
                        continue;
                    }

                    // Merge typedefs
                    {
                        var iter = b.typedefs.iterator();
                        while (iter.next()) |it| {
                            if (!a.typedefs.contains(it.key_ptr.*)) {
                                try a.typedefs.put(it.key_ptr.*, it.value_ptr.*);
                            }
                        }
                    }

                    // Merge structs
                    {
                        var iter = b.structs.iterator();
                        while (iter.next()) |it| {
                            if (!a.structs.contains(it.key_ptr.*)) {
                                try a.structs.put(it.key_ptr.*, it.value_ptr.*);
                            }
                        }
                    }

                    // Merge unions
                    {
                        var iter = b.unions.iterator();
                        while (iter.next()) |it| {
                            if (!a.unions.contains(it.key_ptr.*)) {
                                try a.unions.put(it.key_ptr.*, it.value_ptr.*);
                            }
                        }
                    }

                    // Merge enums
                    {
                        var iter = b.enums.iterator();
                        while (iter.next()) |it| {
                            if (!a.enums.contains(it.key_ptr.*)) {
                                try a.enums.put(it.key_ptr.*, it.value_ptr.*);
                            }
                        }
                    }

                    // Merge interfaces
                    {
                        var iter = b.interfaces.iterator();
                        while (iter.next()) |it| {
                            if (!a.interfaces.contains(it.key_ptr.*)) {
                                try a.interfaces.put(it.key_ptr.*, it.value_ptr.*);
                            }
                        }
                    }

                    // Merge protocols
                    {
                        var iter = b.protocols.iterator();
                        while (iter.next()) |it| {
                            if (!a.protocols.contains(it.key_ptr.*)) {
                                try a.protocols.put(it.key_ptr.*, it.value_ptr.*);
                            }
                        }
                    }
                }
            }

            // TODO: Add the ability to change the output directory as a command line argument.
            const output_path = "output";

            // Try to create the output directory. Ignore any error about the directory already existing.
            fs.cwd().makeDir(output_path) catch |err| {
                if (err != fs.Dir.MakeError.PathAlreadyExists) {
                    return err;
                }
            };

            var output = try fs.cwd().openDir(output_path, .{});
            defer output.close();

            // Copy about the objc runtime to the output directory.
            {
                var objc_file = try output.createFile("objc.zig", .{});
                defer objc_file.close();
                _ = try objc_file.write(@embedFile("objc.zig"));
            }

            // Render every framework we have results for. Each rendering job happens in its own thread.
            {
                const render_progress = base_progress.start("Rendering Frameworks", frameworks.value.len);
                defer render_progress.end();

                var render_framework_work = std.Thread.WaitGroup{};
                defer pool.waitAndWork(&render_framework_work);

                for (results) |*r| {
                    pool.spawnWg(&render_framework_work, Renderer.run, .{
                        .{
                            .allocator = allocator,
                            .output = output,
                            .manifest = manifest,
                            .registry = r,
                            .progress = render_progress,
                        },
                    });
                }
            }

            // Generate the root file that includes the runtime and all the frameworks.
            {
                var root_file = try output.createFile("root.zig", .{});
                defer root_file.close();

                const writer = root_file.writer();
                _ = try writer.write("// THIS FILE IS AUTOGENERATED. MODIFICATIONS WILL NOT BE MAINTAINED.\n\n");
                _ = try writer.write("pub usingnamespace @import(\"objc.zig\"); // Export the objective c runtime to root. \n");

                for (frameworks.value) |f| {
                    try writer.print("pub const {s} = @import(\"{s}.zig\");\n", .{
                        f.output_file,
                        f.output_file,
                    });
                }
            }

            // Run the zig fmt on the output path so everything aligns with the zig coding standards.
            _ = try std.process.Child.run(.{
                .allocator = allocator,
                .argv = &.{
                    "zig",
                    "fmt",
                    output_path,
                },
            });
        },
        .help, .@"error" => |msg| std.debug.print("{s}", .{msg}),
        .exit => {},
    }
}

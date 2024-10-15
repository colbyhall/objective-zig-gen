const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const Progress = std.Progress;
const fs = std.fs;

const builtin = @import("builtin");

const root = @import("root.zig");
const Framework = root.Framework;
const Manifest = root.Manifest;
const Renderer = @import("Renderer.zig");

const Parser = @import("Parser.zig");
const Registry = Parser.Registry;
const Type = Parser.Type;

const ArgParser = @import("arg_parser.zig").ArgParser;

pub fn main() !void {
    var gpa_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa = gpa_allocator.allocator();

    // Parse the commandline arguments to gather how we should execute the generator
    const args = try ArgParser.run(
        gpa,
        &.{
            .{
                .name = "no_render",
                .alias = "nr",
                .description = "Only parse the files. Used for debugging.",
            },
            .{
                .name = "output",
                .alias = "o",
                .description = "Sets the output directory of the generated files.",
                .param = .string,
            },
            .{
                .name = "no_fmt",
                .alias = "nf",
                .description = "Prevents zig fmt from running on the generated output.",
            },
            .{
                .name = "single_threaded",
                .alias = "st",
                .description = "Parses and renders frameworks serially as given in the framework. Typically used for debugging.",
            },
        },
    );
    switch (args) {
        .parsed => |result| {
            // Read in the manifest file and deserialize it.
            const possible_frameworks = try root.parseJsonWithCustomErrorHandling(
                []const Framework,
                gpa,
                result.path,
            );
            if (possible_frameworks == null) {
                std.log.err("Found no frameworks in manifest '{s}'.", .{result.path});
                return;
            }

            const frameworks = possible_frameworks.?;
            defer frameworks.deinit();

            if (frameworks.value.len == 0) {
                std.log.err("Found no frameworks in manifest '{s}'.", .{result.path});
                return;
            }

            // Convert the parsed manifest into a map
            var manifest = Manifest.init(gpa);
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
            const sdk_path = try root.acquireSDKPath(gpa);
            defer gpa.free(sdk_path);

            // Generate a path to the frameworks directory
            const frameworks_path = try fs.path.join(
                gpa,
                &.{
                    sdk_path,
                    "System/Library/Frameworks/",
                },
            );
            defer gpa.free(frameworks_path);

            // Start the thread pool if the program is not in single threaded mode. We wait to do this
            // now as we know there is work to do.
            var pool: std.Thread.Pool = undefined;
            const single_threaded = result.options.contains("single_threaded") or builtin.single_threaded;
            if (!single_threaded) {
                try pool.init(.{ .allocator = gpa });
            }
            defer if (!single_threaded) {
                pool.deinit();
            };

            // This progress is for the runtime of the program. Its children are parsing and then analyzing.
            const base_progress = Progress.start(.{ .estimated_total_items = 2 });
            defer base_progress.end();

            // Parse the frameworks listed in the manifest. Parse each framework on its own thread.
            const results = try gpa.alloc(Registry, frameworks.value.len);
            {
                const parse_progress = base_progress.start("Parsing Frameworks", frameworks.value.len);
                defer parse_progress.end();

                // If single threaded parse the frameworks as given in the manifest serially.
                if (single_threaded) {
                    for (frameworks.value, 0..) |*framework, index| {
                        Parser.parse(.{
                            .gpa = gpa,
                            .arena = gpa,
                            .sdk_path = sdk_path,
                            .framework = framework,
                            .result = &results[index],
                            .progress = parse_progress,
                        });
                    }
                }
                // If multi threaded, parse the frameworks in any given order asynchronously and wait for the
                // work to be completed on this thread.
                else {
                    var parse_framework_work = std.Thread.WaitGroup{};
                    defer pool.waitAndWork(&parse_framework_work);
                    for (frameworks.value, 0..) |*framework, index| {
                        pool.spawnWg(
                            &parse_framework_work,
                            Parser.parse,
                            .{
                                .{
                                    .gpa = gpa,
                                    .arena = gpa,
                                    .sdk_path = sdk_path,
                                    .framework = framework,
                                    .result = &results[index],
                                    .progress = parse_progress,
                                },
                            },
                        );
                    }
                }
            }

            // If the no_render option is enabled stop executing here.
            if (result.options.contains("no_render")) {
                return;
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

            // Gather the output path from the arg parser results.
            var output_path: []const u8 = "output";
            if (result.options.get("output")) |value| {
                output_path = value.string;
            }

            // Try to create the output directory. Ignore any error about the directory already existing.
            fs.cwd().makeDir(output_path) catch |err| {
                if (err != fs.Dir.MakeError.PathAlreadyExists) {
                    return err;
                }
            };

            // Open the output directory to be passed to the render jobs. Every framework creates the file they render out to in their job.
            var output = try fs.cwd().openDir(output_path, .{});
            defer output.close();

            // Copy about the objc runtime to the output directory.
            {
                var objc_file = try output.createFile("objc.zig", .{});
                defer objc_file.close();
                _ = try objc_file.write(@embedFile("objc.zig"));
            }

            {
                const render_progress = base_progress.start("Rendering Frameworks", frameworks.value.len);
                defer render_progress.end();

                // If single threaded, render frameworks serially in order as provided in the manifest.
                if (single_threaded) {
                    for (results) |*r| {
                        Renderer.run(.{
                            .allocator = gpa,
                            .output = output,
                            .manifest = manifest,
                            .registry = r,
                            .progress = render_progress,
                        });
                    }
                }
                // If multi threaded, render frameworks in any order asychronously and wait for the jobs
                // to complete on this thread.
                else {
                    var render_framework_work = std.Thread.WaitGroup{};
                    defer pool.waitAndWork(&render_framework_work);
                    for (results) |*r| {
                        pool.spawnWg(&render_framework_work, Renderer.run, .{
                            .{
                                .allocator = gpa,
                                .output = output,
                                .manifest = manifest,
                                .registry = r,
                                .progress = render_progress,
                            },
                        });
                    }
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

            // If the user has specified no fmt to run then halt.
            if (result.options.contains("no_fmt")) {
                return;
            }

            // Run the zig fmt on the output path so everything aligns with the zig coding style.
            _ = try std.process.Child.run(.{
                .allocator = gpa,
                .argv = &.{
                    "zig",
                    "fmt",
                    output_path,
                },
            });
        },
        // Print help or error messages straight to stderr and then return.
        .help, .@"error" => |msg| std.debug.print("{s}", .{msg}),
    }
}

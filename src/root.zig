const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const json = std.json;
const fs = std.fs;
const fmt = std.fmt;

pub const Framework = struct {
    name: []const u8,
    output_file: []const u8,
    dependencies: []const []const u8 = &.{},
    remove_prefix: []const u8 = &.{},
    header_override: ?[]const u8 = null,
};
pub const Manifest = std.StringHashMap(*const Framework);

pub fn parseJsonWithCustomErrorHandling(
    comptime T: type,
    allocator: Allocator,
    path: []const u8,
) !?json.Parsed(T) {
    const file = try fs.openFileAbsolute(path, .{});
    defer file.close();

    const content = try file.readToEndAlloc(allocator, 2 * 1024 * 1024);
    defer allocator.free(content);

    var scanner = json.Scanner.initCompleteInput(allocator, content);
    var diagnostics = json.Diagnostics{};
    scanner.enableDiagnostics(&diagnostics);

    const result = json.parseFromTokenSource(T, allocator, &scanner, .{
        .allocate = .alloc_always,
    }) catch |err| {
        const lines_above_error_to_show = 10;
        const lines_below_error_to_show = 10;

        const offset = diagnostics.getByteOffset();

        // Reverse iterate through the complete_input at offset looking for newlines
        var previous_line_start: usize = offset;
        var previous_newline_count: u32 = 0;
        while (previous_line_start > 0) {
            previous_line_start -= 1;

            if (content[previous_line_start] == '\n') {
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
        var blob = content[previous_line_start..];

        // We're going to write out multiple lines of input over multiple lines of code so go ahead
        // and lock stderr and print out to it manually as std.debug.print locks internally.
        std.Progress.lockStdErr();
        defer std.Progress.unlockStdErr();
        const stderr = std.io.getStdErr();

        try stderr.writer().print(
            "Failed to parse json file '{s}' into '{s}'.\nError at line {} column {}.\n\n",
            .{
                path,
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

pub fn acquireSDKPath(allocator: Allocator) ![:0]const u8 {
    const args = [_][]const u8{
        "xcrun",
        "--show-sdk-path",
    };
    const result = try std.process.Child.run(.{
        .allocator = allocator,
        .argv = &args,
    });
    defer allocator.free(result.stdout);
    defer allocator.free(result.stderr);

    return try fmt.allocPrintZ(allocator, "{s}", .{result.stdout[0 .. result.stdout.len - 1]});
}

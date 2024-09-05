const std = @import("std");
const Allocator = std.mem.Allocator;

pub const ArgParser = union(enum) {
    const Self = @This();

    pub const Option = struct {
        const Param = enum { none, string };
        const ParamValue = union(Param) {
            none: void,
            string: []const u8,
        };

        name: []const u8,
        description: []const u8,
        param: Param = .none,
    };
    pub const Parsed = struct {
        path: []const u8,
        options: std.StringHashMap(Option.ParamValue),
    };

    parsed: Parsed,
    help: []const u8,
    @"error": []const u8,
    exit,

    pub fn run(allocator: Allocator, comptime options: []const Option) !Self {
        var args = std.process.args();
        defer args.deinit();
        var cwd = std.fs.cwd();

        std.debug.assert(args.skip()); // Skip past the executable in the argument

        var file: []const u8 = undefined;
        var option_map = std.StringHashMap(Option.ParamValue).init(allocator);

        var index: i32 = 1;
        outer: while (args.next()) |arg| {
            defer index += 1;

            // The help option or the spec json file must be the first argument
            if (index == 1) {
                if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "-help")) {
                    return .{
                        .help = "TODO",
                    };
                }

                if (!std.mem.endsWith(u8, arg, ".json")) {
                    return .{
                        .@"error" = "File provided is not a json file.\n",
                    };
                }

                if (std.fs.path.isAbsolute(arg)) {
                    // Catch the invalid file error early so zig cc doesn't have to
                    std.fs.accessAbsolute(arg, .{}) catch |err| {
                        return .{
                            .@"error" = try std.fmt.allocPrint(
                                allocator,
                                "Failed to access file at path \'{s}\' due to {s}.\n",
                                .{ arg, @errorName(err) },
                            ),
                        };
                    };

                    const new_path = try allocator.alloc(u8, arg.len);
                    std.mem.copyForwards(u8, new_path, arg);
                    file = new_path;
                } else {
                    file = cwd.realpathAlloc(allocator, arg) catch |err| {
                        return .{
                            .@"error" = try std.fmt.allocPrint(
                                allocator,
                                "Failed to access file at path \'{s}\' due to {s}.\n",
                                .{ arg, @errorName(err) },
                            ),
                        };
                    };
                }

                continue;
            }

            if (!std.mem.startsWith(u8, arg, "-")) {
                return .{
                    .@"error" = try std.fmt.allocPrint(
                        allocator,
                        "Argument does not start with '-'. See -h for usage.\n",
                        .{},
                    ),
                };
            }

            // Parse the arguments and match them to the comptime provided data.
            const name = arg[1..];
            for (options) |option| {
                // Check to see if 'name' is this option
                if (!std.mem.eql(u8, name, option, .name)) {
                    continue;
                }

                if (option.param == .none) {
                    try option_map.put(option.name, .{.none});
                    continue :outer;
                }

                // Peek ahead to see if there is a possible param being passed to this argument.
                var peek_args = args;
                var param = peek_args.next();
                if (param) |p| {
                    if (std.mem.startsWith(u8, p, "-")) {
                        param = null;
                    }
                }

                // Consume the peek arg that is the param
                if (param != null) {
                    _ = args.next();
                }

                switch (option.param) {
                    .string => {
                        const result = try allocator.alloc(u8, param.len);
                        std.mem.copyForward(u8, result, param);
                        try option_map.put(
                            option.name,
                            .{
                                .string = result,
                            },
                        );
                    },
                    else => unreachable,
                }
            }

            return .{
                .@"error" = try std.fmt.allocPrint(
                    allocator,
                    "Unknown command '{s}'. See usage using -h.\n",
                    .{name},
                ),
            };
        }

        return .{
            .parsed = .{
                .path = file,
                .options = option_map,
            },
        };
    }
};

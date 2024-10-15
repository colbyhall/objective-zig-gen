const std = @import("std");
const fs = std.fs;
const mem = std.mem;
const Allocator = mem.Allocator;
const ascii = std.ascii;
const fmt = std.fmt;
const meta = std.meta;

const root = @import("root.zig");
const Manifest = root.Manifest;

const Parser = @import("Parser.zig");
const Registry = Parser.Registry;
const Type = Parser.Type;

gpa: Allocator,
writer: std.io.BufferedWriter(4096, std.fs.File.Writer),
frameworks: Manifest,
registry: *Registry,

const keyword_remap = std.StaticStringMap([]const u8).initComptime(.{
    .{ "error", "@\"error\"" },
    .{ "align", "@\"align\"" },
    .{ "resume", "@\"resume\"" },
    .{ "suspend", "@\"suspend\"" },
    .{ "type", "@\"type\"" },
    .{ "test", "@\"test\"" },
    .{ "opaque", "@\"opaque\"" },
    .{ "null", "@\"null\"" },
    .{ "bool", "@\"bool\"" },
});

pub fn init(options: struct {
    allocator: Allocator,
    writer: fs.File.Writer,
    manifest: Manifest,
    registry: *Registry,
}) @This() {
    return .{
        .gpa = options.allocator,
        .writer = std.io.bufferedWriter(options.writer),
        .frameworks = options.manifest,
        .registry = options.registry,
    };
}

pub fn render(self: *@This(), comptime format: []const u8, args: anytype) void {
    self.writer.writer().print(format, args) catch {
        @panic("Failed to write to output file.");
    };
}

pub fn renderFrameworkDecl(self: *@This(), named: *Type.Decleration) bool {
    switch (named.origin) {
        .framework => |f| if (mem.eql(u8, f, self.registry.owner.name)) {
            return self.renderNamedDecl(named);
        },
        else => {},
    }
    return false;
}

fn generateCamelCase(self: *@This(), name: []const u8) []const u8 {
    const result = self.gpa.alloc(u8, name.len) catch {
        @panic("OOM");
    };
    var index: u32 = 1;
    while (index < name.len) : (index += 1) {
        const it = name[index];
        if (ascii.isLower(it)) {
            index -= 1;
            break;
        }
    }
    if (index == 0) {
        result[0] = ascii.toLower(name[0]);
        mem.copyForwards(u8, result[1..], name[1..]);
    } else {
        for (0..index) |i| {
            result[i] = ascii.toLower(name[i]);
        }
        mem.copyForwards(u8, result[index..], name[index..]);
    }
    return result;
}

fn generateFunctionName(self: *@This(), in_name: []const u8) []const u8 {
    var name = in_name;
    if (keyword_remap.get(name)) |remap| {
        return remap;
    } else {
        const colon_count = mem.count(u8, name, ":");
        if (colon_count > 0) {
            var index: usize = 0;
            const result = self.gpa.alloc(u8, name.len - colon_count) catch {
                @panic("OOM");
            };

            var colon: u32 = 0;
            while (colon < colon_count) : (colon += 1) {
                const next_colon = mem.indexOf(u8, name, ":").?;
                const current = name[0..next_colon];
                if (current.len > 0) {
                    if (colon > 0) {
                        // Capitalize the first letter of the word to match zig coding style.
                        result[index] = ascii.toUpper(current[0]);
                        mem.copyForwards(u8, result[index + 1 .. index + next_colon], current[1..]);
                    } else {
                        const sub = self.generateCamelCase(current);
                        mem.copyForwards(u8, result[index .. index + next_colon], sub);
                    }
                }

                index += next_colon;
                name = name[next_colon + 1 ..];
            }

            return result;
        } else {
            return self.generateCamelCase(name);
        }
    }
}

fn renderNamedName(self: *@This(), named: *Type.Decleration, options: struct {
    ignore_framework: bool = false,
    ignore_parents: bool = false,
}) void {
    switch (named.origin) {
        .runtime => self.render("objc.{s}", .{named.name}),
        .framework => |f| {
            if (self.frameworks.get(f)) |framework| {
                var found = false;
                if (mem.eql(u8, f, self.registry.owner.name)) {
                    found = true;
                } else {
                    for (self.registry.owner.dependencies) |d| {
                        if (mem.eql(u8, d, f)) {
                            found = true;
                            break;
                        }
                    }
                }

                if (!found) {
                    if (self.frameworks.contains(f)) {
                        std.log.err(
                            "{s} depends on framework {s} but is not listed as a dependency to {s} in the manifest.\n",
                            .{
                                self.registry.owner.name,
                                f,
                                self.registry.owner.name,
                            },
                        );
                    }
                    // Let the user know that the framework is also not in the manifest to ease debugging.
                    else {
                        std.log.err(
                            "{s} depends on framework {s} but is not listed as a dependency to {s} in the manifest. {s} is not listed in the manifest at all.\n",
                            .{
                                self.registry.owner.name,
                                f,
                                self.registry.owner.name,
                                f,
                            },
                        );
                    }
                    unreachable;
                }

                var result = named.name;
                if (named.parent) |parent| {
                    if (mem.startsWith(u8, named.name, parent.name)) {
                        result = result[parent.name.len..];
                    } else {
                        if (framework.remove_prefix.len > 0 and !mem.eql(u8, named.name, framework.remove_prefix)) {
                            if (mem.startsWith(u8, named.name, framework.remove_prefix)) {
                                result = result[framework.remove_prefix.len..];
                            }
                        }
                    }
                } else {
                    if (framework.remove_prefix.len > 0 and !mem.eql(u8, named.name, framework.remove_prefix)) {
                        if (mem.startsWith(u8, named.name, framework.remove_prefix)) {
                            result = result[framework.remove_prefix.len..];
                        }
                    }
                }
                if (result.len > 0 and ascii.isDigit(result[0])) {
                    result = named.name;
                }

                if (!options.ignore_framework and framework != self.registry.owner) {
                    self.render("{s}.", .{framework.output_file});
                }

                if (!options.ignore_parents) {
                    var hierarchy = std.ArrayList(*Type.Decleration).init(self.gpa);
                    var root_type = named;
                    while (root_type.parent != null) {
                        hierarchy.insert(0, root_type.parent.?) catch {
                            @panic("OOM");
                        };
                        root_type = root_type.parent.?;
                    }

                    for (hierarchy.items) |i| {
                        self.renderNamedName(i, .{ .ignore_parents = true });
                        self.render(".", .{});
                    }
                }

                if (keyword_remap.get(result)) |remap| {
                    result = remap;
                }
                self.render("{s}", .{result});
            } else {
                std.log.err(
                    "{s} depends on framework {s} but is not listed as a dependency to {s} in the manifest. {s} is not listed in the manifest at all.\n",
                    .{
                        self.registry.owner.name,
                        f,
                        self.registry.owner.name,
                        f,
                    },
                );
                unreachable;
            }
        },
    }
}

const MethodCache = struct {
    render_count: std.StringHashMap(u32),
    rendered: std.StringHashMap([]const u8),
};

fn renderMethods(self: *@This(), cache: *MethodCache, named: *Type.Decleration) void {
    switch (named.tag) {
        .protocol => |p| {
            for (p.inherits.items) |super| {
                var s = super;
                switch (super.tag) {
                    .identifier => {
                        s = self.registry.lookup(.protocol, super.name).?;
                    },
                    .protocol => {},
                    else => {
                        unreachable;
                    },
                }

                self.renderMethods(cache, s);
            }

            for (p.methods.items) |m| {
                if (cache.rendered.contains(m.asNamed().name)) {
                    continue;
                }

                const generated_name = self.generateFunctionName(m.asNamed().name);
                const e = cache.render_count.getOrPutValue(generated_name, 1) catch {
                    @panic("OOM");
                };
                defer e.value_ptr.* += 1;

                if (e.value_ptr.* > 1) {
                    const number_name = fmt.allocPrint(self.gpa, "{s}{}", .{ generated_name, e.value_ptr.* }) catch {
                        @panic("OOM");
                    };
                    self.renderMethodDecl(number_name, m);
                    cache.rendered.put(m.asNamed().name, number_name) catch {
                        @panic("OOM");
                    };
                } else {
                    self.renderMethodDecl(generated_name, m);
                    cache.rendered.put(m.asNamed().name, generated_name) catch {
                        @panic("OOM");
                    };
                }
                self.render("\n", .{});
            }
        },
        .interface => |*i| {
            // Include protocol methods because can't include generic type params. SuperClass generic info is erased
            // which makes it impossible to include super methods. The user can still get the super.
            for (i.protocols.items) |super| {
                var s = super;
                switch (super.tag) {
                    .identifier => {
                        s = self.registry.lookup(.protocol, super.name).?;
                    },
                    .protocol => {},
                    else => {
                        unreachable;
                    },
                }

                self.renderMethods(cache, s);
            }

            for (i.methods.items) |m| {
                if (cache.rendered.contains(m.asNamed().name)) {
                    continue;
                }

                const generated_name = self.generateFunctionName(m.asNamed().name);
                const e = cache.render_count.getOrPutValue(generated_name, 1) catch {
                    @panic("OOM");
                };
                defer e.value_ptr.* += 1;

                if (e.value_ptr.* > 1) {
                    const number_name = fmt.allocPrint(self.gpa, "{s}{}", .{ generated_name, e.value_ptr.* }) catch {
                        @panic("OOM");
                    };
                    self.renderMethodDecl(number_name, m);
                    cache.rendered.put(m.asNamed().name, number_name) catch {
                        @panic("OOM");
                    };
                } else {
                    self.renderMethodDecl(generated_name, m);
                    cache.rendered.put(m.asNamed().name, generated_name) catch {
                        @panic("OOM");
                    };
                }
                self.render("\n", .{});
            }
        },
        else => unreachable,
    }
}

fn gatherMethodGenericParams(self: *@This(), out_params: *std.StringArrayHashMap(void), param: *Type) void {
    switch (param.*) {
        .decleration => |n| switch (n.tag) {
            .identifier => |i| {
                for (i.type_parameters.items) |p| {
                    self.gatherMethodGenericParams(out_params, p);
                }
            },
            .type_param => {
                out_params.put(n.name, {}) catch {
                    @panic("OOM");
                };
            },
            else => {},
        },
        else => {},
    }
}

fn renderMethodDecl(self: *@This(), name: []const u8, method: *Type.Decleration.Method) void {
    var generic_params = std.StringArrayHashMap(void).init(self.gpa);
    for (method.params.items) |p| {
        self.gatherMethodGenericParams(&generic_params, p.type);
    }

    self.render("pub fn ", .{});
    _ = self.writer.write(name) catch {
        unreachable;
    };
    self.render("(", .{});

    if (method.kind == .instance) {
        self.render("_self: *@This()", .{});

        if (method.params.items.len > 0) {
            self.render(", ", .{});
        }
    }

    // Render the generic params first
    {
        const parent = method.asNamed().parent.?;

        var iter = generic_params.iterator();
        outer: while (iter.next()) |arg| {
            // Skip the generic params that are part of the interface decl.
            switch (parent.tag) {
                .interface => |i| {
                    for (i.type_parameters.items) |p| {
                        if (generic_params.contains(p)) {
                            continue :outer;
                        }
                    }
                },
                else => {},
            }
            self.render("comptime {s}: type, ", .{arg.key_ptr.*});
        }
    }

    for (method.params.items, 0..) |param, index| {
        self.render("_{s}: ", .{param.asNamed().name});
        self.renderTypeAsIdentifier(param.type);
        if (method.params.items.len > 3 or index < method.params.items.len - 1) {
            self.render(", ", .{});
        }
    }

    self.render(") ", .{});
    self.renderTypeAsIdentifier(method.result.?);
    self.render(" {{\n", .{});
    self.render("return objc.msgSend(", .{});
    if (method.kind == .class) {
        self.render("Internal.class(), ", .{});
    } else {
        self.render("_self, ", .{});
    }
    self.render(
        "\"{s}\", ",
        .{method.asNamed().name},
    );
    self.renderTypeAsIdentifier(method.result.?);
    self.render(", .{{", .{});
    for (method.params.items, 0..) |param, index| {
        self.render("_{s}", .{param.asNamed().name});
        if (method.params.items.len > 3 or index < method.params.items.len - 1) {
            self.render(", ", .{});
        }
    }
    self.render("}});\n    }}\n", .{});
}

fn renderChildrenDecl(self: *@This(), children: []const *Type.Decleration) void {
    var rendered = std.StringHashMap(void).init(self.gpa);
    for (children) |c| {
        if (meta.activeTag(c.origin) == .runtime) continue;

        switch (c.tag) {
            .@"struct", .@"union", .@"enum", .interface, .protocol, .typedef => {
                if (!rendered.contains(c.name)) {
                    if (self.renderNamedDecl(c)) {
                        rendered.put(c.name, {}) catch {
                            @panic("OOM");
                        };
                    }
                }
            },
            else => {},
        }
    }
}

fn renderNamedDecl(self: *@This(), named: *Type.Decleration) bool {
    switch (named.tag) {
        .@"struct" => |s| {
            self.render("pub const ", .{});
            self.renderNamedName(named, .{ .ignore_parents = true });
            self.render(" = extern struct {{", .{});
            self.renderChildrenDecl(named.children.items);

            if (s.fields.count() > 0) {
                self.render("\n", .{});
                self.renderFieldDecls(s.fields);
            }

            self.render("}};\n\n", .{});

            return true;
        },
        .@"enum" => |s| {
            self.render("pub const ", .{});
            self.renderNamedName(named, .{ .ignore_parents = true });
            self.render(" = ", .{});
            self.renderTypeAsIdentifier(s.backing);
            self.render(";\n", .{});

            if (s.values.items.len > 0) {
                var values = std.StringHashMap(i64).init(self.gpa);
                for (s.values.items) |v| {
                    var name = v.name;
                    if (name.len > 0 and name[0] == 'k') {
                        name = name[1..];
                    }
                    var last = name;

                    // Remove the enum type prefix from the entires. This assumes the enumerations are in
                    // PascalCase. By assuming the case we can remove words instead of raw characters.
                    //
                    // NOTE: This has issues with the anon enums and any enumerations that arent in PascalCase
                    var index: u32 = 0;
                    while (index < named.name.len and name.len > 0) : (index += 1) {
                        const a = name[0];
                        const b = named.name[index];

                        if (a != b) {
                            if (ascii.isUpper(a)) {
                                last = name;
                            }
                            break;
                        }
                        if (ascii.isUpper(b)) {
                            last = name;
                        }
                        name = name[1..];

                        if (index == named.name.len - 1) {
                            last = name;
                        }
                    }

                    if (last.len > 0 and !values.contains(last)) {
                        values.put(last, v.value) catch {
                            @panic("OOM");
                        };

                        self.render("pub const ", .{});
                        self.renderNamedName(named, .{ .ignore_parents = true });
                        self.render("_{s}: ", .{last});
                        self.renderTypeAsIdentifier(s.backing);
                        self.render(" = {};\n", .{v.value});
                    }
                }
            }

            self.render("\n", .{});

            return true;
        },
        .@"union" => |s| {
            self.render("pub const ", .{});
            self.renderNamedName(named, .{ .ignore_parents = true });
            self.render(" = extern union {{", .{});

            self.renderChildrenDecl(named.children.items);

            if (s.fields.count() > 0) {
                self.render("\n", .{});
                self.renderFieldDecls(s.fields);
            }

            self.render("}};\n\n", .{});

            return true;
        },
        .typedef => |t| {
            // Skip C types that have typedefs to add them to the C global namespace.
            switch (t.child.?.*) {
                .decleration => |n| {
                    if (mem.eql(u8, n.name, named.name)) {
                        return false;
                    }
                },
                else => {},
            }
            self.render("pub const ", .{});
            self.renderNamedName(named, .{ .ignore_parents = true });
            self.render(" = ", .{});
            self.renderTypeAsIdentifier(t.child.?);
            self.render(";\n\n", .{});

            return true;
        },
        .protocol => |p| {
            self.render("/// https://developer.apple.com/documentation/{s}/{s}?language=objc\n", .{ named.origin.framework, named.name });
            self.render("pub const ", .{});
            self.renderNamedName(named, .{ .ignore_parents = true });
            self.render(" = opaque {{", .{});
            self.render("\n", .{});

            self.renderChildrenDecl(named.children.items);

            self.render(
                "    pub const Internal = objc.ExternProtocol(@This(), &.{{",
                .{},
            );

            for (p.inherits.items, 0..) |n, index| {
                self.renderTypeAsIdentifier(n.asType());

                if (index < p.inherits.items.len - 1) {
                    self.render(", ", .{});
                }
            }
            self.render("}});\n", .{});

            self.render("    pub const as = Internal.as;\n", .{});
            self.render("    pub const retain = Internal.retain;\n", .{});
            self.render("    pub const release = Internal.release;\n", .{});
            self.render("    pub const autorelease = Internal.autorelease;\n", .{});

            self.render("\n", .{});

            var rendered = std.StringHashMap([]const u8).init(self.gpa);
            rendered.put("retain", "retain") catch {
                @panic("OOM");
            };
            rendered.put("release", "release") catch {
                @panic("OOM");
            };
            rendered.put("autorelease", "autorelease") catch {
                @panic("OOM");
            };

            var cache = MethodCache{
                .render_count = std.StringHashMap(u32).init(self.gpa),
                .rendered = rendered,
            };
            self.renderMethods(&cache, named);

            self.render("}};\n\n", .{});

            return true;
        },
        .interface => |i| {
            self.render("/// https://developer.apple.com/documentation/{s}/{s}?language=objc\n", .{ named.origin.framework, named.name });

            const is_generic = i.type_parameters.items.len > 0;
            if (is_generic) {
                self.render("pub fn ", .{});
                self.renderNamedName(named, .{ .ignore_parents = true });
                self.render("(", .{});
                for (i.type_parameters.items, 0..) |p, index| {
                    self.render("comptime ", .{});
                    self.renderNameAvoidKeywords(p);
                    self.render(": type", .{});
                    if (index < i.type_parameters.items.len - 1) {
                        self.render(", ", .{});
                    }
                }
                self.render(") type {{\n", .{});
                for (i.type_parameters.items, 0..) |p, index| {
                    self.render("const unused{} = ", .{index});
                    self.renderNameAvoidKeywords(p);
                    self.render(";\n_ = unused{}; // Prevent unused parameter warning!!!\n", .{index});
                }
                self.render("return struct {{", .{});
            } else {
                self.render("pub const ", .{});
                self.renderNamedName(named, .{ .ignore_parents = true });
                self.render(" = opaque {{", .{});
            }
            self.render("\n", .{});

            self.renderChildrenDecl(named.children.items);

            self.render(
                "    pub const Internal = objc.ExternClass(\"{s}\", @This(), ",
                .{named.name},
            );

            if (i.super) |super| {
                switch (super.origin) {
                    .runtime => self.render("objc.NSObject", .{}),
                    else => self.renderTypeAsIdentifier(super.asType()),
                }
            } else {
                self.render("objc.NSObject", .{});
            }

            self.render(", &.{{", .{});
            for (i.protocols.items, 0..) |inh, index| {
                self.renderTypeAsIdentifier(inh.asType());

                if (index < i.protocols.items.len - 1) {
                    self.render(", ", .{});
                }
            }

            self.render("}});\n", .{});

            self.render("    pub const as = Internal.as;\n", .{});
            self.render("    pub const retain = Internal.retain;\n", .{});
            self.render("    pub const release = Internal.release;\n", .{});
            self.render("    pub const autorelease = Internal.autorelease;\n", .{});
            self.render("    pub const new = Internal.new;\n", .{});
            self.render("    pub const alloc = Internal.alloc;\n", .{});
            self.render("\n", .{});

            var rendered = std.StringHashMap([]const u8).init(self.gpa);
            rendered.put("retain", "retain") catch {
                @panic("OOM");
            };
            rendered.put("release", "release") catch {
                @panic("OOM");
            };
            rendered.put("autorelease", "autorelease") catch {
                @panic("OOM");
            };
            rendered.put("new", "new") catch {
                @panic("OOM");
            };
            rendered.put("alloc", "alloc") catch {
                @panic("OOM");
            };

            var cache = MethodCache{
                .render_count = std.StringHashMap(u32).init(self.gpa),
                .rendered = rendered,
            };
            self.renderMethods(&cache, named);

            self.render("}};\n", .{});
            if (is_generic) {
                self.render("}}\n", .{});
            }
            self.render("\n", .{});

            return true;
        },
        .method => |m| {
            self.render("pub fn ", .{});
            const out = self.generateFunctionName(named.name);
            _ = self.writer.write(out) catch {
                unreachable;
            };
            self.render("(_self: *@This()", .{});

            if (m.params.items.len > 0) {
                self.render(", ", .{});
                for (m.params.items, 0..) |param, index| {
                    self.render("_{s}: ", .{param.asNamed().name});
                    self.renderTypeAsIdentifier(param.type);
                    if (m.params.items.len > 3 or index < m.params.items.len - 1) {
                        self.render(", ", .{});
                    }
                }
            }

            self.render(") ", .{});
            self.renderTypeAsIdentifier(m.result.?);
            self.render(" {{\n", .{});
            self.render(
                "        return objc.msgSend(_self, \"{s}\", ",
                .{named.name},
            );
            self.renderTypeAsIdentifier(m.result.?);
            self.render(", .{{", .{});
            for (m.params.items, 0..) |param, index| {
                self.render("_{s}", .{param.asNamed().name});
                if (m.params.items.len > 3 or index < m.params.items.len - 1) {
                    self.render(", ", .{});
                }
            }
            self.render("}});\n    }}\n", .{});

            return true;
        },
        .function => |f| {
            var name = named.name;
            switch (named.origin) {
                .framework => |ff| {
                    if (self.frameworks.get(ff)) |framework| {
                        if (framework.remove_prefix.len > 0 and mem.startsWith(u8, name, framework.remove_prefix)) {
                            name = name[framework.remove_prefix.len..];
                        }
                    }
                },
                else => {},
            }

            const name_changed = !mem.eql(u8, name, named.name);
            if (!name_changed) {
                self.render("pub ", .{});
            }
            self.render("extern \"{s}\" fn {s}(", .{ named.origin.framework, named.name });

            for (f.params.items, 0..) |param, index| {
                if (param.asNamed().name.len > 0) {
                    self.renderNameAvoidKeywords(param.asNamed().name);
                    self.render(": ", .{});
                }

                self.renderTypeAsIdentifier(param.type);
                if (f.params.items.len > 3 or index < f.params.items.len - 1) {
                    self.render(", ", .{});
                }
            }
            self.render(") callconv(.C) ", .{});
            self.renderTypeAsIdentifier(f.result.?);
            self.render(";\n", .{});

            if (name_changed) {
                self.render("pub const ", .{});

                if (keyword_remap.get(name)) |remap| {
                    self.render("{s}", .{remap});
                } else {
                    var index: u32 = 1;
                    while (index < name.len) : (index += 1) {
                        const it = name[index];
                        if (ascii.isLower(it)) {
                            index -= 1;
                            break;
                        }
                    }
                    if (index == 0) {
                        _ = self.writer.writer().writeByte(ascii.toLower(name[0])) catch {
                            unreachable;
                        };
                        _ = self.writer.writer().write(name[1..]) catch {
                            unreachable;
                        };
                    } else {
                        for (0..index) |i| {
                            _ = self.writer.writer().writeByte(ascii.toLower(name[i])) catch {
                                unreachable;
                            };
                        }
                        _ = self.writer.writer().write(name[index..]) catch {
                            unreachable;
                        };
                    }
                }

                self.render(" = {s};\n", .{named.name});
            }

            self.render("\n", .{});

            return true;
        },
        .identifier,
        => {},
        else => unreachable,
    }

    return false;
}

fn renderNameAvoidKeywords(self: *@This(), name: []const u8) void {
    // Change the name of fields that conflict with zig keywords
    var result = name;
    if (keyword_remap.get(name)) |remap| {
        result = remap;
    }
    self.render("{s}", .{result});
}

fn renderFieldDecls(self: *@This(), fields: std.StringArrayHashMap(*Type.Decleration.Field)) void {
    var iter = fields.iterator();
    while (iter.next()) |pair| {
        const f = pair.value_ptr.*;

        self.render("    ", .{});
        self.renderNameAvoidKeywords(pair.key_ptr.*);
        self.render(": ", .{});
        self.renderTypeAsIdentifier(f.type.?);
        self.render(",\n", .{});
    }
}

fn renderTypeAsIdentifier(self: *@This(), @"type": *Type) void {
    switch (@"type".*) {
        .objc_id => self.render("?objc.Id", .{}),
        .objc_class => self.render("objc.Class", .{}),
        .objc_sel => self.render("objc.Selector", .{}),
        .base_protocol => self.render("*objc.Protocol", .{}),
        .instancetype => self.render("*@This()", .{}),
        .void => self.render("void", .{}),
        .int => |i| {
            const num_bits: u32 = @as(u32, i.size) * 8;
            if (i.signed > 0) {
                self.render("i{}", .{num_bits});
            } else {
                self.render("u{}", .{num_bits});
            }
        },
        .char => {
            self.render("c_char", .{});
        },
        .bool => {
            self.render("bool", .{});
        },
        .float => |f| {
            const num_bits: u32 = @as(u32, f.size) * 8;
            self.render("f{}", .{num_bits});
        },
        .pointer, .block_pointer => |p| {
            if (p.@"const" > 0 and meta.activeTag(p.underlying.*) == .char) {
                self.render("[*:0]const u8", .{});
            } else {
                if (p.nullable > 0) {
                    self.render("?", .{});
                }
                self.render("*", .{});
                if (p.@"const" > 0 or meta.activeTag(p.underlying.*) == .function_proto) {
                    self.render("const ", .{});
                }
                if (meta.activeTag(p.underlying.*) == .void) {
                    self.render("anyopaque", .{});
                } else {
                    self.renderTypeAsIdentifier(p.underlying);
                }
            }
        },
        .array => |a| {
            self.render("[{}] ", .{a.length});
            self.renderTypeAsIdentifier(a.element);
        },
        .function_proto => |f| {
            self.render("fn(", .{});
            for (f.params, 0..) |p, index| {
                self.renderTypeAsIdentifier(p);
                if (f.params.len > 3 or index < f.params.len - 1) {
                    self.render(", ", .{});
                }
            }
            self.render(") callconv(.C) ", .{});
            self.renderTypeAsIdentifier(f.result);
        },
        .decleration => |*n| switch (n.tag) {
            .function => {
                var name = n.name;

                switch (n.origin) {
                    .framework => |f| {
                        if (self.frameworks.get(f)) |framework| {
                            if (framework.remove_prefix.len > 0) {
                                name = name[framework.remove_prefix.len..];
                            }
                        }
                    },
                    else => {},
                }

                const out = self.generateFunctionName(name);
                _ = self.writer.write(out) catch {
                    unreachable;
                };
            },
            .identifier => |i| {
                if (self.registry.lookupElaborated(n.name)) |e| {
                    self.renderTypeAsIdentifier(e.asType());
                } else {
                    std.log.err("Failed to find {s} in framework {s}.", .{ n.name, self.registry.owner.name });
                    self.render("anyopaque", .{});
                }

                // Print out the type parameters
                if (i.type_parameters.items.len > 0) {
                    self.render("(", .{});
                    for (i.type_parameters.items, 0..) |arg, index| {
                        self.renderTypeAsIdentifier(arg);

                        if (i.type_parameters.items.len > 3 or index < i.type_parameters.items.len - 1) {
                            self.render(", ", .{});
                        }
                    }
                    self.render(")", .{});
                }
            },
            else => {
                self.renderNamedName(n, .{});
            },
        },
        else => {
            std.log.err("Unhandled type kind {}.", .{meta.activeTag(@"type".*)});
            unreachable;
        },
    }
}

pub fn run(args: struct {
    allocator: Allocator,
    output: fs.Dir,
    manifest: Manifest,
    registry: *Registry,
    progress: std.Progress.Node,
}) void {
    const progress = args.progress.start(args.registry.owner.name, args.registry.order.items.len);
    defer progress.end();

    const path = fmt.allocPrint(args.allocator, "{s}.zig", .{args.registry.owner.output_file}) catch {
        @panic("OOM");
    };
    defer args.allocator.free(path);

    var output_file = args.output.createFile(path, .{}) catch {
        @panic("Failed to create output file");
    };
    defer output_file.close();

    var self = @This().init(.{
        .allocator = args.allocator,
        .writer = output_file.writer(),
        .manifest = args.manifest,
        .registry = args.registry,
    });

    self.render("// THIS FILE IS AUTOGENERATED. MODIFICATIONS WILL NOT BE MAINTAINED.\n\n", .{});
    self.render("const std = @import(\"std\");\n", .{});
    self.render("const objc = @import(\"objc.zig\"); // Objective-C Runtime in zig.\n", .{});

    // Render out imports for framework dependencies
    for (args.registry.owner.dependencies) |d| {
        const dep = args.manifest.get(d).?;
        self.render("const {s} = @import(\"{s}.zig\"); // Framework dependency {s}.\n", .{ dep.output_file, dep.output_file, dep.name });
    }

    // Add an empty line between imports and type declerations
    self.render("\n", .{});

    var declared = std.StringHashMap(void).init(args.allocator);
    for (self.registry.order.items) |o| {
        if (!declared.contains(o.name)) {
            const ref = self.registry.lookup(o.tag, o.name);
            if (self.renderFrameworkDecl(ref.?)) {
                progress.completeOne();
                declared.put(o.name, {}) catch {
                    @panic("OOM");
                };
            }
        }
    }

    self.writer.flush() catch {
        unreachable;
    };
}

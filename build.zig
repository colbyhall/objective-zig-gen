const std = @import("std");
const LazyPath = std.Build.LazyPath;

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const exe = b.addExecutable(.{
        .name = "objective-zig-gen",
        .root_source_file = b.path("src/main.zig"),
        .target = target,
        .optimize = optimize,
    });

    // TODO: Find a better way to link to libclang. This only works with llvm installed through homebrew.
    exe.addIncludePath(LazyPath{ .cwd_relative = "/opt/homebrew/opt/llvm/include/" });
    exe.addLibraryPath(LazyPath{ .cwd_relative = "/opt/homebrew/opt/llvm/lib/" });
    exe.linkSystemLibrary("clang");

    b.installArtifact(exe);
}

const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Get the zig compiler dependency and its exposed compiler_api module
    const zig_dep = b.dependency("zig_compiler", .{});
    const compiler_mod = zig_dep.module("compiler_api");

    // Build clr as a shared library that can be dynamically loaded by the zig compiler
    const lib = b.addLibrary(.{
        .name = "clr",
        .linkage = .dynamic,
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/clr.zig"),
            .target = target,
            .optimize = optimize,
            .pic = true,
            .imports = &.{
                .{ .name = "compiler", .module = compiler_mod },
            },
        }),
        .zig_lib_dir = b.path("zig/lib"),
        .use_llvm = true,
    });

    b.installArtifact(lib);

    // Unit tests
    const lib_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/clr.zig"),
            .target = target,
            .optimize = optimize,
            .imports = &.{
                .{ .name = "compiler", .module = compiler_mod },
            },
        }),
        .zig_lib_dir = b.path("zig/lib"),
    });

    const run_lib_tests = b.addRunArtifact(lib_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_lib_tests.step);
}

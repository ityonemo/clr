const std = @import("std");
pub const FnInterpreter = @import("lib.zig").FnInterpreter;

// =============================================================================
// Core types used across multiple modules.
// This file exists to break circular dependencies between tag.zig and Refinements.zig.
// =============================================================================

/// Global ID - unique identifier for entities, used for provenance tracking across function boundaries.
pub const Gid = u32;

/// Sentinel value for uninitialized GIDs. Any access to a refinement with this GID
/// indicates a bug where the refinement wasn't properly added via appendEntity.
pub const INVALID_GID: Gid = std.math.maxInt(Gid);

/// Generic metadata struct for source locations, used by various analytes.
pub const Meta = struct {
    function: []const u8,
    file: []const u8,
    line: u32,
    column: ?u32 = null,

    pub fn print(self: @This(), writer: anytype, comptime prefix: []const u8, prefix_args: anytype) !void {
        var buf: [1024]u8 = undefined;
        // Handle empty function names (e.g., for globals) - omit "in <function>" entirely
        const func_part = if (self.function.len > 0) self.function else "";
        if (self.column) |column| {
            if (func_part.len > 0) {
                const fmt = comptime prefix ++ "{s} ({s}:{d}:{d})\n";
                const args = prefix_args ++ .{ func_part, self.file, self.line, column };
                const msg = std.fmt.bufPrint(&buf, fmt, args) catch return error.FormatError;
                try writer.writeAll(msg);
            } else {
                const fmt = comptime prefix ++ "({s}:{d}:{d})\n";
                const args = prefix_args ++ .{ self.file, self.line, column };
                const msg = std.fmt.bufPrint(&buf, fmt, args) catch return error.FormatError;
                try writer.writeAll(msg);
            }
        } else {
            if (func_part.len > 0) {
                const fmt = comptime prefix ++ "{s} ({s}:{d})\n";
                const args = prefix_args ++ .{ func_part, self.file, self.line };
                const msg = std.fmt.bufPrint(&buf, fmt, args) catch return error.FormatError;
                try writer.writeAll(msg);
            } else {
                const fmt = comptime prefix ++ "({s}:{d})\n";
                const args = prefix_args ++ .{ self.file, self.line };
                const msg = std.fmt.bufPrint(&buf, fmt, args) catch return error.FormatError;
                try writer.writeAll(msg);
            }
        }
    }
};

/// Name identifier for types and variables.
pub const Name = u32;

/// Represents a struct or union field with type and optional name.
///
/// This struct exists to propagate information from the AIR generator into
/// the parameters of a instruction. Some operations are expected to set
/// the type based on interned information; in those cases, the type will be used.
pub const Type = struct {
    id: ?Name = null,
    ty: union(enum) {
        scalar: void,
        pointer: *const Type,
        optional: *const Type,
        errorunion: *const Type, // error union payload type
        null: *const Type, // used to signal that an optional is being set to null. Inner type must be optional.
        undefined: *const Type, // used to signal that the type is undefined, must be scalar, pointer, optional
        region: *const Type, // unused, for now, will represent slices (maybe)
        @"struct": []const Type, // field types for struct
        @"union": []const Type, // field types for union
        allocator: Name, // allocator type identified by type_id (vtable FQN hash)
        fnptr: void, // function pointer type marker - choices stored separately in Src
        recursive: Name, // reference to the refinement that is being recurred.
        void: void,
        unimplemented: void, // placeholder for unhandled types - will crash if accessed
    },
};

/// Source reference for instructions - indicates where a value comes from.
/// Used by store, br, ret_safe, load, and other tags that reference source values.
pub const Src = union(enum) {
    /// Runtime value from a result in the results table (index into results[])
    inst: usize,
    /// Interned variable by IP index - look up in refinements.getGlobal()
    /// If found, it's a tracked user global; if not, it's a non-user interned var
    /// This includes both direct globals and field pointers (which have their own IP index)
    int_var: u32,
    /// Interned constant - reify refinement from embedded type
    int_const: Type,
    /// Function pointer constant - array of possible target functions
    int_fnptr: []const FnInterpreter,
};



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

/// Struct type descriptor with type_id for field name lookup.
pub const StructType = struct {
    type_id: u32, // InternPool index for getFieldId lookup
    fields: []const Type,
    is_packed: bool = false, // true for packed structs (read-modify-write initialization)
};

/// Union type descriptor with type_id for variant name lookup.
pub const UnionType = struct {
    type_id: u32, // InternPool index for getFieldId lookup
    variants: []const Type,
};

/// Type descriptor for AIR instructions. Propagates type information from
/// codegen into instruction parameters for analysis modules.
pub const Type = union(enum) {
    scalar: void, // simple value type (int, float, bool, etc.)
    pointer: *const Type, // pointer to inner type
    optional: *const Type, // optional wrapping inner type
    errorunion: *const Type, // error union with payload type
    null: *const Type, // signals setting optional to null; inner is the optional's payload type
    undefined: *const Type, // signals undefined value; inner is the actual type
    region: *const Type, // slice/array region; inner is element type
    @"struct": *const StructType, // struct with type_id and field types
    @"union": *const UnionType, // union with type_id and variant types
    allocator: Name, // pseudo-allocator, should always be pointed to, the allocator
                     // pointer represents the pointer to metadata + pointer to vtable.
    fnptr: void, // function pointer (choices stored in Src.fnptr)
    recursive: Name, // self-referential type placeholder (Name unused, set to 0)
    void: void, // void type
    unimplemented: void, // placeholder for unhandled types (will crash if accessed)
};

/// Source reference for instructions - indicates where a value comes from.
/// Used by store, br, ret_safe, load, and other tags that reference source values.
pub const Src = union(enum) {
    /// Runtime value from a result in the results table (index into results[])
    inst: usize,
    /// Interned value - look up in refinements.getGlobal() by ip_idx.
    /// If found, it's a tracked user global; if not, reify from embedded type.
    /// This unifies what was previously int_var (globals) and int_const (comptime constants).
    interned: Interned,
    /// Function pointer constant - array of possible target functions
    fnptr: []const FnInterpreter,
};

/// Interned value reference - combines IP index for lookup with type info for reification.
pub const Interned = struct {
    /// InternPool index of this value
    ip_idx: u32,
    /// Type info for reifying refinement if not found in global_map
    ty: Type,
};



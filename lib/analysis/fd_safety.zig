const std = @import("std");
const Inst = @import("../Inst.zig");
const Refinements = @import("../Refinements.zig");
const Analyte = @import("../Analyte.zig");
const Gid = Refinements.Gid;
const core = @import("../core.zig");
const Meta = core.Meta;
const tag = @import("../tag.zig");
const gates = @import("gates.zig");
const Context = @import("../Context.zig");
const State = @import("../lib.zig").State;

// =========================================================================
// State types
// =========================================================================

pub const Close = struct {
    meta: Meta,
};

pub const FdType = enum {
    file, // posix.open/openat
    socket, // posix.socket/accept
    pipe, // posix.pipe/pipe2
    epoll, // posix.epoll_create
    dup, // posix.dup/dup2
};

pub const Open = struct {
    meta: Meta, // Where fd was opened
    fd_type: FdType, // Type of fd (file, socket, pipe, etc.)
    closed: ?Close = null, // null = still open
    // Note: returned field removed - use connectivity tracking instead
};

pub const FdSafety = union(enum) {
    untracked: void,
    open: Open,

    /// Trivial copy - no heap allocations to duplicate.
    pub fn copy(self: @This(), allocator: std.mem.Allocator) error{OutOfMemory}!@This() {
        _ = allocator;
        return self;
    }

    /// Hash this analysis state for memoization.
    pub fn hash(self: @This(), hasher: *std.hash.Wyhash) void {
        hasher.update(&.{@intFromEnum(self)});
        switch (self) {
            .open => |o| {
                hasher.update(&.{@as(u8, if (o.closed != null) 1 else 0)});
            },
            .untracked => {},
        }
    }

    /// Runtime call filter for fd operations.
    /// Intercepts posix.open, posix.close, etc.
    pub fn call(
        state: State,
        index: usize,
        return_type: tag.Type,
        args: []const tag.Src,
        fqn: []const u8,
    ) anyerror!bool {
        _ = return_type;

        // posix.open/openat returns fd_t!OpenError
        if (gates.isPosixOpen(fqn) or gates.isPosixOpenat(fqn)) {
            try handleFdOpen(state, index, .file);
            return true;
        }

        // posix.socket returns socket_t!SocketError
        if (gates.isPosixSocket(fqn)) {
            try handleFdOpen(state, index, .socket);
            return true;
        }

        // posix.accept returns socket_t!AcceptError
        if (gates.isPosixAccept(fqn)) {
            try handleFdOpen(state, index, .socket);
            return true;
        }

        // posix.epoll_create returns fd_t!EpollCreateError
        if (gates.isPosixEpollCreate(fqn)) {
            try handleFdOpen(state, index, .epoll);
            return true;
        }

        // posix.dup/dup2 returns fd_t!DupError
        if (gates.isPosixDup(fqn) or gates.isPosixDup2(fqn)) {
            try handleFdOpen(state, index, .dup);
            return true;
        }

        // posix.pipe returns struct with read/write fds
        if (gates.isPosixPipe(fqn)) {
            try handlePipeOpen(state, index);
            return true;
        }

        // posix.close takes fd and returns void
        if (gates.isPosixClose(fqn)) {
            try handleFdClose(state, index, args);
            return true;
        }

        // posix.read/write/etc. use fd - check for use-after-close
        if (gates.isPosixRead(fqn) or gates.isPosixWrite(fqn) or
            gates.isPosixPread(fqn) or gates.isPosixPwrite(fqn))
        {
            try checkFdUse(state, args);
            // Intercept to avoid diving into stdlib syscall code
            return true;
        }

        // posix.flock uses fd for file locking - intercept to avoid syscall code
        if (gates.isPosixFlock(fqn)) {
            try checkFdUse(state, args);
            return true;
        }

        return false;
    }

    /// Handle posix.open/openat/socket/accept/etc. calls.
    /// Creates fd refinement (scalar) with open state.
    fn handleFdOpen(state: State, index: usize, fd_type: FdType) !void {
        // Result is errorunion -> scalar (fd_t is i32)
        const eu_idx = state.results[index].refinement orelse return;
        const eu_ref = state.refinements.at(eu_idx);
        if (eu_ref.* != .errorunion) return;

        const fd_idx = eu_ref.errorunion.to;
        const fd_ref = state.refinements.at(fd_idx);
        if (fd_ref.* != .scalar) return;

        // Set fd_safety on the scalar (the fd value)
        fd_ref.scalar.analyte.fd_safety = .{ .open = .{
            .meta = state.ctx.meta,
            .fd_type = fd_type,
        } };
    }

    /// Handle posix.pipe calls.
    /// pipe returns a struct with read_fd and write_fd, both need tracking.
    fn handlePipeOpen(state: State, index: usize) !void {
        // For now, mark result as untracked - pipe returns struct with 2 fds
        // TODO: Track individual pipe fds when we have proper struct field tracking
        const result_idx = state.results[index].refinement orelse return;
        const result_ref = state.refinements.at(result_idx);

        // If it's an errorunion, follow to the payload
        const payload_idx = if (result_ref.* == .errorunion)
            result_ref.errorunion.to
        else
            result_idx;

        const payload_ref = state.refinements.at(payload_idx);

        // If it's a struct with 2 fields (read_fd, write_fd), mark each
        if (payload_ref.* == .@"struct") {
            for (payload_ref.@"struct".fields) |field_gid| {
                const field_ref = state.refinements.at(field_gid);
                if (field_ref.* == .scalar) {
                    field_ref.scalar.analyte.fd_safety = .{ .open = .{
                        .meta = state.ctx.meta,
                        .fd_type = .pipe,
                    } };
                }
            }
        }
    }

    /// Handle posix.close calls.
    /// Checks for double-close and marks fd as closed.
    fn handleFdClose(state: State, index: usize, args: []const tag.Src) !void {
        _ = index; // close returns void

        // close signature: close(fd) -> args[0]=fd
        if (args.len < 1) return;

        // Get fd refinement
        const fd_idx: Gid = switch (args[0]) {
            .inst => |inst| state.results[inst].refinement orelse return,
            .interned, .fnptr => return, // Can't track interned fds
        };

        const fd_ref = state.refinements.at(fd_idx);
        if (fd_ref.* != .scalar) return;

        const fd_state = &(fd_ref.scalar.analyte.fd_safety orelse return);
        if (fd_state.* != .open) return;

        // Check for double-close
        if (fd_state.open.closed) |prev_close| {
            return reportDoubleClose(state.ctx, fd_state.open, prev_close);
        }

        // Mark as closed
        fd_state.open.closed = .{ .meta = state.ctx.meta };
    }

    /// Check fd arguments for use-after-close.
    fn checkFdUse(state: State, args: []const tag.Src) !void {
        // First arg is typically the fd
        if (args.len < 1) return;

        const fd_idx: Gid = switch (args[0]) {
            .inst => |inst| state.results[inst].refinement orelse return,
            .interned, .fnptr => return,
        };

        const fd_ref = state.refinements.at(fd_idx);
        if (fd_ref.* != .scalar) return;

        const fd_state = fd_ref.scalar.analyte.fd_safety orelse return;
        if (fd_state != .open) return;

        // Check for use-after-close
        if (fd_state.open.closed) |close_site| {
            return reportUseAfterClose(state.ctx, fd_state.open, close_site);
        }
    }

    /// Handle ret_safe - with connectivity tracking, no action needed.
    /// FDs reachable from return value are automatically not detected as leaks.
    pub fn ret_safe(state: State, index: usize, params: tag.RetSafe) !void {
        _ = state;
        _ = index;
        _ = params;
    }

    /// Handle ret_load - with connectivity tracking, no action needed.
    /// FDs reachable from return value are automatically not detected as leaks.
    pub fn ret_load(state: State, index: usize, params: tag.RetLoad) !void {
        _ = state;
        _ = index;
        _ = params;
    }

    /// Mark fd as returned (ownership transferred to caller).
    /// Called after receiving a return value from a function call.
    /// With connectivity tracking, no action is needed - fd's reachable from
    /// the return value are automatically not detected as leaks by the caller.
    pub fn call_return(refinements: *Refinements, return_gid: Gid) void {
        _ = refinements;
        _ = return_gid;
    }

    /// FD identity based on where/when it was opened.
    /// Used instead of GID for connectivity tracking because ret_safe deep-copies values.
    const FdIdentity = struct {
        file: []const u8,
        line: u32,
        column: ?u32,
        fd_type: FdType,

        pub fn fromOpen(open: Open) FdIdentity {
            return .{
                .file = open.meta.file,
                .line = open.meta.line,
                .column = open.meta.column,
                .fd_type = open.fd_type,
            };
        }

        pub fn eql(self: FdIdentity, other: FdIdentity) bool {
            return std.mem.eql(u8, self.file, other.file) and
                self.line == other.line and
                self.column == other.column and
                self.fd_type == other.fd_type;
        }

        pub fn hash(self: FdIdentity) u64 {
            var h = std.hash.Wyhash.init(0);
            h.update(self.file);
            h.update(std.mem.asBytes(&self.line));
            if (self.column) |c| h.update(std.mem.asBytes(&c));
            h.update(std.mem.asBytes(&self.fd_type));
            return h.final();
        }
    };

    const FdIdentityContext = struct {
        pub fn hash(_: FdIdentityContext, key: FdIdentity) u64 {
            return key.hash();
        }
        pub fn eql(_: FdIdentityContext, a: FdIdentity, b: FdIdentity) bool {
            return a.eql(b);
        }
    };

    /// End-of-function checks - detect fd leaks.
    pub fn onFinish(results: []Inst, ctx: *Context, refinements: *Refinements, return_gid: Gid) !void {
        // Build set of FD identities reachable from return value (transferred to caller, not leaks)
        // We use FD identity (meta) instead of GID because ret_safe deep-copies values.
        var return_fd_metas = std.HashMap(FdIdentity, void, FdIdentityContext, 80).init(ctx.allocator);
        defer return_fd_metas.deinit();
        collectReachableFds(refinements, return_gid, &return_fd_metas);

        // Check for fd leaks (open fds not closed, not returned)
        for (results) |inst| {
            const idx = inst.refinement orelse continue;

            // Skip arg instructions - fds passed in as arguments are not our
            // responsibility to close, they belong to the caller
            if (inst.inst_tag) |inst_tag| {
                if (std.meta.activeTag(inst_tag) == .arg) continue;
            }

            try checkFdLeakRecursive(refinements, idx, ctx, &return_fd_metas);
        }
    }

    fn checkFdLeakRecursive(refinements: *Refinements, gid: Gid, ctx: *Context, return_fd_metas: *std.HashMap(FdIdentity, void, FdIdentityContext, 80)) !void {
        const ref = refinements.at(gid);
        switch (ref.*) {
            .scalar => {
                const fd = ref.scalar.analyte.fd_safety orelse return;
                if (fd != .open) return;
                if (fd.open.closed != null) return;

                // Skip if this FD identity is reachable from return value
                const fd_id = FdIdentity.fromOpen(fd.open);
                if (return_fd_metas.contains(fd_id)) return;

                return reportFdLeak(ctx, fd.open);
            },
            .errorunion => try checkFdLeakRecursive(refinements, ref.errorunion.to, ctx, return_fd_metas),
            .optional => try checkFdLeakRecursive(refinements, ref.optional.to, ctx, return_fd_metas),
            .@"struct" => |s| {
                for (s.fields) |field_gid| {
                    try checkFdLeakRecursive(refinements, field_gid, ctx, return_fd_metas);
                }
            },
            else => {},
        }
    }

    /// Recursively collect FD identities from a refinement tree.
    fn collectReachableFds(refinements: *Refinements, gid: Gid, fds: *std.HashMap(FdIdentity, void, FdIdentityContext, 80)) void {
        collectReachableFdsInner(refinements, gid, fds, 0);
    }

    fn collectReachableFdsInner(
        refinements: *Refinements,
        gid: Gid,
        fds: *std.HashMap(FdIdentity, void, FdIdentityContext, 80),
        depth: usize,
    ) void {
        if (depth > 100) return;
        if (gid >= refinements.list.items.len) return;

        const ref = refinements.at(gid);
        switch (ref.*) {
            .scalar => {
                // Check for FD state
                if (ref.scalar.analyte.fd_safety) |fd| {
                    if (fd == .open) {
                        fds.put(FdIdentity.fromOpen(fd.open), {}) catch return;
                    }
                }
            },
            .pointer => |p| collectReachableFdsInner(refinements, p.to, fds, depth + 1),
            .optional => |o| collectReachableFdsInner(refinements, o.to, fds, depth + 1),
            .errorunion => |e| collectReachableFdsInner(refinements, e.to, fds, depth + 1),
            .region => |r| collectReachableFdsInner(refinements, r.to, fds, depth + 1),
            .@"struct" => |s| {
                for (s.fields) |field_gid| {
                    collectReachableFdsInner(refinements, field_gid, fds, depth + 1);
                }
            },
            .@"union" => |u| {
                for (u.fields) |maybe_field_gid| {
                    if (maybe_field_gid) |field_gid| {
                        collectReachableFdsInner(refinements, field_gid, fds, depth + 1);
                    }
                }
            },
            else => {},
        }
    }

    // =========================================================================
    // Aggregate Init Handler
    // =========================================================================

    /// Handle aggregate_init - copy fd_safety state from source elements to struct fields.
    pub fn aggregate_init(state: State, index: usize, params: tag.AggregateInit) !void {
        const result_gid = state.results[index].refinement orelse return;
        const result_ref = state.refinements.at(result_gid);

        switch (result_ref.*) {
            .@"struct" => |s| {
                // For structs: copy fd_safety from each source element to corresponding field
                for (s.fields, 0..) |field_gid, i| {
                    if (i >= params.elements.len) break;
                    const src = params.elements[i];
                    copyFdSafetyState(state, field_gid, src);
                }
            },
            .region => |r| {
                // For arrays/regions: use uniform model - first element applies to all
                if (params.elements.len > 0) {
                    copyFdSafetyState(state, r.to, params.elements[0]);
                }
            },
            else => {},
        }
    }

    /// Copy fd_safety state from a source to a destination refinement.
    fn copyFdSafetyState(state: State, dst_gid: Gid, src: tag.Src) void {
        const src_gid: ?Gid = switch (src) {
            .inst => |inst| state.results[inst].refinement,
            .interned => null, // Interned values don't have fd_safety
            .fnptr => null, // Function pointers don't have fd_safety
        };

        // For interned/fnptr sources, nothing to copy
        if (src_gid == null) return;

        const src_ref = state.refinements.at(src_gid.?);
        const dst_ref = state.refinements.at(dst_gid);

        // Copy fd_safety for scalar types
        switch (dst_ref.*) {
            .scalar => |*s| {
                if (src_ref.* != .scalar) return;
                const src_fd = src_ref.scalar.analyte.fd_safety orelse return;
                s.analyte.fd_safety = src_fd;
            },
            .@"struct" => |s| {
                // If destination is a struct, recurse into fields
                if (src_ref.* != .@"struct") return;
                const src_s = src_ref.@"struct";
                for (s.fields, 0..) |field_gid, i| {
                    if (i < src_s.fields.len) {
                        copyFdSafetyStateRecursive(state.refinements, field_gid, src_s.fields[i]);
                    }
                }
            },
            .optional => |o| {
                if (src_ref.* != .optional) return;
                copyFdSafetyStateRecursive(state.refinements, o.to, src_ref.optional.to);
            },
            .errorunion => |e| {
                if (src_ref.* != .errorunion) return;
                copyFdSafetyStateRecursive(state.refinements, e.to, src_ref.errorunion.to);
            },
            .region => |r| {
                if (src_ref.* != .region) return;
                copyFdSafetyStateRecursive(state.refinements, r.to, src_ref.region.to);
            },
            // Other types don't have fd_safety
            else => {},
        }
    }

    /// Recursively copy fd_safety state from source GID to destination GID.
    fn copyFdSafetyStateRecursive(refinements: *Refinements, dst_gid: Gid, src_gid: Gid) void {
        const src_ref = refinements.at(src_gid);
        const dst_ref = refinements.at(dst_gid);

        switch (dst_ref.*) {
            .scalar => |*s| {
                if (src_ref.* != .scalar) return;
                const src_fd = src_ref.scalar.analyte.fd_safety orelse return;
                s.analyte.fd_safety = src_fd;
            },
            .@"struct" => |s| {
                if (src_ref.* != .@"struct") return;
                const src_s = src_ref.@"struct";
                for (s.fields, 0..) |field_gid, i| {
                    if (i < src_s.fields.len) {
                        copyFdSafetyStateRecursive(refinements, field_gid, src_s.fields[i]);
                    }
                }
            },
            .optional => |o| {
                if (src_ref.* != .optional) return;
                copyFdSafetyStateRecursive(refinements, o.to, src_ref.optional.to);
            },
            .errorunion => |e| {
                if (src_ref.* != .errorunion) return;
                copyFdSafetyStateRecursive(refinements, e.to, src_ref.errorunion.to);
            },
            .region => |r| {
                if (src_ref.* != .region) return;
                copyFdSafetyStateRecursive(refinements, r.to, src_ref.region.to);
            },
            else => {},
        }
    }

    // =========================================================================
    // Error Reporting
    // =========================================================================

    fn reportDoubleClose(ctx: *Context, fd_state: Open, prev_close: Close) anyerror {
        try ctx.meta.print(ctx.writer, "double close in ", .{});
        try prev_close.meta.print(ctx.writer, "previously closed in ", .{});
        try fd_state.meta.print(ctx.writer, "originally opened in ", .{});
        return error.DoubleClose;
    }

    fn reportUseAfterClose(ctx: *Context, fd_state: Open, close_site: Close) anyerror {
        try ctx.meta.print(ctx.writer, "use after close in ", .{});
        try close_site.meta.print(ctx.writer, "closed in ", .{});
        try fd_state.meta.print(ctx.writer, "opened in ", .{});
        return error.UseAfterClose;
    }

    fn reportFdLeak(ctx: *Context, fd_state: Open) anyerror {
        try ctx.meta.print(ctx.writer, "fd leak in ", .{});
        try fd_state.meta.print(ctx.writer, "opened in ", .{});
        return error.FdLeak;
    }
};

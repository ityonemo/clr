const core = @import("core.zig");
const Simple = @import("SinglyLinkedList.zig").Simple;

/// A persistent call-chain head. Nodes are allocated and owned by Context.
pub const Trace = Simple(core.Meta);

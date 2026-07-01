const std = @import("std");

/// Vendored from ziglang/zig PR #23590, revision
/// 3ae7876a503be82d7bed91f8532aef12be256068.
pub fn Simple(comptime T: type) type {
    return struct {
        const Self = @This();

        wrapped: std.SinglyLinkedList = .{},

        pub const Item = struct {
            data: T,
            node: std.SinglyLinkedList.Node = .{},

            pub fn next(item: *const Item) ?*Item {
                return @fieldParentPtr("node", item.node.next orelse return null);
            }

            pub fn insertAfter(item: *Item, new_item: *Item) void {
                item.node.insertAfter(&new_item.node);
            }
        };

        pub fn prepend(list: *Self, new_item: *Item) void {
            list.wrapped.prepend(&new_item.node);
        }

        pub fn remove(list: *Self, item: *Item) void {
            list.wrapped.remove(&item.node);
        }

        pub fn popFirst(list: *Self) ?*Item {
            const node = list.wrapped.popFirst() orelse return null;
            return @fieldParentPtr("node", node);
        }

        pub fn at(list: *const Self, index: usize) ?*Item {
            var node = list.wrapped.first orelse return null;
            var remaining = index;
            while (remaining > 0) : (remaining -= 1) {
                node = node.next orelse return null;
            }
            return @fieldParentPtr("node", node);
        }

        pub fn len(list: Self) usize {
            return list.wrapped.len();
        }
    };
}

test "Simple wraps intrusive list nodes" {
    const List = Simple(u32);
    var list: List = .{};
    var one: List.Item = .{ .data = 1 };
    var two: List.Item = .{ .data = 2 };

    list.prepend(&one);
    one.insertAfter(&two);

    try std.testing.expectEqual(@as(usize, 2), list.len());
    try std.testing.expectEqual(@as(u32, 1), list.at(0).?.data);
    try std.testing.expectEqual(@as(u32, 2), one.next().?.data);
    try std.testing.expect(two.next() == null);
}

test "copying Simple head permits persistent branching" {
    const List = Simple(u32);
    var root: List = .{};
    var one: List.Item = .{ .data = 1 };
    var left: List.Item = .{ .data = 2 };
    var right: List.Item = .{ .data = 3 };

    root.prepend(&one);

    var left_branch = root;
    left_branch.prepend(&left);

    var right_branch = root;
    right_branch.prepend(&right);

    try std.testing.expectEqual(@as(u32, 1), root.at(0).?.data);
    try std.testing.expectEqual(@as(u32, 2), left_branch.at(0).?.data);
    try std.testing.expectEqual(@as(u32, 1), left_branch.at(1).?.data);
    try std.testing.expectEqual(@as(u32, 3), right_branch.at(0).?.data);
    try std.testing.expectEqual(@as(u32, 1), right_branch.at(1).?.data);
}

const Node = struct {
    value: u8,
    next: ?*Node,
};

pub fn main() u8 {
    var node1: Node = undefined;
    node1.next = null;
    var node2: Node = undefined;
    node2.value = 2;
    node2.next = &node1;
    return node2.next.?.value; // ERROR: node1.value is undefined
}

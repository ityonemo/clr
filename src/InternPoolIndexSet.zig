const std = @import("std");
const compiler = @import("compiler");

const Allocator = std.mem.Allocator;
const Index = compiler.InternPool.Index;

const empty: u8 = 0;
const occupied: u8 = 1;
const tombstone: u8 = 2;

keys: []Index = &.{},
states: []u8 = &.{},
len: usize = 0,
tombstones: usize = 0,

pub fn deinit(self: *@This(), allocator: Allocator) void {
    if (self.keys.len != 0) allocator.free(self.keys);
    if (self.states.len != 0) allocator.free(self.states);
    self.* = .{};
}

pub fn count(self: *const @This()) usize {
    return self.len;
}

pub fn contains(self: *const @This(), key: Index) bool {
    if (self.states.len == 0) return false;

    var slot = hash(key) & (self.states.len - 1);
    while (self.states[slot] != empty) : (slot = (slot + 1) & (self.states.len - 1)) {
        if (self.states[slot] == occupied and self.keys[slot] == key) return true;
    }
    return false;
}

pub fn put(self: *@This(), allocator: Allocator, key: Index, _: void) Allocator.Error!void {
    try self.ensureCapacity(allocator);
    self.insertAssumeCapacity(key);
}

pub fn popExpected(self: *@This(), key: Index) void {
    if (self.states.len == 0) @panic("empty InternPool index set");

    var slot = hash(key) & (self.states.len - 1);
    while (self.states[slot] != empty) : (slot = (slot + 1) & (self.states.len - 1)) {
        if (self.states[slot] == occupied and self.keys[slot] == key) {
            self.states[slot] = tombstone;
            self.len -= 1;
            self.tombstones += 1;
            return;
        }
    }
    @panic("InternPool index is not active");
}

fn ensureCapacity(self: *@This(), allocator: Allocator) Allocator.Error!void {
    if (self.states.len == 0) {
        try self.rehash(allocator, 16);
        return;
    }
    if ((self.len + self.tombstones + 1) * 4 >= self.states.len * 3) {
        try self.rehash(allocator, self.states.len * 2);
    }
}

fn rehash(self: *@This(), allocator: Allocator, capacity: usize) Allocator.Error!void {
    const new_keys = try allocator.alloc(Index, capacity);
    errdefer allocator.free(new_keys);
    const new_states = try allocator.alloc(u8, capacity);
    @memset(new_states, empty);

    const old_keys = self.keys;
    const old_states = self.states;

    self.keys = new_keys;
    self.states = new_states;
    self.len = 0;
    self.tombstones = 0;

    for (old_states, 0..) |state, slot| {
        if (state == occupied) self.insertAssumeCapacity(old_keys[slot]);
    }

    if (old_keys.len != 0) allocator.free(old_keys);
    if (old_states.len != 0) allocator.free(old_states);
}

fn insertAssumeCapacity(self: *@This(), key: Index) void {
    var slot = hash(key) & (self.states.len - 1);
    var first_tombstone: ?usize = null;

    while (self.states[slot] != empty) : (slot = (slot + 1) & (self.states.len - 1)) {
        if (self.states[slot] == occupied and self.keys[slot] == key) return;
        if (self.states[slot] == tombstone and first_tombstone == null) first_tombstone = slot;
    }

    const target = first_tombstone orelse slot;
    if (self.states[target] == tombstone) self.tombstones -= 1;
    self.keys[target] = key;
    self.states[target] = occupied;
    self.len += 1;
}

fn hash(key: Index) usize {
    var value: u64 = @intFromEnum(key);
    value ^= value >> 30;
    value *%= 0xbf58476d1ce4e5b9;
    value ^= value >> 27;
    value *%= 0x94d049bb133111eb;
    value ^= value >> 31;
    return @truncate(value);
}

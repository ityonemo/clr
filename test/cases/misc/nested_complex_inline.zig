inline fn read(data: []const u8) u64 {
    return @bitCast(data[0..8].*);
}

inline fn round(state: *[2]u64, input: *const [16]u8) void {
    inline for (0..2) |i| {
        const a = read(input[i * 8 ..]);
        state[i] = a;
    }
}

pub fn hash(input: []const u8) u64 {
    var state = [2]u64{ 0, 0 };

    if (input.len >= 16) {
        round(&state, input[0..16]);
    }

    return state[0];
}

pub fn main() u8 {
    const result = hash("seventeen bytes!");
    _ = result;
    return 0;
}

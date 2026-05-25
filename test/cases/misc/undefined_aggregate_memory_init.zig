const State = struct {
    a: u64,
    b: u64,
    lanes: [2]u64,
    c: u64,
    tail: [2]u64,
    d: u64,

    fn init(seed: u64) State {
        var state: State = .{
            .a = undefined,
            .b = undefined,
            .lanes = undefined,
            .c = seed,
            .tail = undefined,
            .d = seed + 1,
        };

        state.a = seed ^ 0xa5a5;
        state.b = seed ^ 0x5a5a;
        state.lanes[0] = state.a;
        state.lanes[1] = state.b;
        state.tail[0] = state.lanes[0] ^ state.c;
        state.tail[1] = state.lanes[1] ^ state.d;
        return state;
    }
};

pub fn main() u8 {
    const state = State.init(3);
    return @intCast((state.tail[0] ^ state.tail[1]) & 0xff);
}

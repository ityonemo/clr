// Test: tokenizer-like state machine pattern
pub fn main() u8 {
    const State = enum { start, reading, done, eof };
    const input = "abc";
    var pos: usize = 0;
    var token_len: u8 = undefined;

    state: switch (State.start) {
        .start => { // START state
            if (pos < input.len) {
                token_len = 0;
                continue :state .reading;
            }
            // EOF - token_len never set
            continue :state .eof;
        },
        .reading => { // READING state
            token_len += 1;
            pos += 1;
            if (pos < input.len) {
                continue :state .reading; // Keep reading
            }
            continue :state .done;
        },
        .done => {
            // token_len is defined here
        },
        .eof => {
            // token_len may be undefined if we came from start
        },
    }
    return token_len; // May be undefined depending on path
}

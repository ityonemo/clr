// Test: tokenizer-like state machine pattern
pub fn main() u8 {
    const input = "abc";
    var pos: usize = 0;
    var token_len: u8 = undefined;

    state: switch (@as(u8, 0)) {
        0 => { // START state
            if (pos < input.len) {
                token_len = 0;
                continue :state 1;
            }
            // EOF - token_len never set
            continue :state 3;
        },
        1 => { // READING state
            token_len += 1;
            pos += 1;
            if (pos < input.len) {
                continue :state 1; // Keep reading
            }
            continue :state 2;
        },
        2 => { // DONE state
            // token_len is defined here
        },
        3 => { // EOF state
            // token_len may be undefined if we came from state 0
        },
        else => {},
    }
    return token_len; // May be undefined depending on path
}

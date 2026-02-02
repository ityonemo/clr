// Simple nested loop - both loops complete normally
pub fn main() u8 {
    var sum: u8 = 0;
    var i: u8 = 0;
    while (i < 3) : (i += 1) {
        var j: u8 = 0;
        while (j < 3) : (j += 1) {
            sum += 1;
        }
    }
    return sum; // OK: sum is defined
}

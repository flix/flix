///
/// We can use type-level Booleans to represent 4-bit integers.
/// Here each Boolean is a bit from msb to lsb.
///
enum I4[_: Bool, _: Bool, _: Bool, _: Bool] with Eq {
    case I4
}

///
/// We can specify a function that returns the 3.
///
def mk3(): I4[false, false, true, true] = I4.I4

///
/// We can define type-level increment on 4-bit integers.
///
/// This is somewhat involved since we have to keep track of overflow.
///
def inc(_: I4[a, b, c, d]):
    I4[a xor (b and c and d), b xor (c and d), c xor d, not d] = I4.I4

///
/// We can now specify a function that returns 4.
///
/// The type system ensures that we cannot make a mistake.
///
def mk4(): I4[false, true, false, false] = inc(mk3())

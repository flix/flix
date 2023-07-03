///
/// We can use type-level Booleans to represent 4-bit integers.
///
/// We represent a 4-bit integer as 4 bits: a, b, c, d.
///
enum I4[_a: Bool, _b: Bool, _c: Bool, _d: Bool] {
    case I4 // Dummy value
}

///
/// We can specify a function that returns the number 3.
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
/// We can now specify a function that returns the number 4.
///
/// The type system ensures that we cannot make a mistake.
///
def mk4(): I4[false, true, false, false] = inc(mk3())

///
/// We can use type inference to compute the number 7.
///
def f(): Unit =
    let _seven = inc(inc(inc(mk4()))); // Hover to see type.
    ()

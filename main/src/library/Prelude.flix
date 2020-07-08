///
/// The identity function.
///
pub def identity(x: a): a = x

///
/// Returns the constant function with return value `y`.
/// That is, returns `f` such that `f(x) = y` for all inputs `x`.
///
pub def constant(y: a): b -> a = _ -> y

///
/// Returns the function `f` with input arguments swapped.
/// That is, given the function `f: (a, b) -> c`, returns a function `(b, a) -> c`
///
pub def flip(f: (a, b) -> c & e): (b, a) -> c & e = (x, y) -> f(y, x)

///
/// Returns the result of currying the function `f`.
/// That is, given the function `f: (a, b) -> c`, returns a function `a -> b -> c`
///
pub def curry(f: (a, b) -> c & e): a -> (b -> c & e) = x -> y -> f(x, y)

///
/// Returns the result of uncurrying the function `f`.
/// That is, given the function `f: a -> b -> c`, returns a function `(a, b) -> c`
///
pub def uncurry(f: a -> (b -> c & e1) & e2): (a, b) -> c & (e1 and e2) = (x, y) -> (y |> f(x))

///
/// Returns the first component of `t`.
///
pub def fst(p: (a, b)): a =
    let (x, _) = p;
        x

///
/// Returns the second component of `t`.
///
pub def snd(p: (a, b)): b =
    let (_, y) = p;
        y

///
/// Returns the pair `p` with the components swapped.
/// That is, returns `(y, x)` if `p = (x, y)`.
///
pub def swap(p: (a, b)): (b, a) =
    let (x, y) = p;
        (y, x)

///
/// Forwards function composition. Applies the function on the left first.
///
/// Given the functions `f: a -> b` and `g: b -> c` returns a function `a -> c`
///
pub def >>(f: a -> b & e1, g: b -> c & e2): a -> c & (e1 and e2) = x -> g(f(x))

///
/// Pipes the given value `x` into the function `f`.
///
/// Given a value `x` and a function `f` returns `f(x)`.
///
pub def |>(x: a, f: a -> b & e): b & e = f(x)

///
/// Pipes the given pair `x` into the function `f`.
///
/// Given a pair `x` and a function `f` returns `f(x)`.
///
pub def ||>(x: (a, b), f: ((a, b)) -> c & e): c & e = f(x)

///
/// Panics (crashes) the current process with the given message `m`.
///
pub def panic(m: String): a = ?panic

///
/// Meta-programming facility to express the time complexity of a function `f`.
///
pub def time(_f: a -> Int32): Int32 = 1

///
/// Meta-programming facility to express the space complexity of a function `f`.
///
pub def space(_f: a -> Int32): Int32 = 1

///
/// The commutative law asserts that for a binary operator `f` the result of `f(x, y)` is equal to `f(y, x)`.
///
law commutative(f: (a, a) -> b): Bool =
    ∀(x: a, y: a). f(x, y) == f(y, x)

///
/// The associative law asserts that for a binary operator `f` the result of `f(x, f(y, z))` is equal to `f(f(x, y), z)`.
///
law associative(f: (a, a) -> a): Bool =
    ∀(x: a, y: a, z: a). f(x, f(y, z)) == f(f(x, y), z)

///
/// The reflexivity law asserts that any element is less than or equal to itself.
///
law reflexive(⊑: (e, e) -> Bool): Bool = ∀(x: e). x ⊑ x

///
/// The anti-symmetry law asserts that if `x` is less than or equal to `y` and vice versa then the
/// two elements must be equal.
///
law antiSymmetric(⊑: (e, e) -> Bool): Bool =
    use Bool.{∧, →};
    ∀(x: e, y: e). ((x ⊑ y) ∧ (y ⊑ x)) → (x == y)

///
/// The transitivity law asserts that if `x` less than or equal to `y` and `y` is less than or equal
/// to `z` then `x` must be less than or equal to `z`.
///
law transitive(⊑: (e, e) -> Bool): Bool =
    use Bool.{∧, →};
    ∀(x: e, y: e, z: e). ((x ⊑ y) ∧ (y ⊑ z)) → (x ⊑ z)

///
/// The non-negative law asserts that the co-domain of `h` is non-negative.
///
law nonNegative(h: e -> BigInt): Bool = ∀(x: e). h(x) >= 0ii

///
/// The decreasing law asserts that if an element `x` is strictly less than an element `y` then
/// the height assigned to `x` is stricty greater than the height assigned to `y`.
///
law decreasing(h: e -> BigInt, equ: (e, e) -> Bool, ⊑: (e, e) -> Bool): Bool =
    use Bool.{∧, →};
    ∀(x: e, y: e). (x ⊑ y ∧ !equ(x, y)) → (h(x) > h(y))

///
/// Asserts that the expression `x` is contextually equivalent to the expression `y`.
///
def ===(_x: a, _y: a): Bool = panic("meta-programming construct")

namespace Tuple2 {

    ///
    /// Returns `true` if and only if `t1` and `t2` and equal.
    ///
    def __eq(t1: (a, b), t2: (a, b)): Bool =
        let (a1, b1) = t1;
        let (a2, b2) = t2;
            a1 == a2 && b1 == b2

    ///
    /// Compares `t1` and `t2` lexicographically.
    ///
    def __cmp(t1: (a, b), t2: (a, b)): Int32 =
        let (a1, b1) = t1;
        let (a2, b2) = t2;
            let cmp1 = a1 <=> a2;
            if (cmp1 != 0)
                cmp1
            else
                b1 <=> b2
}

namespace Tuple3 {

    ///
    /// Returns `true` if and only if `t1` and `t2` and equal.
    ///
    def __eq(t1: (a, b, c), t2: (a, b, c)): Bool =
        let (a1, b1, c1) = t1;
        let (a2, b2, c2) = t2;
            a1 == a2 && b1 == b2 && c1 == c2

    ///
    /// Compares `t1` and `t2` lexicographically.
    ///
    def __cmp(t1: (a, b, c), t2: (a, b, c)): Int32 =
        let (a1, b1, c1) = t1;
        let (a2, b2, c2) = t2;
            let cmp1 = a1 <=> a2;
            if (cmp1 != 0)
                cmp1
            else
                let cmp2 = b1 <=> b2;
                if (cmp2 != 0)
                    cmp2
                else
                    c1 <=> c2
}

namespace Tuple4 {

    ///
    /// Returns `true` if and only if `t1` and `t2` and equal.
    ///
    def __eq(t1: (a, b, c, d), t2: (a, b, c, d)): Bool =
        let (a1, b1, c1, d1) = t1;
        let (a2, b2, c2, d2) = t2;
            a1 == a2 && b1 == b2 && c1 == c2 && d1 == d2

    ///
    /// Compares `t1` and `t2` lexicographically.
    ///
    def __cmp(t1: (a, b, c, d), t2: (a, b, c, d)): Int32 =
        let (a1, b1, c1, d1) = t1;
        let (a2, b2, c2, d2) = t2;
            let cmp1 = a1 <=> a2;
            if (cmp1 != 0)
                cmp1
            else
                let cmp2 = b1 <=> b2;
                if (cmp2 != 0)
                    cmp2
                else
                    let cmp3 = c1 <=> c2;
                    if (cmp3 != 0)
                        cmp3
                    else
                        d1 <=> d2
}

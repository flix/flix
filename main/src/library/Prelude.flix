///
/// The identity function.
///
@Time(1) @Space(1)
pub def identity(x: a): a = x

///
/// Returns the constant function with return value `y`.
/// That is, returns `f` such that `f(x) = y` for all inputs `x`.
///
@Time(1) @Space(1)
pub def constant(y: a): b -> a = _ -> y

///
/// Returns the function `f` with input arguments swapped.
/// That is, given the function `f: (a, b) -> c`, returns a function `(b, a) -> c`
///
@Time(1) @Space(1)
pub def flip(f: (a, b) -> c & e): (b, a) -> c & e = (x, y) -> f(y, x)

///
/// Returns the result of currying the function `f`.
/// That is, given the function `f: (a, b) -> c`, returns a function `a -> b -> c`
///
@Time(1) @Space(1)
pub def curry(f: (a, b) -> c & e): a -> (b -> c & e) = x -> y -> f(x, y)

///
/// Returns the result of uncurrying the function `f`.
/// That is, given the function `f: a -> b -> c`, returns a function `(a, b) -> c`
///
@Time(1) @Space(1)
pub def uncurry(f: a -> (b -> c & e1) & e2): ((a, b)) -> c & (e1 and e2) = match (x, y) -> f(x, y)

///
/// Returns the first component of `t`.
///
@Time(1) @Space(1)
pub def fst(p: (a, b)): a =
    let (x, _) = p;
        x

///
/// Returns the second component of `t`.
///
@Time(1) @Space(1)
pub def snd(p: (a, b)): b =
    let (_, y) = p;
        y

///
/// Returns the pair `p` with the components swapped.
/// That is, returns `(y, x)` if `p = (x, y)`.
///
@Time(1) @Space(1)
pub def swap(p: (a, b)): (b, a) =
    let (x, y) = p;
        (y, x)

///
/// Forwards function composition. Applies the function on the left first.
///
/// Given the functions `f: a -> b` and `g: b -> c` returns a function `a -> c`
///
@Time(1) @Space(1)
pub def >>(f: a -> b & e1, g: b -> c & e2): a -> c & (e1 and e2) = x -> g(f(x))

///
/// Pipes the given value `x` into the function `f`.
///
/// Given a value `x` and a function `f` returns `f(x)`.
///
@Time(1) @Space(1)
pub def |>(x: a, f: a -> b & e): b & e = f(x)

///
/// Pipes the given pair `x` into the function `f`.
///
/// Given a pair `x` and a function `f` returns `f(x)`.
///
@Time(1) @Space(1)
pub def ||>(x: (a, b), f: ((a, b)) -> c & e): c & e = f(x)

///
/// Unsafely converts `x` to a string using Java's built-in toString.
///
pub def unsafeToString(x: a): String =
    import java.util.Objects:toString(##java.lang.Object);
    toString(x as ##java.lang.Object) as & Pure

///
/// Panics (crashes) the current process with the given message `m`.
///
@Time(1) @Space(1)
pub def panic(_m: String): a = ?panic

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
law commutative[a, b : Eq](f: (a, a) -> b): Bool =
    ∀(x: a, y: a). f(x, y) == f(y, x)

///
/// The associative law asserts that for a binary operator `f` the result of `f(x, f(y, z))` is equal to `f(f(x, y), z)`.
///
law associative[a : Eq](f: (a, a) -> a): Bool =
    ∀(x: a, y: a, z: a). f(x, f(y, z)) == f(f(x, y), z)

///
/// The reflexivity law asserts that any element is less than or equal to itself.
///
law reflexive(⊑: (e, e) -> Bool): Bool = ∀(x: e). x ⊑ x

///
/// The anti-symmetry law asserts that if `x` is less than or equal to `y` and vice versa then the
/// two elements must be equal.
///
law antiSymmetric[e : Eq](⊑: (e, e) -> Bool): Bool =
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
    ∀(x: e, y: e). (x ⊑ y ∧ not equ(x, y)) → (h(x) > h(y))

///
/// Asserts that the expression `x` is contextually equivalent to the expression `y`.
///
def ===(_x: a, _y: a): Bool = panic("meta-programming construct")
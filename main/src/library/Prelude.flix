///
/// The Void type.
///
pub enum Void { }

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
pub def uncurry(f: a -> (b -> c & e1) & e2): (a, b) -> c & {e1, e2} = (x, y) -> (y |> f(x))

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
/// Backwards function composition. Applies the function on the right first.
///
/// Given the functions `f: b -> c` and `g: a -> b` returns a function `a -> c`
///
pub def <<(f: b -> c & e1, g: a -> b & e2): a -> c & {e1, e2} = x -> f(g(x))

///
/// Forwards function composition. Applies the function on the left first.
///
/// Given the functions `f: a -> b` and `g: b -> c` returns a function `a -> c`
///
pub def >>(f: a -> b & e1, g: b -> c & e2): a -> c & {e1, e2} = x -> g(f(x))

///
/// Pipes the given value `x` into the function `f`.
///
/// Given a value `x: a` and a function `f: a -> b` returns `f(x)`.
///
pub def |>(x: a, f: a -> b & e): b & e = f(x)

///
/// Pipes the given pair `p` into the function `f`.
///
/// Given a pair `x: (a1, a1)` and a function `f: ((a1, a2)) -> b` returns `f(x)`.
///
pub def ||>(x: (a1, a2), f: ((a1, a2)) -> b & e): b & e = f(x)

///
/// Pipes the given value `x` into the function `f`.
///
/// Given a value `x: a` and a function `f: a -> b` returns `f(x)`.
///
pub def <|(f: a -> b & e, x: a): b & e = f(x)

///
/// Pipes the given pair `p` into the function `f`.
///
/// Given a pair `x: (a1, a1)` and a function `f: ((a1, a2)) -> b` returns `f(x)`.
///
pub def <||(f: ((a1, a2)) -> b & e, x: (a1, a2)): b & e = f(x)

///
/// Alias for logical conjunction.
///
pub def ∧(x: Bool, y: Bool): Bool = x && y

///
/// Alias for logical disjunction.
///
pub def ∨(x: Bool, y: Bool): Bool = x || y

///
/// Alias for logical implication.
///
pub def →(x: Bool, y: Bool): Bool = !x ∨ y

///
/// Alias for logical biconditional.
///
pub def ↔(x: Bool, y: Bool): Bool = (x → y) ∧ (y → x)

///
/// Alias for exclusive disjunction.
///
pub def ⊕(x: Bool, y: Bool): Bool = (x ∨ y) ∧ !(x ∧ y)

///
/// Alias for the Sheffer stroke.
///
pub def ↑(x: Bool, y: Bool): Bool = !(x ∧ y)

///
/// Returns `true` if the given value `x` is `null`.
///
/// NB: For internal use only.
///
pub def isNull(x: a): Bool =
    import java.util.Objects:isNull(##java.lang.Object);
    isNull(x as ##java.lang.Object) as & Pure

///
/// Asserts that the given boolean `b` is `true`.
///
/// NB: For internal use only.
///
pub def assert!(b: Bool): Bool =
    if (b) true else panic "assertion failed"

///
/// Asserts that the given boolean `b` is `false`.
///
/// NB: For internal use only.
///
pub def assertNot!(b: Bool): Bool =
    if (!b) true else panic "assertion failed"

///
/// Asserts that the given values `x` and `y` are equal.
///
/// NB: For internal use only.
///
pub def assertEq!(x: a, y: a): Bool = if (x == y) true else panic "assertion failed"

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
law antiSymmetric(⊑: (e, e) -> Bool): Bool = ∀(x: e, y: e). ((x ⊑ y) ∧ (y ⊑ x)) → (x == y)

///
/// The transitivity law asserts that if `x` less than or equal to `y` and `y` is less than or equal
/// to `z` then `x` must be less than or equal to `z`.
///
law transitive(⊑: (e, e) -> Bool): Bool = ∀(x: e, y: e, z: e). ((x ⊑ y) ∧ (y ⊑ z)) → (x ⊑ z)

///
/// The non-negative law asserts that the co-domain of `h` is non-negative.
///
law nonNegative(h: e -> BigInt): Bool = ∀(x: e). h(x) >= 0ii

///
/// The decreasing law asserts that if an element `x` is strictly less than an element `y` then
/// the height assigned to `x` is stricty greater than the height assigned to `y`.
///
law decreasing(h: e -> BigInt, equ: (e, e) -> Bool, ⊑: (e, e) -> Bool): Bool = ∀(x: e, y: e). (x ⊑ y && !equ(x, y)) → (h(x) > h(y))

///
/// The Void type.
///
pub enum Void { }

///
/// The True predicate.
///
rel True()

///
/// The False predicate.
///
rel False()

///
/// The identity function.
///
pub def identity[a](x: a): a = x

///
/// Returns the constant function with return value `y`.
/// That is, returns `f` such that `f(x) = y` for all inputs `x`.
///
pub def constant[a, b](y: a): b -> a = _ -> y

///
/// Returns the function `f` with input arguments swapped.
/// That is, given the function `f: (a, b) -> c`, returns a function `(b, a) -> c`
///
pub def flip[a, b, c](f: (a, b) -> c): (b, a) -> c = (x, y) -> f(y, x)

///
/// Returns the result of currying the function `f`.
/// That is, given the function `f: (a, b) -> c`, returns a function `a -> b -> c`
///
pub def curry[a, b, c](f: (a, b) -> c): a -> b -> c = x -> y -> f(x, y)

///
/// Returns the result of uncurrying the function `f`.
/// That is, given the function `f: a -> b -> c`, returns a function `(a, b) -> c`
///
pub def uncurry[a, b, c](f: a -> b -> c): (a, b) -> c = (x, y) -> (y |> f(x))

///
/// Returns the first component of `t`.
///
pub def fst[a, b](p: (a, b)): a =
    let (x, _) = p;
        x

///
/// Returns the second component of `t`.
///
pub def snd[a, b](p: (a, b)): b =
    let (_, y) = p;
        y

///
/// Returns the pair `p` with the components swapped.
/// That is, returns `(y, x)` if `p = (x, y)`.
///
pub def swap[a, b](p: (a, b)): (b, a) =
    let (x, y) = p;
        (y, x)

///
/// Backwards function composition. Applies the function on the right first.
///
/// Given the functions `f: b -> c` and `g: a -> b` returns a function `a -> c`
///
pub def <<[a, b, c](f: b -> c, g: a -> b): a -> c = x -> f(g(x))

///
/// Forwards function composition. Applies the function on the left first.
///
/// Given the functions `f: a -> b` and `g: b -> c` returns a function `a -> c`
///
pub def >>[a, b, c](f: a -> b, g: b -> c): a -> c = x -> g(f(x))

///
/// Pipes the given value `x` into the function `f`.
///
/// Given a value `x: a` and a function `f: a -> b` returns `f(x)`.
///
pub def |>[a, b](x: a, f: a -> b): b = f(x)

///
/// Pipes the given pair `p` into the function `f`.
///
/// Given a pair `x: (a1, a1)` and a function `f: ((a1, a2)) -> b` returns `f(x)`.
///
pub def ||>[a1, a2, b](x: (a1, a2), f: ((a1, a2)) -> b): b = f(x)

///
/// Pipes the given value `x` into the function `f`.
///
/// Given a value `x: a` and a function `f: a -> b` returns `f(x)`.
///
pub def <|[a, b](f: a -> b, x: a): b = f(x)

///
/// Pipes the given pair `p` into the function `f`.
///
/// Given a pair `x: (a1, a1)` and a function `f: ((a1, a2)) -> b` returns `f(x)`.
///
pub def <||[a1, a2, b](f: ((a1, a2)) -> b, x: (a1, a2)): b = f(x)

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
/// Asserts that the given boolean `b` is `true`.
///
pub def assert!(b: Bool): Bool =
    if (b) true else ???

///
/// Asserts that the given boolean `b` is `false`.
///
pub def assertNot!(b: Bool): Bool =
    if (!b) true else ???

///
/// Asserts that the given values `x` and `y` are equal.
///
pub def assertEq![a](x: a, y: a): Bool = if (x == y) true else ???

///
/// The commutative law asserts that for a binary operator `f` the result of `f(x, y)` is equal to `f(y, x)`.
///
law commutative[a, b](f: (a, a) -> b): Bool =
    ∀(x: a, y: a). f(x, y) == f(y, x)

///
/// The associative law asserts that for a binary operator `f` the result of `f(x, f(y, z))` is equal to `f(f(x, y), z)`.
///
law associative[a](f: (a, a) -> a): Bool =
    ∀(x: a, y: a, z: a). f(x, f(y, z)) == f(f(x, y), z)

///
/// The reflexivity law asserts that any element is less than or equal to itself.
///
law reflexive[e](⊑: (e, e) -> Bool): Bool = ∀(x: e). x ⊑ x

///
/// The anti-symmetry law asserts that if `x` is less than or equal to `y` and vice versa then the
/// two elements must be equal.
///
law antiSymmetric[e](⊑: (e, e) -> Bool): Bool = ∀(x: e, y: e). ((x ⊑ y) ∧ (y ⊑ x)) → (x == y)

///
/// The transitivity law asserts that if `x` less than or equal to `y` and `y` is less than or equal
/// to `z` then `x` must be less than or equal to `z`.
///
law transitive[e](⊑: (e, e) -> Bool): Bool = ∀(x: e, y: e, z: e). ((x ⊑ y) ∧ (y ⊑ z)) → (x ⊑ z)

///
/// The non-negative law asserts that the co-domain of `h` is non-negative.
///
law nonNegative[e](h: e -> BigInt): Bool = ∀(x: e). h(x) >= 0ii

///
/// The decreasing law asserts that if an element `x` is strictly less than an element `y` then
/// the height assigned to `x` is stricty greater than the height assigned to `y`.
///
law decreasing[e](h: e -> BigInt, equ: (e, e) -> Bool, ⊑: (e, e) -> Bool): Bool = ∀(x: e, y: e). (x ⊑ y && !equ(x, y)) → (h(x) > h(y))

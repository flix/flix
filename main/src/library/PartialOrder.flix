///
/// A Partial Order is a function ⊑ which satisfies three properties: reflexivity, anti-symmetry, and transitivity.
///
namespace PartialOrder {

    ///
    /// The monotonicity law asserts that a function `f` is order-preserving w.r.t.
    /// the partial order `dom` on its domain and the partial order `codom` on its co-domain.
    ///
    /// NB: This particular law is for unary functions.
    ///
    law monotone1[a, b](dom: (a, a) -> Bool, f: a -> b, codom: (b, b) -> Bool): Bool =
        ∀(x: a, y: a). (x `dom` y) → (f(x) `codom` f(y))

    ///
    /// The monotonicity law asserts that a function `f` is order-preserving w.r.t.
    /// the partial order `dom` on its domain and the partial order `codom` on its co-domain.
    ///
    /// NB: This particular law is for binary functions.
    ///
    law monotone2[a, b, c](dom1: (a, a) -> Bool, dom2: (b, b) -> Bool, f: (a, b) -> c, codom: (c, c) -> Bool): Bool =
        ∀(x1: a, x2: a, y1: b, y2: b). ((x1 `dom1` y1) ∧ (x2 `dom2` y2)) → (f(x1, x2) `codom` f(y1, y2))

    ///
    /// The safety law asserts that the abstract function `fa` is an over-approximation of the concrete
    /// function `fc` according to the partial order `leq` and abstraction function `alpha`.
    ///
    /// NB: This particular law is for unary functions.
    ///
    law safe1[a, c](alpha: c -> a, leq: (a, a) -> Bool, fc: c -> c, fa: a -> a): Bool =
        ∀(x: c). alpha(fc(x)) `leq` fa(alpha(x))

    ///
    /// The safety law asserts that the abstract function `fa` is an over-approximation of the concrete
    /// function `fc` according to the partial order `leq` and abstraction function `alpha`.
    ///
    /// NB: This particular law is for binary functions.
    ///
    law safe2[a, c](alpha: c -> a, leq: (a, a) -> Bool, fc: (c, c) -> c, fa: (a, a) -> a): Bool =
        ∀(x: c, y: c). alpha(fc(x, y)) `leq` fa(alpha(x), alpha(y))

}

///
/// A Partial Order is a function ⊑ which satisfies three properties: reflexivity, anti-symmetry, and transitivity.
///
namespace PartialOrder {

    ///
    /// The reflexivity law asserts that any element is less than or equal to itself.
    ///
    law reflexivity[e](⊑: (e, e) -> Bool): Bool = ∀(x: e). x ⊑ x

    ///
    /// The anti-symmetry law asserts that if `x` is less than or equal to `y` and vice versa then the
    /// two elements must be equal.
    ///
    law antiSymmetry[e](⊑: (e, e) -> Bool): Bool = ∀(x: e, y: e). ((x ⊑ y) ∧ (y ⊑ x)) → (x == y)

    ///
    /// The transitivity law asserts that if `x` less than or equal to `y` and `y` is less than or equal
    /// to `z` then `x` must be less than or equal to `z`.
    ///
    law transitivity[e](⊑: (e, e) -> Bool): Bool = ∀(x: e, y: e, z: e). ((x ⊑ y) ∧ (y ⊑ z)) → (x ⊑ z)


    ///
    /// The monotonicity law asserts that a function `f` is order-preserving w.r.t.
    /// the partial order `dom` on its domain and the partial order `codom` on its co-domain.
    ///
    /// NB: This particular law is for unary functions.
    ///
    law monotone1[a, b](f: a -> b, dom: (a, a) -> Bool, codom: (b, b) -> Bool): Bool =
        ∀(x: a, y: a). (x `dom` y) → (f(x) `codom` f(y))

    ///
    /// The monotonicity law asserts that a function `f` is order-preserving w.r.t.
    /// the partial order `dom` on its domain and the partial order `codom` on its co-domain.
    ///
    /// NB: This particular law is for binary functions.
    ///
    law monotone2[a, b](f: (a, a) -> b, dom: (a, a) -> Bool, codom: (b, b) -> Bool): Bool =
        ∀(x1: a, x2: a, y1: a, y2: a). ((x1 `dom` y1) ∧ (x2 `dom` y2)) → (f(x1, x2) `codom` f(y1, y2))

    ///
    /// The monotonicity law asserts that a function `f` is order-preserving w.r.t.
    /// the partial order `dom` on its domain and the partial order `codom` on its co-domain.
    ///
    /// NB: This particular law is for ternary functions.
    ///
    law monotone3[a, b](f: (a, a, a) -> b, dom: (a, a) -> Bool, codom: (b, b) -> Bool): Bool =
        ∀(x1: a, x2: a, x3: a, y1: a, y2: a, y3: a).
            ((x1 `dom` y1) ∧ (x2 `dom` y2) ∧ (x3 `dom` y3)) → (f(x1, x2, x3) `codom` f(y1, y2, y3))


}

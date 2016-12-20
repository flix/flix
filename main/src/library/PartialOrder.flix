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

}

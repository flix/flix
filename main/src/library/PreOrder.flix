///
/// A Pre Order is a function ≼ which satisfies two properties: reflexivity and transitivity.
///
namespace PreOrder {

    ///
    /// The reflexivity law asserts that any element is less than or equal to itself.
    ///
    law reflexivity[e](≼: (e, e) -> Bool): Bool = ∀(x: e). x ≼ x

    ///
    /// The transitivity law asserts that if `x` less than or equal to `y` and `y` is less than or equal
    /// to `z` then `x` must be less than or equal to `z`.
    ///
    law transitivity[e](≼: (e, e) -> Bool): Bool = ∀(x: e, y: e, z: e). ((x ≼ y) ∧ (y ≼ z)) → (x ≼ z)

}
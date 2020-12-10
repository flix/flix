///
/// A Total Order is a Partial Order where the function ⊑ satisfies totality.
///
// TODO: Deprecated, but some old stuff still depends on it.
namespace TotalOrder {

    /**
     * The totality law asserts that either `x` is less than or equal to `y`
     * or `y` is less than or equal to `x`, i.e. every pair of elements must
     * be mutually comparable.
     */
    law totality[e](⊑: (e, e) -> Bool): Bool =
        use Bool.∨;
        ∀(x: e, y: e). (x ⊑ y) ∨ (y ⊑ x)

}

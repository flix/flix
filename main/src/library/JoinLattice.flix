///
/// A Join Lattice is pair of functions (⊑, ⊔) where ⊑ is a partial order and ⊔ satisfy two properties:
/// upper-bound and least-upper-bound.
///
namespace JoinLattice {

    // TODO: Move this somewhere else.

    ///
    /// The upper bound law asserts that the least upper bound operator returns an element that is
    /// greater than or equal to each of its arguments.
    ///
    law upperBound[e](⊑: (e, e) -> Bool, ⊔: (e, e) -> e): Bool =
        ∀(x: e, y: e). (x ⊑ (x ⊔ y)) and (y ⊑ (x ⊔ y))

    ///
    /// The least upper bound law asserts that the least upper bound operator returns the smallest
    /// element that is larger than its two arguments.
    ///
    law leastUpperBound[e](⊑: (e, e) -> Bool, ⊔: (e, e) -> e): Bool =
        let implies = (x, y) -> (not x) or y;
        ∀(x: e, y: e, z: e). ((x ⊑ z) and (y ⊑ z)) `implies` ((x ⊔ y) ⊑ z)

}

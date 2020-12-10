///
/// A Join Lattice is pair of functions (⊑, ⊔) where ⊑ is a partial order and ⊔ satisfy two properties:
/// upper-bound and least-upper-bound.
///
// TODO: Deprecated, but some old stuff still depends on it.
namespace JoinLattice {

    ///
    /// The upper bound law asserts that the least upper bound operator returns an element that is
    /// greater than or equal to each of its arguments.
    ///
    law upperBound[e](⊑: (e, e) -> Bool, ⊔: (e, e) -> e): Bool =
        use Bool.∧;
        ∀(x: e, y: e). (x ⊑ (x ⊔ y)) ∧ (y ⊑ (x ⊔ y))

    ///
    /// The least upper bound law asserts that the least upper bound operator returns the smallest
    /// element that is larger than its two arguments.
    ///
    law leastUpperBound[e](⊑: (e, e) -> Bool, ⊔: (e, e) -> e): Bool =
        use Bool.{∧, →};
        ∀(x: e, y: e, z: e). ((x ⊑ z) ∧ (y ⊑ z)) → ((x ⊔ y) ⊑ z)

}

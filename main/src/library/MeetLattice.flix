///
/// A Meet Lattice is pair of functions (⊑, ⊓) where ⊑ is a partial order and ⊓ satisfy two properties:
/// lower-bound and greatest-lower-bound.
///
// TODO: Deprecated, but some old stuff still depends on it.
namespace MeetLattice {

    ///
    /// The lower bound law asserts that the greatest lower bound operator returns an element that
    /// is less than or equal to each of its arguments.
    ///
    law lowerBound[e](⊑: (e, e) -> Bool, ⊓: (e, e) -> e): Bool =
        use Bool.∧;
        ∀(x: e, y: e). ((x ⊓ y) ⊑ x) ∧ ((x ⊓ y) ⊑ y)

    ///
    /// The greatest lower bound law asserts that the greatest lower bound operator returns the
    /// largest element that is smaller than its two arguments.
    ///
    law greatestLowerBound[e](⊑: (e, e) -> Bool, ⊓: (e, e) -> e): Bool =
        use Bool.{∧, →};
        ∀(x: e, y: e, z: e). ((z ⊑ x) ∧ (z ⊑ y)) → (z ⊑ (x ⊓ y))

}

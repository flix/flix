///
/// A Meet Lattice is pair of functions (⊑, ⊓) where ⊑ is a partial order and ⊓ satisfy two properties:
/// lower-bound and greatest-lower-bound.
///
namespace MeetLattice {

    // TODO: Move this somewhere else.

    ///
    /// The lower bound law asserts that the greatest lower bound operator returns an element that
    /// is less than or equal to each of its arguments.
    ///
    law lowerBound[e](⊑: (e, e) -> Bool, ⊓: (e, e) -> e): Bool =
        ∀(x: e, y: e). ((x ⊓ y) ⊑ x) and ((x ⊓ y) ⊑ y)

    ///
    /// The greatest lower bound law asserts that the greatest lower bound operator returns the
    /// largest element that is smaller than its two arguments.
    ///
    law greatestLowerBound[e](⊑: (e, e) -> Bool, ⊓: (e, e) -> e): Bool =
        let implies = (x, y) -> (not x) or y;
        ∀(x: e, y: e, z: e). ((z ⊑ x) and (z ⊑ y)) `implies` (z ⊑ (x ⊓ y))

}

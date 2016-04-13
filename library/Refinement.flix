/**
 * Types equiped with a refinement.
 */
class Refined[E] {

    /**
     * Returns `true` iff the given value `e` is actually a member of the refined type `E`.
     */
    def isRefined(e: E): Bool

}

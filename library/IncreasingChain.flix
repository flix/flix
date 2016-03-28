/**
 * Combinator to construct an increasing chain lattice from a total order.
 */
enum IncreasingChain[E] {
    case Single(E)
}

/**
 * Implementation of `PartialOrder` for `IncreasingChain[E]`.
 */
impl PartialOrder[IncreasingChain[E]] <= TotalOrder[E] {
    def leq(x: IncreasingChain[E], y: IncreasingChain[E]): Bool =
        let IncreasingChain.Single(x1) = x in
        let IncreasingChain.Single(y1) = y in
            x1 <= y1
}

/**
 * Implementation of `JoinLattice` for `IncreasingChain[E]`.
 */
impl JoinLattice[IncreasingChain[E] <= TotalOrder[E] + MinValue[E] {
    def bot: E = minValue

    def lub(x: IncreasingChain[E], y: IncreasingChain[E]): IncreasingChain[E] =
            let IncreasingChain.Single(x1) = x in
            let IncreasingChain.Single(y1) = y in
                IncreasingChain.Single(x1 `maximum` y1)
}

/**
 * Implementation of `MeetLattice` for `IncreasingChain[E]`.
 */
impl MeetLattice[IncreasingChain[E]] <= TotalOrder[E] + MaxValue[E] {
    def top: E = maxValue

    def glb(x: IncreasingChain[E], y: IncreasingChain[E]): IncreasingChain[E] =
            let IncreasingChain.Single(x1) = x in
            let IncreasingChain.Single(y1) = y in
                IncreasingChain.Single(x1 `minimum` y1)
}

type IncSet[A] = IncreasingChain[Set[A]]
type IncInt[A] = IncreasingChain[BigInt]

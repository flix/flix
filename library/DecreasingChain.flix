/**
 * Combinator to construct a decreasing chain lattice from a total order.
 */
enum DecreasingChain[E] {
    case Single(E)
}

/**
 * Implementation of `PartialOrder` for `DecreasingChain[E]`.
 */
impl PartialOrder[DecreasingChain[E]] <= TotalOrder[E] {
    def leq(x: DecreasingChain[E], y: DecreasingChain[E]): Bool =
        let DecreasingChain.Single(x1) = x in
        let DecreasingChain.Single(y1) = y in
            x1 >= y1
}

/**
 * Implementation of `JoinLattice` for `DecreasingChain[E]`.
 */
impl JoinLattice[DecreasingChain[E] <= TotalOrder[E] + MaxValue[E] {
    def bot: E = maxValue

    def lub(x: DecreasingChain[E], y: DecreasingChain[E]): DecreasingChain[E] =
            let DecreasingChain.Single(x1) = x in
            let DecreasingChain.Single(y1) = y in
                DecreasingChain.Single(x1 `mininum` y1)
}

/**
 * Implementation of `MeetLattice` for `DecreasingChain[E]`.
 */
impl MeetLattice[DecreasingChain[E]] <= TotalOrder[E] + MinValue[E] {
    def top: E = minValue

    def glb(x: DecreasingChain[E], y: DecreasingChain[E]): DecreasingChain[E] =
            let DecreasingChain.Single(x1) = x in
            let DecreasingChain.Single(y1) = y in
                DecreasingChain.Single(x1 `maximum` y1)
}

type DecSet[A] = DecreasingChain[Set[A]]
type DecInt[A] = DecreasingChain[BigInt]

// TODO: We can generalize this to chains.
// See below.
enum NatInc {
    case NatInc(Int32)
}

impl PartialOrder[NatInc] {
    def leq(x: NatInc, y: NatInc): Bool =
        let NatInc.NatInc(i1) = x in
        let NatInc.NatInc(i2) = y in
          x <= y
}

impl JoinLattice[NatInc] {
    def bot: NatInc = NatInc.NatInc(0)

    def lub(x: NatInc, y: NatInc): NatInc =
        let NatInc.NatInc(i1) = x in
        let NatInc.NatInc(i2) = y in
          Int32/max(i1, i2)
}

impl MeetLattice[NatInc] {
    def top: NatInc = NatInc.NatInc(Int32/maxValue)

    def glb(x: NatInc, y: NatInc): NatInc =
        let NatInc.NatInc(i1) = x in
        let NatInc.NatInc(i2) = y in
          Int32/min(i1, i2)
}

enum IncreasingChain[E: TotalOrder] {
    case Elm(E)
}

impl PartialOrder[IncreasingChain[E: TotalOrder]] {
    def leq(x: IncreasingChain, y: IncreasingChain): Bool =
        let IncreasingChain.Elm(x1) = x in
        let IncreasingChain.Elm(y1) = y in
           x ⊑ y
}

impl JoinLattice[IncreasingChain[E: TotalOrder]] {
    def bot: IncreasingChain[E] = IncreasingChain.Elm(??? /* TODO: need the smallest element. probly from bounded? */)

    def lub(x: IncreasingChain, y: IncreasingChain): Bool =
        let IncreasingChain.Elm(x1) = x in
        let IncreasingChain.Elm(y1) = y in
           let = e if (x <= y) y else x in
             IncreasingChain.Elm(e)
}

// TODO: Similarly we can implement MeetLattice

// And similarly we can implemenet DecreasingChain

// What about sets?
// - Easy, just define TotalOrd and bounded on sets.
// - Top may be tricky? How to even ...?

// TODO: Give example of how to combine these things, e.g.
// lat L(x: (Int, PreOrdType))
// type F = Opt[PreOrdType]]

// TODO: What happens if you have a type A which is lifted over a new bottom element,
// and you want to apply a function to the original type? You should be able to lift the function too.

def f[E: JoinLattice](x: E): A = ??? // TODO ...

def lift[A: JoinLattice, B: JoinLattice](f: A -> B): Lifted[A] -> Lifted[B] = ??? ///

// but we would still have to call such a function.. hmm


enum NatInc {
    case NatInc(Int32)
}

impl PartialOrder[NatInc] {
    def leq(x: NatInc, y: NatInc): Bool =
        let NatInc.NatInc(i1) = x in
        let NatInc.NatInc(i2) = y in
          x <= y
}

impl JoinSemiLattice[NatInc] {
    def bot: NatInc = NatInc.NatInc(0)

    def lub(x: NatInc, y: NatInc): NatInc =
        let NatInc.NatInc(i1) = x in
        let NatInc.NatInc(i2) = y in
          Int32/max(i1, i2)
}

impl MeetSemiLattice[NatInc] {
    def top: NatInc = NatInc.NatInc(Int32/maxValue)

    def glb(x: NatInc, y: NatInc): NatInc =
        let NatInc.NatInc(i1) = x in
        let NatInc.NatInc(i2) = y in
          Int32/min(i1, i2)
}


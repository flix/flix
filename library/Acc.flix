class Acc[E: JoinLattice] {

    def iso(x: E): Int

    law AscendingChainCondition()

}
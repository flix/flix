namespace Test/Exp/ReifyEff {

    @test
    def reifyEff01(): Bool = reifyEff(doPure) {
        case Pure(_) => true
        case _ => false
    }

    @test
    def reifyEff02(): Bool = reifyEff(doImpure) {
        case Pure(_) => false
        case _ => true
    }

    @test
    def reifyEff03(): Bool = reifyEff(doWithEff) {
        case Pure(_) => false
        case _ => true
    }

    def doPure(): Unit = ()

    def doImpure(): Unit \ Impure = () as \ Impure

    def doWithEff(): Unit \ E = () as \ E

    eff E
}

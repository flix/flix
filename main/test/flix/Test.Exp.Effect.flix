namespace Test/Exp/Effect {

    // no test annotations for now
    // just making it public for redundancy purposes
    // @test
    pub def handleEff01(): Bool = {
        try true with {
            case Fail.fail(_) => false
        }
    }

    // @test
    pub def handleEff02(): Bool = {
        try true with {
            case Fail.fail(_) => resume(false)
        }
    }

    eff Fail {
        def fail(x: String): Unit
    }
}

namespace Test/Exp/Effect {

    // no test annotations for now
    // just making it public for redundancy purposes
    // @test
    pub def handleEff01(): Bool = {
        try true with {
            def Fail.fail(_) = false
        }
    }

    // @test
    pub def handleEff02(): Bool = {
        try true with {
            def Fail.fail(_) = resume(false)
        }
    }

    pub def perform01(): Bool = {
        do Fail.fail(":(")
    }

    pub def handlePerform01(): Bool = {
        try do Fail.fail("rats") with {
            def Fail.fail(_) => true
        }
    }

    eff Fail {
        def fail(x: String): Unit
    }
}

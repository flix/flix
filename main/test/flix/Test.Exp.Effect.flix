namespace Test/Exp/Effect {

    // no test annotations for now
    // just making it public for redundancy purposes
    pub def handleEff01(): Bool = {
        try true with {
            case Fail.fail(_) => false
        }
    }

    eff Fail {
        def fail(x: String): Unit
    }
}

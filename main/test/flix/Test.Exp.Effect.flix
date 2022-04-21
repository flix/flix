namespace Test/Exp/Effect {

    // no test annotations for now
    // just making it public for redundancy purposes
    // @test
    pub def handleEff01(): Bool = {
        try true with Fail {
            def fail(_) = false
        }
    }

    // @test
    pub def handleEff02(): Bool = {
        try true with Fail {
            def fail(_) = resume(false)
        }
    }

    // @test
    pub def handleEff03(): Bool = {
        try true with Empty
    }

    // @test
    pub def perform01(): Bool = {
        do Fail.fail(":(")
    }

    // @test
    pub def perform02(): Bool = {
        try {
            let x = do Console.readln();
            let y = do Console.readln();
            x == y
        } with Console {
            def readln() = resume("hello")
            def println(_) = resume()
        }
    }

    // @test
    pub def handlePerform01(): Bool = {
        try do Fail.fail("rats") with Fail {
            def fail(_) = true
        }
    }

    // @test
    pub def handlePerform02(): Bool = {
        try {
            let _ = do Console.readln();
            true
        } with Console {
            def readln() = "read"
            def println(_) = resume()
        }
    }

    // @test
    pub def handlePerform03(): Bool = {
        // drunken coin flip: flip a coin to see if we can flip a coin
        try {
            try {
                let caught = do Flip.flip();
                if (caught) {
                    Fail.fail("dropped the coin :(")
                } else {
                    do Flip.flip()
                }
            } with Flip {
                def flip() = resume(true)
            }
        } with Fail {
            def fail(_) = false
        }
    }

    // @test
    pub def handlePerform04(): Bool = {
        // flaky failure: flip a coin to see if we recover
        try {
            try {
                try {
                    do Fail.fail("oh no!")
                } with Fail {
                    def fail(msg) = {
                        let reallyFail = do Flip.flip();
                        if (reallyFail) {
                            do Fail.fail(msg)
                        } else {
                            ()
                        }
                    }
                }
            } with Flip {
                def flip() = resume(false)
            }
        } with Fail {
            def fail(_) = false
        }
    }

    pub def singleEffect(): Bool \ Empty = ???

    pub def complementEffect(): Bool \ ~Empty = ???

    pub def unionEffect(): Bool \ Empty + Fail = ???

    pub def intersectionEffect(): Bool \ Empty & Fail = ???

    pub def differenceEffect(): Bool \ Empty - Fail = ???

    pub def effectSet01(): Bool \ { Empty } = ???

//    pub def effectSet02(): Bool \ { Empty, Fail } = ???

//    pub def effectSet03(): Bool \ { Empty, Fail, Console } = ???

//    pub def effectSet04(): Bool \ {} = ???

    eff Empty

    eff Fail {
        def fail(x: String): Unit
    }

    eff Console {
        def readln(): String
        def println(x: String): Unit
    }

    eff Flip {
        def flip(): Bool
    }
}

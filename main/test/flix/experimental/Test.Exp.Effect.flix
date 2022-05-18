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
            do Test/Exp/Effect/Console.println("hi");
            do Test/Exp/Effect/Console.println("bye");
            true
        } with Console {
            def println(_) = resume()
        }
    }

    // @test
    pub def handlePerform01(): Bool = {
        try do Test/Exp/Effect/Fail.fail("rats") with Fail {
            def fail(_) = true
        }
    }

    // @test
    pub def handlePerform02(): Bool = {
        try {
            let _ = do Test/Exp/Effect/Console.println("It was the best of times");
            true
        } with Console {
            def println(_) = resume()
        }
    }

    // @test
    pub def handlePerform03(): Bool = {
        // drunken coin flip: flip a coin to see if we can flip a coin
        try {
            try {
                let caught = do Test/Exp/Effect/Flip.flip();
                if (caught) {
                    do Test/Exp/Effect/Fail.fail("dropped the coin :(")
                } else {
                    do Test/Exp/Effect/Flip.flip()
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

    pub def singleEffect(): Bool \ Flip = ??? as & _

    pub def complementEffect(): Bool \ ~Flip = ??? as & _

    pub def unionEffect(): Bool \ Flip + Fail = ??? as & _

    pub def intersectionEffect(): Bool \ Flip & Fail = ??? as & _

    pub def differenceEffect(): Bool \ Flip - Fail = ??? as & _

    pub def singlePolyEffect(): Bool \ ef1 = ??? as & ef1

    pub def complementPolyEffect(): Bool \ ~ef1 = ??? as & _

    pub def unionPolyEffect(): Bool \ ef1 + ef2 = ??? as & _

    pub def intersectionPolyEffect(): Bool \ ef1 & ef2 = ??? as & _

    pub def differencePolyEffect(): Bool \ ef1 - ef2 = ??? as & _

    pub def effectSet01(): Bool \ { Fail } = ??? as & _

    pub def effectSet02(): Bool \ { Flip, Fail } = ??? as & _

    pub def effectSet03(): Bool \ { Flip, Fail, Console } = ??? as & _

    pub def effectSet04(): Bool \ {} = ??? as & _

    pub def polyEffectSet01[ef: Eff](): Bool \ ef = ??? as & _

    pub def polyEffectSet02[ef: Eff](): Bool \ { ef, Fail } = ??? as & _

    pub def arrowWithEffect01(f: a -> b \ {}): Bool = ??? as & _

    pub def arrowWithEffect02(f: a -> b \ {ef - Fail}): Bool = ??? as & _

    pub def arrowWithEffect03(f: a -> b \ {Console, ef - Fail}): Bool = ??? as & _

    eff Empty

    eff Fail {
        pub def fail(x: String): Unit
    }

    eff Console {
        pub def println(x: String): Unit
    }
}

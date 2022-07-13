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
    pub def handleEff02(): Unit = {
        try () with Fail {
            def fail(_) = resume()
        }
    }

    // @test
    pub def handleEff03(): Bool = {
        try true with EffNoOps
    }

    // @test
    pub def perform01(): Unit \ Fail = {
        do Test/Exp/Effect/Fail.fail(":(")
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
    pub def handlePerform01(): Unit = {
        try do Test/Exp/Effect/Fail.fail("rats") with Fail {
            def fail(_) = ()
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

    pub def without01(): Bool = true without Fail

    pub def without02(): Bool = true without {Fail, Console}

    pub def singleEffect(): Bool \ Fail = ??? as \ Fail

    pub def complementEffect(): Bool \ ~Fail = ??? as \ ~Fail

    pub def unionEffect(): Bool \ Fail + Fail = ??? as \ Fail + Fail

    pub def intersectionEffect(): Bool \ Fail & Fail = ??? as \ Fail & Fail

    pub def differenceEffect(): Bool \ Fail - Fail = ??? as \ Fail - Fail

    pub def singlePolyEffect(): Bool \ ef1 = ??? as \ ef1

    pub def complementPolyEffect(): Bool \ ~ef1 = ??? as \ ~ef1

    pub def unionPolyEffect(): Bool \ ef1 + ef2 = ??? as \ ef1 + ef2

    pub def intersectionPolyEffect(): Bool \ ef1 & ef2 = ??? as \ ef1 & ef2

    pub def differencePolyEffect(): Bool \ ef1 - ef2 = ??? as \ ef1 - ef2

    pub def effectSet01(): Bool \ { Fail } = ??? as \ { Fail }

    pub def effectSet02(): Bool \ { Fail, Fail } = ??? as \ { Fail, Fail }

    pub def effectSet03(): Bool \ { Fail, Fail, Console } = ??? as \ { Fail, Fail, Console }

    pub def effectSet04(): Bool \ {} = ??? as \ {}

    pub def polyEffectSet01[ef: Eff](): Bool \ ef = ??? as \ ef

    pub def polyEffectSet02[ef: Eff](): Bool \ { ef, Fail } = ??? as \ { ef, Fail }

    pub def arrowWithEffect01(f: a -> b \ {}): Bool = ???

    pub def arrowWithEffect02(f: a -> b \ {ef - Fail}): Bool = ???

    pub def arrowWithEffect03(f: a -> b \ {Console, ef - Fail}): Bool = ???

    eff EffNoOps

    eff Fail {
        pub def fail(x: String): Unit
    }

    eff Console {
        pub def println(x: String): Unit
    }
}

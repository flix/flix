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

// TODO disabled until typing
//    pub def singleEffect(): Bool \ Console = ???
//
//    pub def complementEffect(): Bool \ ~Console = ???
//
//    pub def unionEffect(): Bool \ Console + Fail = ???
//
//    pub def intersectionEffect(): Bool \ Console & Fail = ???
//
//    pub def differenceEffect(): Bool \ Console - Fail = ???
//
//    pub def singlePolyEffect(): Bool \ ef1 = ???
//
//    pub def complementPolyEffect(): Bool \ ~ef1 = ???
//
//    pub def unionPolyEffect(): Bool \ ef1 + ef2 = ???
//
//    pub def intersectionPolyEffect(): Bool \ ef1 & ef2 = ???
//
//    pub def differencePolyEffect(): Bool \ ef1 - ef2 = ???
//
//    pub def effectSet01(): Bool \ { Fail } = ???
//
//    pub def effectSet02(): Bool \ { Console, Fail } = ???
//
//    pub def effectSet03(): Bool \ {} = ???
//
//    pub def polyEffectSet01[ef: Eff](): Bool \ ef = ???
//
//    pub def polyEffectSet02[ef: Eff](): Bool \ { ef, Fail } = ???
//
//    pub def arrowWithEffect01(f: a -> b \ {}): Bool = ???
//
//    pub def arrowWithEffect02(f: a -> b \ {ef - Fail}): Bool = ???
//
//    pub def arrowWithEffect03(f: a -> b \ {Console, ef - Fail}): Bool = ???

    eff Empty

    eff Fail {
        pub def fail(x: String): Unit
    }

    eff Console {
        pub def println(x: String): Unit
    }
}

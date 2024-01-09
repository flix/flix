mod Test.Exp.Effect {

    // no test annotations for now
    // just making it public for redundancy purposes
    // @test
    pub def handleEff01(): Bool = {
        try checked_ecast (true) with Fail {
            def fail(_, _cont) = false
        }
    }

    // @test
    pub def handleEff02(): Unit = {
        try checked_ecast (()) with Fail {
            def fail(_, cont) = cont()
        }
    }

    // @test
    pub def handleEff03(): Bool = {
        try checked_ecast (true) with Empty
    }

    // @test
    pub def perform01(): Unit \ Fail = {
        do Test.Exp.Effect.Fail.fail(":(")
    }

    // @test
    pub def perform02(): Bool = {
        try {
            do Test.Exp.Effect.Console.println("hi");
            do Test.Exp.Effect.Console.println("bye");
            true
        } with Console {
            def println(_, cont) = cont()
        }
    }

    // @test
    pub def handlePerform01(): Unit = {
        try do Test.Exp.Effect.Fail.fail("rats") with Fail {
            def fail(_, _cont) = ()
        }
    }

    // @test
    pub def handlePerform02(): Bool = {
        try {
            let _ = do Test.Exp.Effect.Console.println("It was the best of times");
            true
        } with Console {
            def println(_, cont) = cont()
        }
    }

    pub def without01(): Bool = true without Fail

    pub def without02(): Bool = true without {Fail, Console}

    pub def singleEffect(): Bool \ Fail = checked_ecast(???)

    pub def complementEffect(): Bool \ ~Fail = checked_ecast(???)

    pub def unionEffect(): Bool \ Fail + Fail = checked_ecast(???)

    pub def intersectionEffect(): Bool \ Fail & Fail = checked_ecast(???)

    pub def differenceEffect(): Bool \ Fail - Fail = ???

    pub def singlePolyEffect(): Bool \ ef1 = checked_ecast(???)

    pub def complementPolyEffect(): Bool \ ~ef1 = checked_ecast(???)

    pub def unionPolyEffect(): Bool \ ef1 + ef2 = checked_ecast(???)

    pub def intersectionPolyEffect(): Bool \ ef1 & ef2 = checked_ecast(???)

    pub def differencePolyEffect(): Bool \ ef1 - ef2 = checked_ecast(???)

    pub def effectSet01(): Bool \ { Fail } = checked_ecast(???)

    pub def effectSet02(): Bool \ { Fail, Fail } = checked_ecast(???)

    pub def effectSet03(): Bool \ { Fail, Fail, Console } = checked_ecast(???)

    pub def effectSet04(): Bool \ {} = ???

    pub def polyEffectSet01[ef: Eff](): Bool \ ef = checked_ecast(???)

    pub def polyEffectSet02[ef: Eff](): Bool \ { ef, Fail } = checked_ecast(???)

    pub def arrowWithEffect01(f: a -> b \ {}): Bool = ???

    pub def arrowWithEffect02(f: a -> b \ ef - Fail): Bool = ???

    pub def arrowWithEffect03(f: a -> b \ Console + (ef - Fail)): Bool = ???

    pub def enumWithEffect01(f: Do[{Pure}]): Bool = ???

    pub def enumWithEffect02(f: Do[{ef}]): Bool = ???

    pub def enumWithEffect03(f: Do[{ef, Fail}]): Bool = ???

    pub def enumWithEffect04(f: Do[ef - Fail]): Bool = ???

    pub def enumWithEffect05(f: Do[Console + (ef - Fail)]): Bool = ???

    eff Empty

    eff Fail {
        pub def fail(x: String): Unit
    }

    eff Console {
        pub def println(x: String): Unit
    }

    pub enum Do[ef: Eff](Unit -> Unit \ ef)
}

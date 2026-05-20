mod Test.Exp.Effect {

    use Assert.assertTrue

    @Test
    def handleEff01(): Unit \ Assert = assertTrue({
        run true with handler Fail {
            def fail(_, _cont) = false
        }
    })

    @CompileTest
    def handleEff02(): Unit = {
        run () with handler Fail {
            def fail(_, cont) = cont()
        }
    }

    @CompileTest
    def perform01(): Unit \ Fail = {
        Fail.fail(":(")
    }

    @Test
    def perform02(): Unit \ Assert = assertTrue({
        run {
            Console.println("hi");
            Console.println("bye");
            true
        } with handler Console {
            def println(_, cont) = cont()
        }
    })

    @CompileTest
    def handlePerform01(): Unit = {
        run Fail.fail("rats") with handler Fail {
            def fail(_, _cont) = ()
        }
    }

    @Test
    def handlePerform02(): Unit \ Assert = assertTrue({
        run {
            let _ = Console.println("It was the best of times");
            true
        } with handler Console {
            def println(_, cont) = cont()
        }
    })


    @CompileTest
    def singleEffect(): Bool \ Fail = ???

    @CompileTest
    def complementEffect(): Bool \ ~Fail = ???

    @CompileTest
    def unionEffect(): Bool \ Fail + Fail = ???

    @CompileTest
    def intersectionEffect(): Bool \ Fail & Fail = ???

    @CompileTest
    def differenceEffect(): Bool \ Fail - Fail = ???

    @CompileTest
    def singlePolyEffect(): Bool \ ef1 = ???

    @CompileTest
    def complementPolyEffect(): Bool \ ~ef1 = ???

    @CompileTest
    def unionPolyEffect(): Bool \ ef1 + ef2 = ???

    @CompileTest
    def intersectionPolyEffect(): Bool \ ef1 & ef2 = ???

    @CompileTest
    def differencePolyEffect(): Bool \ ef1 - ef2 = ???

    @CompileTest
    def effectSet01(): Bool \ { Fail } = ???

    @CompileTest
    def effectSet02(): Bool \ { Fail, Fail } = ???

    @CompileTest
    def effectSet03(): Bool \ { Fail, Fail, Console } = ???

    @CompileTest
    def effectSet04(): Bool \ {} = ???

    @CompileTest
    def polyEffectSet01[ef: Eff](): Bool \ ef = ???

    @CompileTest
    def polyEffectSet02[ef: Eff](): Bool \ { ef, Fail } = ???

    @CompileTest
    def arrowWithEffect01(f: a -> b \ {}): Bool = ???

    @CompileTest
    def arrowWithEffect02(f: a -> b \ ef - Fail): Bool = ???

    @CompileTest
    def arrowWithEffect03(f: a -> b \ Console + (ef - Fail)): Bool = ???

    @CompileTest
    def enumWithEffect01(f: Do[{}]): Bool = ???

    @CompileTest
    def enumWithEffect02(f: Do[{ef}]): Bool = ???

    @CompileTest
    def enumWithEffect03(f: Do[{ef, Fail}]): Bool = ???

    @CompileTest
    def enumWithEffect04(f: Do[ef - Fail]): Bool = ???

    @CompileTest
    def enumWithEffect05(f: Do[Console + (ef - Fail)]): Bool = ???

    eff Fail {
        def fail(x: String): Unit
    }

    eff Console {
        def println(x: String): Unit
    }

    pub enum Do[ef: Eff](Unit -> Unit \ ef)

    pub eff A

    pub eff B

    pub eff C

    @CompileTest
    def f(): Unit \ {A, B, C} - {B} + {C} = ???

    @CompileTest
    def g(): Unit \ ({A, B, C} - {B}) + {C} = f()

    @CompileTest
    def h(): Unit \ {A, C} = f()
}

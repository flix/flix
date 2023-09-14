// TODO rename file and consolidate with Test.Exp.Effect.flix
mod Test.Exp.Effect {
    use Test.Exp.Effect.Print.print
    use Test.Exp.Effect.Print.println
    use Test.Exp.Effect.Throw.throw

    eff Print {
        pub def println(): Unit
        pub def print(s: String): Unit
    }

    eff Throw {
        pub def throw(): Unit
    }

// TODO EFF-MIGRATION temporarily disabled
//    eff Ding {
//        pub def ding(): Unit
//    }

    @Test
    def testInferEffect01(): Unit \ Print = {
        do Print.println()
    }

    @Test
    def testInferEffect02(): Unit \ Print = {
        do Print.print("hello");
        do Print.println()
    }

    @Test
    def testInferEffect03(): Unit \ {Print, Throw} = {
        do Print.println();
        do Throw.throw()
    }

    @Test
    def testInferEffect04(): Unit \ {Print, Throw} = {
        do Throw.throw();
        do Print.print("hello")
    }

    @Test
    def testInferEffect05(): Unit \ {Print, Throw} = {
        do Throw.throw() without Print;
        do Print.println() without Throw
    }

// TODO EFF-MIGRATION temporarily disabled
//    @Test
//    def testWithout01(): Unit = {
//        () without Print
//    }
//
//    @Test
//    def testWithout02(): Unit = {
//        () without {Print, Throw}
//    }
//
//    @Test
//    def testHofEffect01(): Unit = {
//        noPrint(x -> x)
//    }
//
//    @Test
//    def testHofEffect02(): Unit \ Throw = {
//        noPrint(_ -> do Throw.throw())
//    }
//
//    @Test
//    def testHofEffect03(): Unit = {
//        noPrintNoThrow(x -> x)
//    }
//
//    @Test
//    def testHofEffect04(): Unit = {
//        disjoint(x -> x, x -> x)
//    }
//
//    @Test
//    def testHofEffect05(): Unit \ {Print, Throw} = {
//        disjoint(_ -> do Print.println(), _ -> do Throw.throw())
//    }
//
//    @Test
//    def testHofEffect06(): Unit \ Print = {
//        yesPrint(_ -> do Print.println())
//    }
//
//    @Test
//    def testHofEffect07(): Unit \ {Print, Throw} = {
//        yesPrint(_ -> { do Print.println(); do Throw.throw() })
//    }
//
//    @Test
//    def testHofEffect08(): Unit \ {Print, Ding} = {
//        yesPrintNoThrow(_ -> { do Print.println(); do ding() })
//    }
//
//    def noPrint(f: Unit -> Unit \ ef - Print): Unit \ ef - Print = {
//        f() without Print
//    }
//
//    def noPrintNoThrow(f: Unit -> Unit \ ef - {Print, Throw}): Unit \ ef - {Print, Throw} = {
//        f() without {Print, Throw}
//    }
//
//    def disjoint(f: Unit -> Unit \ ef1, g: Unit -> Unit \ ef2 - ef1): Unit \ {ef1, ef2} = {
//        f(); g()
//    }
//
//    def yesPrint(f: Unit -> Unit \ {ef, Print}): Unit \ {ef, Print} = {
//        f()
//    }
//
//    def yesPrintNoThrow(f: Unit -> Unit \ {ef, Print} - Throw): Unit \ {ef, Print} - Throw = {
//        f() without Throw
//    }

    // @Test
    pub def compose(f: a -> a \ ef, g: a -> a \ ef): (a -> a \ ef) = x -> f(g(x))

    // @Test
    pub def compose3(f: a -> b \ ef, g: b -> a \ ef): (a -> a \ ef) = x -> g(f(x))

    // @Test
    pub def doBoth(f: Unit -> Unit \ ef, g: Unit -> Unit \ ef): Unit \ ef = f(); g()

    // @Test
// TODO EFF-MIGRATION temporarily disabled
//    pub def doBoth2(f: Unit -> Unit \ ef - Print, g: Unit -> Unit \ ef - Throw): Unit \ ef = f(); g()
}

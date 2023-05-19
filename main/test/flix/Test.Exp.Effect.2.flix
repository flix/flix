// TODO rename file and consolidate with Test.Exp.Effect.flix
mod Test.Exp.Effect {
    use Test.Exp.Effect.Ding.ding
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

    eff Ding {
        pub def ding(): Unit
    }

    @test
    def testInferEffect01(): Unit \ Print = {
        do Print.println()
    }

    @test
    def testInferEffect02(): Unit \ Print = {
        do Print.print("hello");
        do Print.println()
    }

    @test
    def testInferEffect03(): Unit \ {Print, Throw} = {
        do Print.println();
        do Throw.throw()
    }

    @test
    def testInferEffect04(): Unit \ {Print, Throw} = {
        do Throw.throw();
        do Print.print("hello")
    }

    @test
    def testInferEffect05(): Unit \ {Print, Throw} = {
        do Throw.throw() without Print;
        do Print.println() without Throw
    }

    @test
    def testWithout01(): Unit = {
        () without Print
    }

    @test
    def testWithout02(): Unit = {
        () without {Print, Throw}
    }

    @test
    def testHofEffect01(): Unit = {
        noPrint(x -> x)
    }

    @test
    def testHofEffect02(): Unit \ Throw = {
        noPrint(_ -> do Throw.throw())
    }

    @test
    def testHofEffect03(): Unit = {
        noPrintNoThrow(x -> x)
    }

    @test
    def testHofEffect04(): Unit = {
        disjoint(x -> x, x -> x)
    }

    @test
    def testHofEffect05(): Unit \ {Print, Throw} = {
        disjoint(_ -> do Print.println(), _ -> do Throw.throw())
    }

    @test
    def testHofEffect06(): Unit \ Print = {
        yesPrint(_ -> do Print.println())
    }

    @test
    def testHofEffect07(): Unit \ {Print, Throw} = {
        yesPrint(_ -> { do Print.println(); do Throw.throw() })
    }

    @test
    def testHofEffect08(): Unit \ {Print, Ding} = {
        yesPrintNoThrow(_ -> { do Print.println(); do ding() })
    }

    def noPrint(f: Unit -> Unit \ ef - Print): Unit \ ef - Print = {
        f() without Print
    }

    def noPrintNoThrow(f: Unit -> Unit \ ef - {Print, Throw}): Unit \ ef - {Print, Throw} = {
        f() without {Print, Throw}
    }

    def disjoint(f: Unit -> Unit \ ef1, g: Unit -> Unit \ ef2 - ef1): Unit \ {ef1, ef2} = {
        f(); g()
    }

    def yesPrint(f: Unit -> Unit \ {ef, Print}): Unit \ {ef, Print} = {
        f()
    }

    def yesPrintNoThrow(f: Unit -> Unit \ {ef, Print} - Throw): Unit \ {ef, Print} - Throw = {
        f() without Throw
    }

    // @test
    pub def compose(f: a -> a \ ef, g: a -> a \ ef): (a -> a \ ef) = x -> f(g(x))

    // @test
    pub def compose3(f: a -> b \ ef, g: b -> a \ ef): (a -> a \ ef) = x -> g(f(x))

    // @test
    pub def doBoth(f: Unit -> Unit \ ef, g: Unit -> Unit \ ef): Unit \ ef = f(); g()

    // @test
    pub def doBoth2(f: Unit -> Unit \ ef - Print, g: Unit -> Unit \ ef - Throw): Unit \ ef = f(); g()
}

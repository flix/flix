// TODO rename file and consolidate with Test.Exp.Effect.flix
namespace Test/Exp/Effect {
    use Test/Exp/Effect/Ding.ding;
    use Test/Exp/Effect/Print.print;
    use Test/Exp/Effect/Print.println;
    use Test/Exp/Effect/Throw.throw;

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
        do println()
    }

    @test
    def testInferEffect02(): Unit \ Print = {
        do print("hello");
        do println()
    }

    @test
    def testInferEffect03(): Unit \ {Print, Throw} = {
        do println();
        do throw()
    }

    @test
    def testInferEffect04(): Unit \ {Print, Throw} = {
        do throw();
        do print("hello")
    }

    @test
    def testInferEffect05(): Unit \ {Print, Throw} = {
        do throw() without Print;
        do println() without Throw
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
        noPrint(_ -> do throw())
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
        disjoint(_ -> do println(), _ -> do throw())
    }

    @test
    def testHofEffect06(): Unit \ Print = {
        yesPrint(_ -> do println())
    }

    @test
    def testHofEffect07(): Unit \ {Print, Throw} = {
        yesPrint(_ -> { do println(); do throw() })
    }

    @test
    def testHofEffect08(): Unit \ {Print, Ding} = {
        yesPrintNoThrow(_ -> { do println(); do ding() })
    }

    def noPrint(f: Unit -> Unit \ {ef - Print}): Unit \ {ef - Print} = {
        f() without Print
    }

    def noPrintNoThrow(f: Unit -> Unit \ {ef - Print - Throw}): Unit \ {ef - Print - Throw} = {
        f() without {Print, Throw}
    }

    def disjoint(f: Unit -> Unit \ ef1, g: Unit -> Unit \ ef2 - ef1): Unit \ {ef1, ef2} = {
        f(); g()
    }

    def yesPrint(f: Unit -> Unit \ {ef, Print}): Unit \ {ef, Print} = {
        f()
    }

    def yesPrintNoThrow(f: Unit -> Unit \ {(ef + Print) - Throw}): Unit \ {(ef + Print) - Throw} = {
        f() without Throw
    }
}

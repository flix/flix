mod Test.Exp.LetRec {

    ///
    /// Basic Tail Recursion.
    ///
    @test
    def testLetRec01(): Bool =
        def loop(n, acc) = if (n == 0) acc else loop(n - 1, acc + 1);
        loop(10, 0) == 10
}

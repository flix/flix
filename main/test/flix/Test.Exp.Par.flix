namespace Test/Exp/Par {

    def f(x: Int32): Bool = x == 1

    @test
    def testPar01(): Bool =
        par f(1)

    @test
    def testPar02(): Bool = // Could this be simplified?
        let f = (a, b, c) -> a and b and c;
        let a = () -> List.range(0, 2000) |> List.forall(x -> x > -1);
        let b = () -> a();
        let c = () -> b();
        par f(a(), b(), c())

}

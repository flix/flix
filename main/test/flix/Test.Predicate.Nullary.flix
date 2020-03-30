namespace Test/Predicate/Nullary {

    @test
    def testNullary01(): Bool =
        Nullary(). |= Nullary().

    @test
    def testNullary02(): Bool =
        let x = Nullary().;
        let y = Nullary().;
        (solve x) |= y

    @test
    def testNullary03(): Bool =
        let x = Nullary().;
        let y = Nullary().;
        (solve x <+> y) |= Nullary().

    @test
    def testNullary04(): Bool =
        let x = Rich().;
        let y = Famous().;
        (solve x <+> y) |= Rich().

    @test
    def testNullary05(): Bool =
        let x = Rich().;
        let y = Famous().;
        (solve x <+> y) |= Famous().

    @test
    def testNullary06(): Bool =
        let x = Rich().;
        let y = Famous().;
        !((solve x <+> y) |= Happy().)

    @test
    def testNullary07(): Bool =
        let x = Rich().;
        let _ = Famous().;
        let c = Happy() :- Rich(), Famous().;
        !((solve x <+> c) |= Happy().)

    @test
    def testNullary08(): Bool =
        let _ = Rich().;
        let y = Famous().;
        let c = Happy() :- Rich(), Famous().;
        !((solve y <+> c) |= Happy().)

    @test
    def testNullary09(): Bool =
        let x = Rich().;
        let y = Famous().;
        let c = Happy() :- Rich(), Famous().;
        (solve x <+> y <+> c) |= Happy().

}

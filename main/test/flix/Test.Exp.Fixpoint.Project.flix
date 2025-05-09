mod Test.Exp.Fixpoint.Project {

    @test
    def testProject01(): Bool =
        let p = #{ A(1). B(1). C(1). };
        let q = solve p project A;
        ((query q select x from A(x)) |> Vector.length) == 1

    @test
    def testProject02(): Bool =
        let p = #{ A(1). B(1). C(1). };
        let q = solve p project B;
        ((query q select x from B(x)) |> Vector.length) == 1

    @test
    def testProject03(): Bool =
        let p = #{ A(1). B(1). C(1). };
        let q = solve p project C;
        ((query q select x from C(x)) |> Vector.length) == 1

}

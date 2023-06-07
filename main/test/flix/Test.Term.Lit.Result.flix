mod Test.Term.Lit.Result {

    @test
    def testLitResult01(): Bool =
        let p = #{
            A(Ok(123): Result[Int32, Int32]).
            B(Ok(123): Result[Int32, Int32]).
            R(x) :- A(x), B(x).
        };
        let r = query p select x from R(x);
        Vector.memberOf(Ok(123), r)

    @test
    def testLitResult02(): Bool =
        let p = #{
            A(Ok("Hello World"): Result[Int32, String]).
            B(Ok("Hello World"): Result[Int32, String]).
            R(x) :- A(x), B(x).
        };
        let r = query p select x from R(x);
        Vector.memberOf(Ok("Hello World"), r)

    @test
    def testLitResult03(): Bool =
        let p = #{
            A(Ok(123)).
            B(Ok(123)).
            A(Err(456)).
            B(Err(456)).
            R(x) :- A(x), B(x).
        };
        let r = query p select x from R(x);
        Vector.memberOf(Ok(123), r)

}

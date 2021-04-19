namespace Test/Term/Lit/Result {

    @test
    def testLitResult01(): Bool & Impure =
        let p = #{
            A(Ok(123): Result[Int32, Int32]).
            B(Ok(123): Result[Int32, Int32]).
            R(x) :- A(x), B(x).
        };
        let r = query p select x from R(x);
        Array.memberOf(Ok(123), r)

    @test
    def testLitResult02(): Bool & Impure =
        let p = #{
            A(Ok("Hello World"): Result[String, Int32]).
            B(Ok("Hello World"): Result[String, Int32]).
            R(x) :- A(x), B(x).
        };
        let r = query p select x from R(x);
        Array.memberOf(Ok("Hello World"), r)

    @test
    def testLitResult03(): Bool & Impure =
        let p = #{
            A(Ok(123)).
            B(Ok(123)).
            A(Err(456)).
            B(Err(456)).
            R(x) :- A(x), B(x).
        };
        let r = query p select x from R(x);
        Array.memberOf(Ok(123), r)

}

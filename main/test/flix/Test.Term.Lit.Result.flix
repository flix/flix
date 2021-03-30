namespace Test/Term/Lit/Result {

    @test
    def testLitResult01(): Bool =
        let p = #{
            A(Ok(123): Result[Int32, Int32]).
            B(Ok(123): Result[Int32, Int32]).
            R(x) :- A(x), B(x).
        };
        (solve p) |= #{ R(Ok(123)). }

    @test
    def testLitResult02(): Bool =
        let p = #{
            A(Ok("Hello World"): Result[String, Int32]).
            B(Ok("Hello World"): Result[String, Int32]).
            R(x) :- A(x), B(x).
        };
        (solve p) |= #{ R(Ok("Hello World")). }

    @test
    def testLitResult03(): Bool =
        let p = #{
            A(Ok(123)).
            B(Ok(123)).
            A(Err(456)).
            B(Err(456)).
            R(x) :- A(x), B(x).
        };
        (solve p) |= #{ R(Ok(123)). }

}

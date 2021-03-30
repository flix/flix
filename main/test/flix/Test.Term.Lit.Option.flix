namespace Test/Term/Lit/Option {

    @test
    def testLitOption01(): Bool =
        let p = #{
            A(None: Option[Int32]).
            B(None: Option[Int32]).
            R(x) :- A(x), B(x).
        };
        (solve p) |= #{ R(None). }

    @test
    def testLitOption02(): Bool =
        let p = #{
            A(None: Option[String]).
            B(None: Option[String]).
            R(x) :- A(x), B(x).
        };
        (solve p) |= #{ R(None). }

    @test
    def testLitOption03(): Bool =
        let p = #{
            A(Some(123)).
            B(Some(123)).
            R(x) :- A(x), B(x).
        };
        (solve p) |= #{ R(Some(123)). }

    @test
    def testLitOption04(): Bool =
        let p = #{
            A(Some("Hello World")).
            B(Some("Hello World")).
            R(x) :- A(x), B(x).
        };
        (solve p) |= #{ R(Some("Hello World")). }

}

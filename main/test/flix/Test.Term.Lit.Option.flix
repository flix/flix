namespace Test/Term/Lit/Option {

    @test
    def testLitOption01(): Bool & Impure =
        let p = #{
            A(None: Option[Int32]).
            B(None: Option[Int32]).
            R(x) :- A(x), B(x).
        };
        let r = query p select x from R(x);
        Array.memberOf(None, r)

    @test
    def testLitOption02(): Bool & Impure =
        let p = #{
            A(None: Option[String]).
            B(None: Option[String]).
            R(x) :- A(x), B(x).
        };
        let r = query p select x from R(x);
        Array.memberOf(None, r)

    @test
    def testLitOption03(): Bool & Impure =
        let p = #{
            A(Some(123)).
            B(Some(123)).
            R(x) :- A(x), B(x).
        };
        let r = query p select x from R(x);
        Array.memberOf(Some(123), r)

    @test
    def testLitOption04(): Bool & Impure =
        let p = #{
            A(Some("Hello World")).
            B(Some("Hello World")).
            R(x) :- A(x), B(x).
        };
        let r = query p select x from R(x);
        Array.memberOf(Some("Hello World"), r)

}

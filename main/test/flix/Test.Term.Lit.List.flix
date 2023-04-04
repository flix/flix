mod Test.Term.Lit.List {

    @test
    def testLitList01(): Bool =
        let p = #{
            A(Nil: List[Int32]).
            B(Nil: List[Int32]).
            R(x) :- A(x), B(x).
        };
        let r = query p select x from R(x);
        List.memberOf(Nil, r)

    @test
    def testLitList02(): Bool =
        let p = #{
            A(Nil: List[String]).
            B(Nil: List[String]).
            R(x) :- A(x), B(x).
        };
        let r = query p select x from R(x);
        List.memberOf(Nil, r)

    @test
    def testLitList03(): Bool =
        let p = #{
            A(1 :: 2 :: Nil).
            B(1 :: 2 :: Nil).
            R(x) :- A(x), B(x).
        };
        let r = query p select x from R(x);
        List.memberOf(1 :: 2 :: Nil, r)

    @test
    def testLitList04(): Bool =
        let p = #{
            A("hello" :: "world" :: Nil).
            B("hello" :: "world" :: Nil).
            R(x) :- A(x), B(x).
        };
        let r = query p select x from R(x);
        List.memberOf("hello" :: "world" :: Nil, r)

}

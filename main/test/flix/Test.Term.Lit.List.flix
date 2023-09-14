mod Test.Term.Lit.List {

    @Test
    def testLitList01(): Bool =
        let p = #{
            A(Nil: List[Int32]).
            B(Nil: List[Int32]).
            R(x) :- A(x), B(x).
        };
        let r = query p select x from R(x);
        Vector.memberOf(Nil, r)

    @Test
    def testLitList02(): Bool =
        let p = #{
            A(Nil: List[String]).
            B(Nil: List[String]).
            R(x) :- A(x), B(x).
        };
        let r = query p select x from R(x);
        Vector.memberOf(Nil, r)

    @Test
    def testLitList03(): Bool =
        let p = #{
            A(1 :: 2 :: Nil).
            B(1 :: 2 :: Nil).
            R(x) :- A(x), B(x).
        };
        let r = query p select x from R(x);
        Vector.memberOf(1 :: 2 :: Nil, r)

    @Test
    def testLitList04(): Bool =
        let p = #{
            A("hello" :: "world" :: Nil).
            B("hello" :: "world" :: Nil).
            R(x) :- A(x), B(x).
        };
        let r = query p select x from R(x);
        Vector.memberOf("hello" :: "world" :: Nil, r)

}

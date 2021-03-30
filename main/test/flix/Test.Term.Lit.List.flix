namespace Test/Term/Lit/List {

    @test
    def testLitList01(): Bool =
        let p = #{
            A(Nil: List[Int32]).
            B(Nil: List[Int32]).
            R(x) :- A(x), B(x).
        };
        (solve p) |= #{ R(Nil). }

    @test
    def testLitList02(): Bool =
        let p = #{
            A(Nil: List[String]).
            B(Nil: List[String]).
            R(x) :- A(x), B(x).
        };
        (solve p) |= #{ R(Nil). }

    @test
    def testLitList03(): Bool =
        let p = #{
            A(1 :: 2 :: Nil).
            B(1 :: 2 :: Nil).
            R(x) :- A(x), B(x).
        };
        (solve p) |= #{ R(1 :: 2 :: Nil). }

    @test
    def testLitList04(): Bool =
        let p = #{
            A("hello" :: "world" :: Nil).
            B("hello" :: "world" :: Nil).
            R(x) :- A(x), B(x).
        };
        (solve p) |= #{ R("hello" :: "world" :: Nil). }

}

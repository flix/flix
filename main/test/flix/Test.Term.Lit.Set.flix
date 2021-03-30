namespace Test/Term/Lit/Set {

    @test
    def testLitSet01(): Bool =
        let p = #{
            A(Set#{}: Set[Int32]).
            B(Set#{}: Set[Int32]).
            R(x) :- A(x), B(x).
        };
        (solve p) |= #{ R(Set#{}). }

    @test
    def testLitSet02(): Bool =
        let p = #{
            A(Set#{1}).
            B(Set#{1}).
            R(x) :- A(x), B(x).
        };
        (solve p) |= #{ R(Set#{1}). }

    @test
    def testLitSet03(): Bool =
        let p = #{
            A(Set#{1, 2}).
            B(Set#{2, 1}).
            R(x) :- A(x), B(x).
        };
        (solve p) |= #{ R(Set#{1, 2}). }

    @test
    def testLitSet04(): Bool =
        let p = #{
            A(Set#{1, 2, 3}).
            B(Set#{3, 2, 1}).
            R(x) :- A(x), B(x).
        };
        (solve p) |= #{ R(Set#{1, 2, 3}). }

}

namespace Test/Term/Lit/Set {

    @test
    def testLitSet01(): Bool & Impure =
        let p = #{
            A(Set#{}: Set[Int32]).
            B(Set#{}: Set[Int32]).
            R(x) :- A(x), B(x).
        };
        let r = query p select x from R(x);
        Array.memberOf(Set#{}, r)

    @test
    def testLitSet02(): Bool & Impure =
        let p = #{
            A(Set#{1}).
            B(Set#{1}).
            R(x) :- A(x), B(x).
        };
        let r = query p select x from R(x);
        Array.memberOf(Set#{1}, r)

    @test
    def testLitSet03(): Bool & Impure =
        let p = #{
            A(Set#{1, 2}).
            B(Set#{2, 1}).
            R(x) :- A(x), B(x).
        };
        let r = query p select x from R(x);
        Array.memberOf(Set#{1, 2}, r)

    @test
    def testLitSet04(): Bool & Impure =
        let p = #{
            A(Set#{1, 2, 3}).
            B(Set#{3, 2, 1}).
            R(x) :- A(x), B(x).
        };
        let r = query p select x from R(x);
        Array.memberOf(Set#{1, 2, 3}, r)

}

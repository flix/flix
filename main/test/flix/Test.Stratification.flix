namespace Test/Stratification {

    @test
    def testStratification01(): #{ A(Int32), B(Int32), R(Int32) } = solve #{
        A(1). A(2). A(3).
        B(3).
        R(x) :- A(x), not B(x).
    }

    @test
    def testStratification02(): #{ A(Int32), B(Int32), R(Int32) } = solve #{
        A(1). A(2). A(3).
        B(3).
        R(x) :- not A(x), B(x).
    }

    @test
    def testStratification03(): #{ A(Int32), B(Int32), C(Int32), R(Int32) } = solve #{
        A(1). A(2). A(3).
        B(3).
        C(3).
        R(x) :- A(x), not B(x), not C(x).
    }

    @test
    def testStratification04(): #{ A(Int32), B(Int32), C(Int32), R(Int32) } = solve #{
        A(1). A(2). A(3).
        B(3).
        C(3).
        R(x) :- not A(x), B(x), not C(x).
    }

    @test
    def testStratification05(): #{ A(Int32), B(Int32), C(Int32), R(Int32) } = solve #{
        A(1). A(2).
        B(3).
        C(x) :- A(x).
        C(x) :- B(x).
        R(x) :- C(x), not A(x).
    }

    @test
    def testStratification06(): #{ A(Int32), B(Int32), C(Int32), D(Int32), R(Int32) } = solve #{
        A(1). A(2).
        B(3).
        C(x) :- A(x).
        C(x) :- B(x).
        D(x) :- C(x), not A(x).
        R(x) :- A(x), not D(x).
    }

    @test
    def testStratification07(): #{ A(Int32), B(Int32), C(Int32), D(Int32), E(Int32), R(Int32) } = solve #{
        A(1). A(2).
        B(3).
        C(x) :- A(x).
        C(x) :- B(x).
        D(x) :- C(x), not A(x).
        E(x) :- B(x), not D(x).
        R(x) :- E(x).
    }

    @test
    def testStratification08(): #{ A(Int32), N() } = solve #{
        A(1).
        A(2).
        A(3).
        N() :- not A(1).
        N() :- not A(2).
        N() :- not A(3).
    }

    @test
    def testStratification09(): #{ A(Int32), B(Int32), N() } = solve #{
        A(1). A(2).
        B(1). B(2).
        N() :- not A(1), not B(1).
        N() :- not A(2), not B(2).
    }

    @test
    def testStratification10(): Unit =
        // `_a` and `_b` will not be in the same stratification since they do not include
        // the `Diff_10` of the other.
        let _a: #{ A10(Int32), B10(Int32), DiffA10(Int32) } = #{
            A10(x) :- B10(x), DiffA10(_).
        };
        let _b: #{ B10(Int32), A10(Int32), DiffB10(Int32) } = #{
            B10(x) :- not A10(x), DiffB10(x).
        };
        ()

    @test
    def testStratification11(): Unit =
        // `_b` and `_c` will not be in the same stratification since they do not include
        // the `Diff_11` of the other.
        let _a: #{ A11(Int32), B11(Int32), C11(Int32) } = #{
            C11(12).
            A11(x) :- B11(x).
        };
        let _b: #{ A11(Int32), B11(Int32), C11(Int32), DiffB11() } = #{
            B11(x) :- not C11(x), A11(x), DiffB11().
        };
        let _c: #{ A11(Int32), B11(Int32), C11(Int32), DiffC11() } = #{
            B11(12).
            C11(x) :- A11(x), DiffC11().
        };
        ()

    @test
    def testStratification12(): Unit =
        // `_a` and `_b` will not be in the same stratification since they differ on the arity of `B12`.
        let _a: #{ A12(Int32), B12(Int32), C12(Int32) } = #{
            A12(x) :- B12(x), C12(x).
        };
        let _b: #{ B12(Int32, Int32), A12(Int32), C12(Int32) } = #{
            B12(x, 2) :- not A12(x), C12(x).
        };
        ()

    @test
    def testStratification13(): Unit =
        // `_a` and `_b` will not be in the same stratification since they differ on the arity of `C13`.
        let _a: #{ A13(Int32), B13(Int32), C13(Int32) } = #{
            A13(x) :- B13(x), C13(x).
        };
        let _b: #{ A13(Int32), B13(Int32), C13(Int32, Int32) }= #{
            B13(x) :- not A13(x), C13(x, 12).
        };
        ()

    @test
    def testStratification14(): Unit =
        // `_a` and `_b` will not be in the same stratification since they differ on the arity of `C14.
        let _a: #{ A14(Int32), B14(Int32), C14() } = #{
            A14(x) :- B14(x), C14().
        };
        let _b: #{ A14(Int32), B14(Int32), C14(Int32) } = #{
            B14(x) :- not A14(x), C14(x).
        };
        ()
}

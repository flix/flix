namespace Test/Stratification {

    //
    //
    // Non-recursive tests
    //
    //

    @test
    def test0NonRecursiveStratification00(): #{ A000(Int32), B000(Int32), R000(Int32) } = solve #{
        A000(1). A000(2). A000(3).
        B000(3).
        R000(x) :- A000(x), not B000(x).
    }

    @test
    def test0NonRecursiveStratification01(): #{ A001(Int32), B001(Int32), R001(Int32) } = solve #{
        A001(1). A001(2). A001(3).
        B001(3).
        R001(x) :- not A001(x), B001(x).
    }

    @test
    def test0NonRecursiveStratification02(): #{ A002(Int32), B002(Int32), C002(Int32), R002(Int32) } = solve #{
        A002(1). A002(2). A002(3).
        B002(3).
        C002(3).
        R002(x) :- A002(x), not B002(x), not C002(x).
    }

    @test
    def test0NonRecursiveStratification03(): #{ A003(Int32), B003(Int32), C003(Int32), R003(Int32) } = solve #{
        A003(1). A003(2). A003(3).
        B003(3).
        C003(3).
        R003(x) :- not A003(x), B003(x), not C003(x).
    }

    @test
    def test0NonRecursiveStratification04(): #{ A004(Int32), B004(Int32), C004(Int32), R004(Int32) } = solve #{
        A004(1). A004(2).
        B004(3).
        C004(x) :- A004(x).
        C004(x) :- B004(x).
        R004(x) :- C004(x), not A004(x).
    }

    @test
    def test0NonRecursiveStratification05(): #{ A005(Int32), B005(Int32), C005(Int32), D005(Int32), R005(Int32) } = solve #{
        A005(1). A005(2).
        B005(3).
        C005(x) :- A005(x).
        C005(x) :- B005(x).
        D005(x) :- C005(x), not A005(x).
        R005(x) :- A005(x), not D005(x).
    }

    @test
    def test0NonRecursiveStratification06(): #{ A006(Int32), B006(Int32), C006(Int32), D006(Int32), E006(Int32), R006(Int32) } = solve #{
        A006(1). A006(2).
        B006(3).
        C006(x) :- A006(x).
        C006(x) :- B006(x).
        D006(x) :- C006(x), not A006(x).
        E006(x) :- B006(x), not D006(x).
        R006(x) :- E006(x).
    }

    @test
    def test0NonRecursiveStratification07(): #{ A007(Int32), N007() } = solve #{
        A007(1).
        A007(2).
        A007(3).
        N007() :- not A007(1).
        N007() :- not A007(2).
        N007() :- not A007(3).
    }

    @test
    def test0NonRecursiveStratification08(): #{ A008(Int32), B008(Int32), N008() } = solve #{
        A008(1). A008(2).
        B008(1). B008(2).
        N008() :- not A008(1), not B008(1).
        N008() :- not A008(2), not B008(2).
    }

    //
    //
    // Rule based filtering
    //
    //

    @test
    def test1RuleFilteredStratification00(): Bool =
        let p1 = #{
            B100(12). DiffA100(12).
            A100(x) :- B100(x), DiffA100(_).
        };
        let p2 = #{
            A100(1). DiffB100(1).  DiffB100(2).
            B100(x) :- not A100(x), DiffB100(x).
        };
        (query p1 select x from A100(x) |> Array.length) +
        (query p2 select x from B100(x) |> Array.length)
        > 0


    @test
    def test1RuleFilteredStratification01(): Bool =
        let p1 = #{
            C101(12). B101(13).
            A101(x) :- B101(x).
        };
        let p2 = #{
            A101(101).
            B101(x) :- not C101(x), A101(x), DiffB101().
        };
        let p3 = #{
            B101(12). A101(101).
            C101(x) :- A101(x), DiffC101().
        };
        (query p1 select x from A101(x) |> Array.length) +
        (query p2 select x from B101(x) |> Array.length) +
        (query p3 select x from C101(x) |> Array.length)
        > 0

    //
    //
    // Arity based filtering
    //
    //

    @test
    def test2ArityFilteredStratification00(): Bool =
        let p1 = #{
            B200(17). C200(17).
            A200(x) :- B200(x), C200(x).
        };
        let p2 = #{
            C200(12).
            B200(x, 2) :- not A200(x), C200(x).
        };
        (query p1 select x from A200(x) |> Array.length) +
        (query p2 select x from B200(x, _) |> Array.length)
        > 0

    @test
    def test2ArityFilteredStratification01(): Bool =
        let p1 = #{
            B201(13). C201(13).
            A201(x) :- B201(x), C201(x).
        };
        let p2 = #{
            C201(14, 12).
            B201(x) :- not A201(x), C201(x, 12).
        };
        (query p1 select x from A201(x) |> Array.length) +
        (query p2 select x from B201(x) |> Array.length)
        > 0

    @test
    def test2ArityFilteredStratification02(): Bool =
        let p1 = #{
            B202("s").
            A202(x) :- B202(x), C202().
        };
        let p2 = #{
            C202("a").
            B202(x) :- not A202(x), C202(x).
        };
        (query p1 select x from A202(x) |> Array.length) +
        (query p2 select x from B202(x) |> Array.length)
        > 0
    
    // combined with rule based filtering
    @test
    def test2ArityFilteredStratification99(): Bool =
        let p1 = #{
            B203(12). Diff203("string").
            A203(x) :- B203(x), P1Diff203("string").
        };
        let p2 = #{
            C203(13). P2Diff203(13).
            B203(x) :- not A203(x), P2Diff203(x).
        };
        (query p1 select x from A203(x) |> Array.length) +
        (query p2 select x from B203(x) |> Array.length)
        > 0

    //
    //
    // Term-type based filtering
    //
    //

    // combined with rule based filtering


    //
    //
    // Denotation based filtering
    //
    //
    
    // combined with rule based filtering
}

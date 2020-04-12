namespace Test/Stratification {

    @test
    def testStratification01(): #{ A(Int), B(Int), R(Int) } = solve #{
        A(1). A(2). A(3).
        B(3).
        R(x) :- A(x), not B(x).
    }

    @test
    def testStratification02(): #{ A(Int), B(Int), R(Int) } = solve #{
        A(1). A(2). A(3).
        B(3).
        R(x) :- not A(x), B(x).
    }

    @test
    def testStratification03(): #{ A(Int), B(Int), C(Int), R(Int) } = solve #{
        A(1). A(2). A(3).
        B(3).
        C(3).
        R(x) :- A(x), not B(x), not C(x).
    }

    @test
    def testStratification04(): #{ A(Int), B(Int), C(Int), R(Int) } = solve #{
        A(1). A(2). A(3).
        B(3).
        C(3).
        R(x) :- not A(x), B(x), not C(x).
    }

    @test
    def testStratification05(): #{ A(Int), B(Int), C(Int), R(Int) } = solve #{
        A(1). A(2).
        B(3).
        C(x) :- A(x).
        C(x) :- B(x).
        R(x) :- C(x), not A(x).
    }

    @test
    def testStratification06(): #{ A(Int), B(Int), C(Int), D(Int), R(Int) } = solve #{
        A(1). A(2).
        B(3).
        C(x) :- A(x).
        C(x) :- B(x).
        D(x) :- C(x), not A(x).
        R(x) :- A(x), not D(x).
    }

    @test
    def testStratification07(): #{ A(Int), B(Int), C(Int), D(Int), E(Int), R(Int) } = solve #{
        A(1). A(2).
        B(3).
        C(x) :- A(x).
        C(x) :- B(x).
        D(x) :- C(x), not A(x).
        E(x) :- B(x), not D(x).
        R(x) :- E(x).
    }

    @test
    def testStratification08(): #{ A(Int), N() } = solve #{
        A(1).
        A(2).
        A(3).
        N() :- not A(1).
        N() :- not A(2).
        N() :- not A(3).
    }

    @test
    def testStratification09(): #{ A(Int), B(Int), N() } = solve #{
        A(1). A(2).
        B(1).  B(2).
        N() :- not A(1), not B(1).
        N() :- not A(2), not B(2).
    }

}

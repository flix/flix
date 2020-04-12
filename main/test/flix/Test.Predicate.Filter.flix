namespace Test/Predicate/Filter {

    @test
    def testFilter01(): #{ R(String) } = solve {
        R("a") :- success().
    }

    @test
    def testFilter02(): #{ R(String) } = solve {
        R("a") :- failure().
    }

    @test
    def testFilter03(): #{ R(String) } = solve {
        R("a") :- isZero(0).
    }

    @test
    def testFilter04(): #{ R(String) } = solve {
        R("b") :- isZero(1).
    }

    @test
    def testFilter05(): #{ R(String) } = solve {
        R("c") :- isZero(2).
    }

    @test
    def testFilter06(): #{ R(String) } = solve #{
        R("a") :- isZero(0).
        R("b") :- isZero(1).
        R("c") :- isZero(2).
    }

    @test
    def testFilter07(): #{ R(String) } = solve #{
        R("a") :- isZero(0), isZero(1), isZero(2).
    }

    @test
    def testFilter08(): #{ A(Int), R(String) } = solve #{
        A(0). A(1). A(2).
        R("a") :- isZero(x), A(x).
    }

    @test
    def testFilter09(): #{ A(Int), B(Int), R(String) } = solve #{
        A(0). A(1). A(2).
        B(0). B(1). B(2).
        R("a") :- isSame(x, y), A(x), B(y).
    }

    @test
    def testFilter10(): #{ A(Int), B(Int), R(String) } = solve #{
        A(0). A(1). A(2).
        B(0). B(1). B(2).
        R("a") :- isZero(x), isZero(y), isSame(x, y), A(x), B(y).
    }

    @test
    def testFilter11(): #{ R(String) } = solve #{
        R("a") :- isZero(increment(0)).
    }

    @test
    def testFilter12(): #{ R(String) } = solve #{
        R("a") :- isZero(increment(decrement(0))).
    }

    @test
    def testFilter13(): #{ R(String) } = solve #{
        R("a") :- isZero(increment(decrement(decrement(0)))).
    }

    @test
    def testFilter14(): #{ A(Int), R(String) } = solve #{
        A(0). A(1). A(2).
        R("a") :- isZero(increment(decrement(x))), A(x).
    }

    @test
    def testFilter15(): #{ A(Int), B(Int), R(String) } = solve #{
        A(0). A(1). A(2).
        R("a") :- isSame(increment(x), increment(y)), A(x), B(y).
    }

    def success(): Bool = true
    def failure(): Bool = false
    def isZero(x: Int): Bool = x == 0
    def isSame(x: Int, y: Int): Bool = x == y
    def increment(x: Int): Int = x + 1
    def decrement(x: Int): Int = x - 1

}

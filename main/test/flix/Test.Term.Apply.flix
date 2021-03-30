namespace Test/Term/Apply {

    @test
    def testTermApply01(): #{ R(Int) } = solve #{
        R(zer()).
        R(one()).
        R(two()).
    }

    @test
    def testTermApply02(): #{ R(Int) } = solve #{
        R(inc(1)).
        R(dec(2)).
    }

    @test
    def testTermApply03(): #{ R(Int) } = solve #{
        R(inc(dec(1))).
        R(dec(inc(2))).
    }

    @test
    def testTermApply04(): #{ A(Int), B(Int), R(Int) } = solve #{
        A(0). A(1). A(3).
        B(1). B(2). B(3).

        R(inc(x)) :- A(x).
        R(dec(x)) :- B(x).
    }

    @test
    def testTermApply05(): #{ A(Int), B(Int), R(Int) } = solve #{
        A(0). A(1). A(3).
        B(1). B(2). B(3).

        R(inc(dec(x))) :- A(x), B(x).
    }

    @test
    def testTermApply06(): #{ A(Int), B(Int), R(Int) } = solve #{
        A(0). A(1). A(3).
        B(1). B(2). B(3).

        R(add(x, y)) :- A(x), B(y).
    }

    @test
    def testTermApply07(): #{ A(Int), B(Int), R(Int) } = solve #{
        A(0). A(1). A(3).
        B(1). B(2). B(3).

        R(add(x)(y)) :- A(x), B(y).
    }

    @test
    def testTermApply08(): #{ A(Int), B(Int), R(Int) } = solve #{
        A(0). A(1). A(3).
        B(1). B(2). B(3).

        R(add(inc(x), dec(y))) :- A(x), B(y).
    }

    @test
    def testTermApply09(): #{ R(Int) } = solve #{
        R(identity(42)).
    }

    @test
    def testTermApply10(): #{ R(Int) } = solve #{
        R(fst((1, 2))).
        R(snd((1, 2))).
    }

    @test
    def testTermApply11(): Bool =
        let f = zer;
        let m = solve #{
            R(f()).
        };
        m |= R(0).

    @test
    def testTermApply12(): Bool =
        let f = one;
        let m = solve #{
            R(f()).
        };
        m |= R(1).

    @test
    def testTermApply13(): Bool =
        let f = two;
        let m = solve #{
            R(f()).
        };
        m |= R(2).

    @test
    def testTermApply14(): Bool =
        let f = inc;
        let m = solve #{
            A(1). A(2). A(3).
            R(f(x)) :- A(x).
        };
        m |= R(4).

    @test
    def testTermApply15(): Bool =
        let f = dec;
        let m = solve #{
            A(1). A(2). A(3).
            R(f(x)) :- A(x).
        };
        m |= R(0).

    @test
    def testTermApply16(): Bool =
        let f = add;
        let m = solve #{
            A(1). A(2). A(3).
            R(f(x, y)) :- A(x), A(y).
        };
        m |= R(6).

    @test
    def testTermApply17(): Bool =
        let m = solve ho1(inc);
        m |= R(4).

    @test
    def testTermApply18(): Bool =
        let m = solve ho1(dec);
        m |= R(0).

    @test
    def testTermApply19(): Bool =
        let m = solve ho2(add);
        m |= R(6).

    @test
    def testTermApply20(): Bool =
        let m = solve ho3(inc);
        m |= P(4).

    @test
    def testTermApply21(): Bool =
        let m = solve ho3(dec);
        m |= P(0).

    @test
    def testTermApply22(): Bool =
        let m = solve ho3(_ -> true);
        m |= P(true).

    @test
    def testTermApply23(): Bool =
        let m = solve ho3(_ -> "hello");
        m |= P("hello").

    pub def zer(): Int = 0
    pub def one(): Int = 1
    pub def two(): Int = 2
    pub def inc(x: Int): Int = x + 1
    pub def dec(x: Int): Int = x - 1
    pub def add(x: Int, y: Int): Int = x + y

    pub def identity(x: a): a = x
    pub def fst(p: (a, b)): a = let (x, _) = p; x
    pub def snd(p: (a, b)): b = let (_, y) = p; y

    pub def ho1(f: Int -> Int): #{A(Int), R(Int)} = #{
        A(1). A(2). A(3).
        R(f(x)) :- A(x), A(y).
    }

    pub def ho2(f: (Int, Int) -> Int): #{A(Int), R(Int)} = #{
        A(1). A(2). A(3).
        R(f(x, y)) :- A(x), A(y).
    }

    pub def ho3[a](f: Int -> a): #{A(Int), P(a)} with Boxable[a] = #{
        A(1). A(2). A(3).
        P(f(x)) :- A(x).
    }

}

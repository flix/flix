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
    def testTermApply11(): Bool & Impure =
        let f = zer;
        let p = #{
            R(f()).
        };
        let m = query p select x from R(x);
        Array.memberOf(0, m)

    @test
    def testTermApply12(): Bool & Impure =
        let f = one;
        let p = #{
            R(f()).
        };
        let m = query p select x from R(x);
        Array.memberOf(1, m)

    @test
    def testTermApply13(): Bool & Impure =
        let f = two;
        let p = solve #{
            R(f()).
        };
        let m = query p select x from R(x);
        Array.memberOf(2, m)

    @test
    def testTermApply14(): Bool & Impure =
        let f = inc;
        let p = #{
            A(1). A(2). A(3).
            R(f(x)) :- A(x).
        };
        let m = query p select x from R(x);
        Array.memberOf(4, m)

    @test
    def testTermApply15(): Bool & Impure =
        let f = dec;
        let p = #{
            A(1). A(2). A(3).
            R(f(x)) :- A(x).
        };
        let m = query p select x from R(x);
        Array.memberOf(0, m)

    @test
    def testTermApply16(): Bool & Impure =
        let f = add;
        let p = solve #{
            A(1). A(2). A(3).
            R(f(x, y)) :- A(x), A(y).
        };
        let m = query p select x from R(x);
        Array.memberOf(6, m)

    @test
    def testTermApply17(): Bool & Impure =
        let m = query ho1(inc) select x from R(x);
        Array.memberOf(4, m)

    @test
    def testTermApply18(): Bool & Impure =
        let m = query ho1(dec) select x from R(x);
        Array.memberOf(0, m)

    @test
    def testTermApply19(): Bool & Impure =
        let m = query ho2(add) select x from R(x);
        Array.memberOf(6, m)

    @test
    def testTermApply20(): Bool & Impure =
        let m = query ho3(inc) select x from P(x);
        Array.memberOf(4, m)

    @test
    def testTermApply21(): Bool & Impure =
        let m = query ho3(dec) select x from P(x);
        Array.memberOf(0, m)

    @test
    def testTermApply22(): Bool & Impure =
        let m = query ho3(_ -> true) select x from P(x);
        Array.memberOf(true, m)

    @test
    def testTermApply23(): Bool & Impure =
        let m = query ho3(_ -> "hello") select x from P(x);
        Array.memberOf("hello", m)

    pub def zer(): Int = 0
    pub def one(): Int = 1
    pub def two(): Int = 2
    pub def inc(x: Int): Int = x + 1
    pub def dec(x: Int): Int = x - 1
    pub def add(x: Int, y: Int): Int = x + y

    pub def identity(x: a): a = x
    pub def fst(p: (a, b)): a = let (x, _) = p; x
    pub def snd(p: (a, b)): b = let (_, y) = p; y

    pub def ho1(f: Int -> Int): #{A(Int), R(Int) | r} = #{
        A(1). A(2). A(3).
        R(f(x)) :- A(x), A(y).
    }

    pub def ho2(f: (Int, Int) -> Int): #{A(Int), R(Int) | r} = #{
        A(1). A(2). A(3).
        R(f(x, y)) :- A(x), A(y).
    }

    pub def ho3(f: Int -> a): #{A(Int), P(a) | r} with Boxable[a] = #{
        A(1). A(2). A(3).
        P(f(x)) :- A(x).
    }

}

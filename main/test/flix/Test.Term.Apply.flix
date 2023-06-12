mod Test.Term.Apply {

    @test
    def testTermApply01(): #{ R(Int32) } = solve #{
        R(zer()).
        R(one()).
        R(two()).
    }

    @test
    def testTermApply02(): #{ R(Int32) } = solve #{
        R(inc(1)).
        R(dec(2)).
    }

    @test
    def testTermApply03(): #{ R(Int32) } = solve #{
        R(inc(dec(1))).
        R(dec(inc(2))).
    }

    @test
    def testTermApply04(): #{ A(Int32), B(Int32), R(Int32) } = solve #{
        A(0). A(1). A(3).
        B(1). B(2). B(3).

        R(inc(x)) :- A(x).
        R(dec(x)) :- B(x).
    }

    @test
    def testTermApply05(): #{ A(Int32), B(Int32), R(Int32) } = solve #{
        A(0). A(1). A(3).
        B(1). B(2). B(3).

        R(inc(dec(x))) :- A(x), B(x).
    }

    @test
    def testTermApply06(): #{ A(Int32), B(Int32), R(Int32) } = solve #{
        A(0). A(1). A(3).
        B(1). B(2). B(3).

        R(add(x, y)) :- A(x), B(y).
    }

    @test
    def testTermApply07(): #{ A(Int32), B(Int32), R(Int32) } = solve #{
        A(0). A(1). A(3).
        B(1). B(2). B(3).

        R(add(x)(y)) :- A(x), B(y).
    }

    @test
    def testTermApply08(): #{ A(Int32), B(Int32), R(Int32) } = solve #{
        A(0). A(1). A(3).
        B(1). B(2). B(3).

        R(add(inc(x), dec(y))) :- A(x), B(y).
    }

    @test
    def testTermApply09(): #{ R(Int32) } = solve #{
        R(identity(42)).
    }

    @test
    def testTermApply10(): #{ R(Int32) } = solve #{
        R(fst((1, 2))).
        R(snd((1, 2))).
    }

    @test
    def testTermApply11(): Bool =
        let f = zer;
        let p = #{
            R(f()).
        };
        let m = query p select x from R(x);
        Vector.memberOf(0, m)

    @test
    def testTermApply12(): Bool =
        let f = one;
        let p = #{
            R(f()).
        };
        let m = query p select x from R(x);
        Vector.memberOf(1, m)

    @test
    def testTermApply13(): Bool =
        let f = two;
        let p = solve #{
            R(f()).
        };
        let m = query p select x from R(x);
        Vector.memberOf(2, m)

    @test
    def testTermApply14(): Bool =
        let f = inc;
        let p = #{
            A(1). A(2). A(3).
            R(f(x)) :- A(x).
        };
        let m = query p select x from R(x);
        Vector.memberOf(4, m)

    @test
    def testTermApply15(): Bool =
        let f = dec;
        let p = #{
            A(1). A(2). A(3).
            R(f(x)) :- A(x).
        };
        let m = query p select x from R(x);
        Vector.memberOf(0, m)

    @test
    def testTermApply16(): Bool =
        let f = add;
        let p = solve #{
            A(1). A(2). A(3).
            R(f(x, y)) :- A(x), A(y).
        };
        let m = query p select x from R(x);
        Vector.memberOf(6, m)

    @test
    def testTermApply17(): Bool =
        let m = query ho1(inc) select x from R(x);
        Vector.memberOf(4, m)

    @test
    def testTermApply18(): Bool =
        let m = query ho1(dec) select x from R(x);
        Vector.memberOf(0, m)

    @test
    def testTermApply19(): Bool =
        let m = query ho2(add) select x from R(x);
        Vector.memberOf(6, m)

    @test
    def testTermApply20(): Bool =
        let m = query ho3(inc) select x from P(x);
        Vector.memberOf(4, m)

    @test
    def testTermApply21(): Bool =
        let m = query ho3(dec) select x from P(x);
        Vector.memberOf(0, m)

    @test
    def testTermApply22(): Bool =
        let m = query ho3(_ -> true) select x from P(x);
        Vector.memberOf(true, m)

    @test
    def testTermApply23(): Bool =
        let m = query ho3(_ -> "hello") select x from P(x);
        Vector.memberOf("hello", m)

    pub def zer(): Int32 = 0
    pub def one(): Int32 = 1
    pub def two(): Int32 = 2
    pub def inc(x: Int32): Int32 = x + 1
    pub def dec(x: Int32): Int32 = x - 1
    pub def add(x: Int32, y: Int32): Int32 = x + y

    pub def identity(x: a): a = x
    pub def fst(p: (a, b)): a = let (x, _) = p; x
    pub def snd(p: (a, b)): b = let (_, y) = p; y

    pub def ho1(f: Int32 -> Int32): #{A(Int32), R(Int32) | r} = #{
        A(1). A(2). A(3).
        R(f(x)) :- A(x), A(_y).
    }

    pub def ho2(f: (Int32, Int32) -> Int32): #{A(Int32), R(Int32) | r} = #{
        A(1). A(2). A(3).
        R(f(x, y)) :- A(x), A(y).
    }

    pub def ho3(f: Int32 -> a): #{A(Int32), P(a) | r} with Order[a] = #{
        A(1). A(2). A(3).
        P(f(x)) :- A(x).
    }

}

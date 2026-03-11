mod Test.Term.Apply {

    use Assert.assertMemberOf

    @Test
    def testTermApply01(): Unit \ Assert =
        let p = solve #{
            R(zer()).
            R(one()).
            R(two()).
        };
        let m = query p select x from R(x);
        assertMemberOf(0, m);
        assertMemberOf(1, m);
        assertMemberOf(2, m)

    @Test
    def testTermApply02(): Unit \ Assert =
        let p = solve #{
            R(inc(1)).
            R(dec(2)).
        };
        let m = query p select x from R(x);
        assertMemberOf(2, m);
        assertMemberOf(1, m)

    @Test
    def testTermApply03(): Unit \ Assert =
        let p = solve #{
            R(inc(dec(1))).
            R(dec(inc(2))).
        };
        let m = query p select x from R(x);
        assertMemberOf(1, m);
        assertMemberOf(2, m)

    @Test
    def testTermApply04(): Unit \ Assert =
        let p = solve #{
            A(0). A(1). A(3).
            B(1). B(2). B(3).

            R(inc(x)) :- A(x).
            R(dec(x)) :- B(x).
        };
        let m = query p select x from R(x);
        assertMemberOf(1, m);
        assertMemberOf(2, m);
        assertMemberOf(4, m);
        assertMemberOf(0, m)

    @Test
    def testTermApply05(): Unit \ Assert =
        let p = solve #{
            A(0). A(1). A(3).
            B(1). B(2). B(3).

            R(inc(dec(x))) :- A(x), B(x).
        };
        let m = query p select x from R(x);
        assertMemberOf(1, m);
        assertMemberOf(3, m)

    @Test
    def testTermApply06(): Unit \ Assert =
        let p = solve #{
            A(0). A(1). A(3).
            B(1). B(2). B(3).

            R(add(x, y)) :- A(x), B(y).
        };
        let m = query p select x from R(x);
        assertMemberOf(4, m)

    @Test
    def testTermApply07(): Unit \ Assert =
        let p = solve #{
            A(0). A(1). A(3).
            B(1). B(2). B(3).

            R(add(x)(y)) :- A(x), B(y).
        };
        let m = query p select x from R(x);
        assertMemberOf(4, m)

    @Test
    def testTermApply08(): Unit \ Assert =
        let p = solve #{
            A(0). A(1). A(3).
            B(1). B(2). B(3).

            R(add(inc(x), dec(y))) :- A(x), B(y).
        };
        let m = query p select x from R(x);
        assertMemberOf(1, m)

    @Test
    def testTermApply09(): Unit \ Assert =
        let p = solve #{
            R(identity(42)).
        };
        let m = query p select x from R(x);
        assertMemberOf(42, m)

    @Test
    def testTermApply10(): Unit \ Assert =
        let p = solve #{
            R(fst((1, 2))).
            R(snd((1, 2))).
        };
        let m = query p select x from R(x);
        assertMemberOf(1, m);
        assertMemberOf(2, m)

    @Test
    def testTermApply11(): Unit \ Assert =
        let f = zer;
        let p = #{
            R(f()).
        };
        let m = query p select x from R(x);
        assertMemberOf(0, m)

    @Test
    def testTermApply12(): Unit \ Assert =
        let f = one;
        let p = #{
            R(f()).
        };
        let m = query p select x from R(x);
        assertMemberOf(1, m)

    @Test
    def testTermApply13(): Unit \ Assert =
        let f = two;
        let p = solve #{
            R(f()).
        };
        let m = query p select x from R(x);
        assertMemberOf(2, m)

    @Test
    def testTermApply14(): Unit \ Assert =
        let f = inc;
        let p = #{
            A(1). A(2). A(3).
            R(f(x)) :- A(x).
        };
        let m = query p select x from R(x);
        assertMemberOf(4, m)

    @Test
    def testTermApply15(): Unit \ Assert =
        let f = dec;
        let p = #{
            A(1). A(2). A(3).
            R(f(x)) :- A(x).
        };
        let m = query p select x from R(x);
        assertMemberOf(0, m)

    @Test
    def testTermApply16(): Unit \ Assert =
        let f = add;
        let p = solve #{
            A(1). A(2). A(3).
            R(f(x, y)) :- A(x), A(y).
        };
        let m = query p select x from R(x);
        assertMemberOf(6, m)

    @Test
    def testTermApply17(): Unit \ Assert =
        let m = query ho1(inc) select x from R(x);
        assertMemberOf(4, m)

    @Test
    def testTermApply18(): Unit \ Assert =
        let m = query ho1(dec) select x from R(x);
        assertMemberOf(0, m)

    @Test
    def testTermApply19(): Unit \ Assert =
        let m = query ho2(add) select x from R(x);
        assertMemberOf(6, m)

    @Test
    def testTermApply20(): Unit \ Assert =
        let m = query ho3(inc) select x from P(x);
        assertMemberOf(4, m)

    @Test
    def testTermApply21(): Unit \ Assert =
        let m = query ho3(dec) select x from P(x);
        assertMemberOf(0, m)

    @Test
    def testTermApply22(): Unit \ Assert =
        let m = query ho3(_ -> true) select x from P(x);
        assertMemberOf(true, m)

    @Test
    def testTermApply23(): Unit \ Assert =
        let m = query ho3(_ -> "hello") select x from P(x);
        assertMemberOf("hello", m)

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

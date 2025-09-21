mod Test.Exp.Fixpoint.Application {

    use Assert.assertEq;

    /////////////////////////////////////////////////////////////////////////////
    // Tests with application of 1 argument                                    //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testApplicationOne01(): Unit \ Assert = {
        let tuples = Vector.range(0, 50);
        let f = inject tuples into P/1;
        let r = #{
            R(x, x + 1) :- P(x).
        };
        let expected = tuples |> Vector.map(x -> (x, x + 1));
        let actual = query f, r select (x, y) from R(x, y);
        assertEq(expected = expected, actual)
    }

    @Test
    def testApplicationOne02(): Unit \ Assert = {
        let n = 50;
        let r = #{
            Succ(0).
            Succ(x + 1) :- Succ(x), if(x < n).
        };
        let expected = Vector.range(0, n + 1);
        let actual = query r select x from Succ(x);
        assertEq(expected = expected, actual)
    }

    @Test
    def testApplicationOne03(): Unit \ Assert = {
        let g = x -> {
            let fizz = if (Int32.modulo(x, 3) == 0) "Fizz" else "";
            let buzz = if (Int32.modulo(x, 5) == 0) "Buzz" else "";
            let out = fizz + buzz;
            if (out == "")
                Int32.toString(x)
            else
                out
        };
        let tuples = Vector.range(1, 21);
        let f = inject tuples into Nat/1;
        let r = #{
            FizzBuzz(x, g(x)) :- Nat(x).
        };
        let expected = Vector#{
            "1", "2", "Fizz", "4", "Buzz", "Fizz", "7", "8", "Fizz", "Buzz",
            "11", "Fizz", "13", "14", "FizzBuzz", "16", "17", "Fizz", "19", "Buzz"
        };
        let actual = query f, r select (x, y) from FizzBuzz(x, y) |> Vector.unzip |> snd;
        assertEq(expected = expected, actual)

    }

    @Test
    def testApplicationOne04(): Unit \ Assert = {
        let tuples = Vector.range(0, 50);
        let f = inject tuples into P/1;
        let r = #{
            R(x + 1, x) :- P(x).
        };
        let expected = tuples |> Vector.map(x -> (x + 1, x));
        let actual = query f, r select (x, y) from R(x, y);
        assertEq(expected = expected, actual)
    }

    @Test
    def testApplicationOne05(): Unit \ Assert = {
        let tuples = Vector.range(0, 50);
        let f = inject tuples into P/1;
        let r = #{
            R(x + 1, x + 1) :- P(x).
        };
        let expected = tuples |> Vector.map(x -> (x + 1, x + 1));
        let actual = query f, r select (x, y) from R(x, y);
        assertEq(expected = expected, actual)
    }

    /////////////////////////////////////////////////////////////////////////////
    // Tests with application of 2 arguments                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testApplicationTwo01(): Unit \ Assert = {
        let tuples = Vector.range(0, 50);
        let f = inject tuples into P/1;
        let r = #{
            Q(1).
            R(x, x + y) :- P(x), Q(y).
        };
        let expected = tuples |> Vector.map(x -> (x, x + 1));
        let actual = query f, r select (x, y) from R(x, y);
        assertEq(expected = expected, actual)
    }

    @Test
    def testApplicationTwo02(): Unit \ Assert = {
        let n = 20;
        let r = #{
            Two(2).
            SuccTwo(0).
            SuccTwo(x + y) :- SuccTwo(x), Two(y), if(x < n).
        };
        let expected = Vector#{0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20};
        let actual = query r select x from SuccTwo(x);
        assertEq(expected = expected, actual)
    }

    @Test
    def testApplicationTwo03(): Unit \ Assert = {
        let tuples = Vector.range(0, 50) |> Vector.map(Int32.toString);
        let f = inject tuples into P/1;
        let r = #{
            Q("1").
            R(a, a + b) :- P(a), Q(b).
        };
        let expected = tuples |> Vector.map(x -> (x, x + "1")) |> Vector.sort;
        let actual = query f, r select (x, y) from R(x, y);
        assertEq(expected = expected, actual)
    }

    @Test
    def testApplicationTwo04(): Unit \ Assert = {
        let tuples = Vector.range(0, 50);
        let f = inject tuples into P/1;
        let r = #{
            Q(1).
            R(x + y, x) :- P(x), Q(y).
        };
        let expected = tuples |> Vector.map(x -> (x + 1, x));
        let actual = query f, r select (x, y) from R(x, y);
        assertEq(expected = expected, actual)
    }

    @Test
    def testApplicationTwo05(): Unit \ Assert = {
        let tuples = Vector.range(0, 50);
        let f = inject tuples into P/1;
        let r = #{
            Q(1).
            R(x + y, x + y) :- P(x), Q(y).
        };
        let expected = tuples |> Vector.map(x -> (x + 1, x + 1));
        let actual = query f, r select (x, y) from R(x, y);
        assertEq(expected = expected, actual)
    }

    /////////////////////////////////////////////////////////////////////////////
    // Tests with application of 3 arguments                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testApplicationThree01(): Unit \ Assert = {
        let tuples = Vector.range(0, 50);
        let f = inject tuples into P/1;
        let r = #{
            Q(1).
            S(2).
            R(x, x + y + z) :- P(x), Q(y), S(z).
        };
        let expected = tuples |> Vector.map(x -> (x, x + 1 + 2));
        let actual = query f, r select (x, y) from R(x, y);
        assertEq(expected = expected, actual)
    }

    @Test
    def testApplicationThree02(): Unit \ Assert = {
        let tuples = Vector.range(0, 50) |> Vector.map(Int32.toString);
        let f = inject tuples into P/1;
        let r = #{
            Q("1").
            S("2").
            R(a, a + b + c) :- P(a), Q(b), S(c).
        };
        let expected = tuples |> Vector.map(x -> (x, x + "1" + "2")) |> Vector.sort;
        let actual = query f, r select (x, y) from R(x, y);
        assertEq(expected = expected, actual)
    }

    @Test
    def testApplicationThree03(): Unit \ Assert = {
        let tuples = Vector.range(0, 50);
        let f = inject tuples into P/1;
        let r = #{
            Q(1).
            S(2).
            R(x + y + z, x) :- P(x), Q(y), S(z).
        };
        let expected = tuples |> Vector.map(x -> (x + 1 + 2, x));
        let actual = query f, r select (x, y) from R(x, y);
        assertEq(expected = expected, actual)
    }

    @Test
    def testApplicationThree04(): Unit \ Assert = {
        let tuples = Vector.range(0, 50);
        let f = inject tuples into P/1;
        let r = #{
            Q(1).
            S(2).
            R(x + y + z, x + y + z) :- P(x), Q(y), S(z).
        };
        let expected = tuples |> Vector.map(x -> (x + 1 + 2, x + 1 + 2));
        let actual = query f, r select (x, y) from R(x, y);
        assertEq(expected = expected, actual)
    }

    /////////////////////////////////////////////////////////////////////////////
    // Tests with application of 4 arguments                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testApplicationFour01(): Unit \ Assert = {
        let tuples = Vector.range(0, 50);
        let f = inject tuples into P/1;
        let r = #{
            Q(1).
            S(2).
            T(3).
            R(a, a + b + c + d) :- P(a), Q(b), S(c), T(d).
        };
        let expected = tuples |> Vector.map(x -> (x, x + 1 + 2 + 3));
        let actual = query f, r select (x, y) from R(x, y);
        assertEq(expected = expected, actual)
    }

    @Test
    def testApplicationFour02(): Unit \ Assert = {
        let tuples = Vector.range(0, 50) |> Vector.map(Int32.toString);
        let f = inject tuples into P/1;
        let r = #{
            Q("1").
            S("2").
            T("3").
            R(a, a + b + c + d) :- P(a), Q(b), S(c), T(d).
        };
        let expected = tuples |> Vector.map(x -> (x, x + "1" + "2" + "3")) |> Vector.sort;
        let actual = query f, r select (x, y) from R(x, y);
        assertEq(expected = expected, actual)
    }

    @Test
    def testApplicationFour03(): Unit \ Assert = {
        let tuples = Vector.range(0, 50);
        let f = inject tuples into P/1;
        let r = #{
            Q(1).
            S(2).
            T(3).
            R(a + b + c + d, a) :- P(a), Q(b), S(c), T(d).
        };
        let expected = tuples |> Vector.map(x -> (x + 1 + 2 + 3, x));
        let actual = query f, r select (x, y) from R(x, y);
        assertEq(expected = expected, actual)
    }

    @Test
    def testApplicationFour04(): Unit \ Assert = {
        let tuples = Vector.range(0, 50);
        let f = inject tuples into P/1;
        let r = #{
            Q(1).
            S(2).
            T(3).
            R(a + b + c + d, a + b + c + d) :- P(a), Q(b), S(c), T(d).
        };
        let expected = tuples |> Vector.map(x -> (x + 1 + 2 + 3, x + 1 + 2 + 3));
        let actual = query f, r select (x, y) from R(x, y);
        assertEq(expected = expected, actual)
    }

    /////////////////////////////////////////////////////////////////////////////
    // Tests with application of 5 arguments                                   //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testApplicationFive01(): Unit \ Assert = {
        let tuples = Vector.range(0, 50);
        let f = inject tuples into P/1;
        let r = #{
            Q(1).
            S(2).
            T(3).
            U(4).
            R(a, a + b + c + d + e) :- P(a), Q(b), S(c), T(d), U(e).
        };
        let expected = tuples |> Vector.map(x -> (x, x + 1 + 2 + 3 + 4));
        let actual = query f, r select (x, y) from R(x, y);
        assertEq(expected = expected, actual)
    }

    @Test
    def testApplicationFive02(): Unit \ Assert = {
        let tuples = Vector.range(0, 50) |> Vector.map(Int32.toString);
        let f = inject tuples into P/1;
        let r = #{
            Q("1").
            S("2").
            T("3").
            U("4").
            R(a, a + b + c + d + e) :- P(a), Q(b), S(c), T(d), U(e).
        };
        let expected = tuples |> Vector.map(x -> (x, x + "1" + "2" + "3" + "4")) |> Vector.sort;
        let actual = query f, r select (x, y) from R(x, y);
        assertEq(expected = expected, actual)
    }

    @Test
    def testApplicationFive03(): Unit \ Assert = {
        let tuples = Vector.range(0, 50);
        let f = inject tuples into P/1;
        let r = #{
            Q(1).
            S(2).
            T(3).
            U(4).
            R(a + b + c + d + e, a) :- P(a), Q(b), S(c), T(d), U(e).
        };
        let expected = tuples |> Vector.map(x -> (x + 1 + 2 + 3 + 4, x));
        let actual = query f, r select (x, y) from R(x, y);
        assertEq(expected = expected, actual)
    }

    @Test
    def testApplicationFive04(): Unit \ Assert = {
        let tuples = Vector.range(0, 50);
        let f = inject tuples into P/1;
        let r = #{
            Q(1).
            S(2).
            T(3).
            U(4).
            R(a + b + c + d + e, a + b + c + d + e) :- P(a), Q(b), S(c), T(d), U(e).
        };
        let expected = tuples |> Vector.map(x -> (x + 1 + 2 + 3 + 4, x + 1 + 2 + 3 + 4));
        let actual = query f, r select (x, y) from R(x, y);
        assertEq(expected = expected, actual)
    }

    /////////////////////////////////////////////////////////////////////////////
    // Tests with application of lattice elements                              //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testApplicationLattice01(): Unit \ Assert = {
        let r = #{
            P(;1).
            R(;x + 1) :- P(;x).
        };
        let expected = Vector#{1 + 1};
        let actual = query r select (x) from R(;x);
        assertEq(expected = expected, actual)
    }

    @Test
    def testApplicationLattice02(): Unit \ Assert = {
        let r = #{
            Q(;2).
            P(;1).
            R(;x + y) :- P(;x), Q(;y).
        };
        let expected = Vector#{1 + 2};
        let actual = query r select x from R(;x);
        assertEq(expected = expected, actual)
    }

    @Test
    def testApplicationLattice03(): Unit \ Assert = {
        let r = #{
            S(;3).
            Q(;2).
            P(;1).
            R(;x + y + z) :- P(;x), Q(;y), S(;z).
        };
        let expected = Vector#{1 + 2 + 3};
        let actual = query r select x from R(;x);
        assertEq(expected = expected, actual)
    }

    @Test
    def testApplicationLattice04(): Unit \ Assert = {
        let r = #{
            T(;4).
            S(;3).
            Q(;2).
            P(;1).
            R(;x1 + x2 + x3 + x4) :- P(;x1), Q(;x2), S(;x3), T(;x4).
        };
        let expected = Vector#{1 + 2 + 3 + 4};
        let actual = query r select x from R(;x);
        assertEq(expected = expected, actual)
    }

    @Test
    def testApplicationLattice05(): Unit \ Assert = {
        let r = #{
            U(;5).
            T(;4).
            S(;3).
            Q(;2).
            P(;1).
            R(;x1 + x2 + x3 + x4 + x5) :- P(;x1), Q(;x2), S(;x3), T(;x4), U(;x5).
        };
        let expected = Vector#{1 + 2 + 3 + 4 + 5};
        let actual = query r select x from R(;x);
        assertEq(expected = expected, actual)
    }

}

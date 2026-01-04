mod Test.Exp.Fixpoint.If {

    use Assert.{assertEq, assertTrue};

    /////////////////////////////////////////////////////////////////////////////
    // Test if-guard with 0 parameters                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testIfGuardZero01(): Unit \ Assert = {
        let f = inject Vector.range(0, 10) into A/1;
        let r = #{
            Empty(x) :- A(x), if (false).
        };
        assertTrue(query f, r select x from Empty(x) |> Vector.isEmpty)
    }

    @Test
    def testIfGuardZero02(): Unit \ Assert = {
        let tuples = Vector.range(0, 10);
        let f = inject tuples into A/1;
        let r = #{
            Empty(x) :- A(x), if (true).
        };
        assertEq(expected = tuples, query f, r select x from Empty(x))
    }

    /////////////////////////////////////////////////////////////////////////////
    // Test if-guard with 1 parameter                                          //
    /////////////////////////////////////////////////////////////////////////////

    def zeroModuloUntil(modulo: Int32, n: Int32): Vector[Int32] = {
        Vector.range(0, n) |> Vector.filter(x -> Int32.modulo(x, modulo) == 0)
    }

    @Test
    def testIfGuardOne01(): Unit \ Assert = {
        let nat = Vector.range(0, 100);
        let f = inject nat into Nat/1;
        let even = Vector.filter(x -> Int32.modulo(x, 2) == 0, nat);
        let r = #{
            Even(x) :- Nat(x), if (Int32.modulo(x, 2) == 0).
        };
        assertEq(expected = even, query f, r select x from Even(x))
    }

    @Test
    def testIfGuardOne02(): Unit \ Assert = {
        let nat = Vector.range(0, 100);
        let f = inject nat into Nat/1;
        let odd = Vector.filter(x -> Int32.modulo(x, 2) != 0, nat);
        let r = #{
            Odd(x) :- Nat(x), if (Int32.modulo(x, 2) != 0).
        };
        assertEq(expected = odd, query f, r select x from Odd(x))
    }

    @Test
    def testIfGuardOne03(): Unit \ Assert = {
        let nat = Vector.range(0, 100);
        let f = inject nat into Nat/1;
        let even = Vector.filter(x -> Int32.modulo(x, 2) == 0, nat);
        let odd = Vector.filter(x -> Int32.modulo(x, 2) != 0, nat);
        let r = #{
            Even(x) :- Nat(x), if (Int32.modulo(x, 2) == 0).
            Odd(x) :- Nat(x), if (Int32.modulo(x, 2) != 0).
        };
        assertEq(expected = even, query f, r select x from Even(x));
        assertEq(expected = odd, query f, r select x from Odd(x))
    }

    @Test
    pub def testIfGuardOne04(): Unit \ Assert = {
        let nat = Vector.range(0, 100);
        let f = inject nat into Nat/1;
        let even = Vector.filter(x -> Int32.modulo(x, 2) == 0, nat);
        let odd = Vector.filter(x -> Int32.modulo(x, 2) != 0, nat);
        let r = #{
            Even(x) :- Nat(x), if (Int32.modulo(x, 2) == 0).
            Odd(x) :- Nat(x), if (Int32.modulo(x, 2) != 0).
        };
        let res = even |> Vector.flatMap(e -> odd |> Vector.map(o -> (e, o))) |> Vector.sort;
        assertEq(expected = res, query f, r select (x, y) from Even(x), Odd(y))
    }

    /////////////////////////////////////////////////////////////////////////////
    // Test if-guard with 2 parameters                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testIfGuardTwo01(): Unit \ Assert = {
        let tuples = Vector.range(0, 100) |> Vector.map(x -> (x, x + 1));
        let f = inject tuples into A/2;
        let r = #{
            R(x, y) :- A(x, y), if (x + 1 == y).
        };
        assertEq(expected = tuples, query f, r select (x, y) from R(x, y))
    }

    @Test
    def testIfGuardTwo02(): Unit \ Assert = {
        let tuples = Vector.range(0, 100) |> Vector.map(x -> (x, x + 1));
        let f = inject tuples into A/2;
        let r = #{
            R(x, y) :- A(x, y), if (x + 1 == y).
        };
        assertEq(expected = tuples, query f, r select (x, y) from R(x, y))
    }

    @Test
    def testIfGuardTwo03(): Unit \ Assert = {
        let tuples = Vector.range(0, 100) |> Vector.map(x -> (x, x + 1));
        let tenth = zeroModuloUntil(10, 400);
        let f = inject tuples into A/2;
        let r = #{
            R(x, y) :- A(x, y), if (Vector.memberOf(x + y, tenth)).
        };
        let res = tuples |> Vector.filter(match (x, y) -> Vector.memberOf(x + y, tenth)) |> Vector.sort;
        assertEq(expected = res, query f, r select (x, y) from R(x, y))
    }

    @Test
    def testIfGuardTwo04(): Unit \ Assert = {
        let tuples = Vector.range(0, 100) |> Vector.map(x -> (Int32.toString(x), Int32.toString(2 * x)));
        let f = inject tuples into A/2;
        let r = #{
            R(x, y) :- A(x, y), if (String.exists(c -> c == '3', x + y)).
        };
        let res = tuples |> Vector.filter(match (x, y) -> String.exists(c -> c == '3', x + y)) |> Vector.sort;
        assertEq(expected = res, query f, r select (x, y) from R(x, y))
    }

    /////////////////////////////////////////////////////////////////////////////
    // Test if-guard with 3 parameters                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testIfGuardThree01(): Unit \ Assert = {
        let tuples = Vector.range(0, 100) |> Vector.map(x -> (x, x, 2 * x));
        let f = inject tuples into A/3;
        let r = #{
            R(x, y, z) :- A(x, y, z), if (x + y == 2 * x).
        };
        assertEq(expected = tuples, query f, r select (x, y, z) from R(x, y, z))
    }

    @Test
    def testIfGuardThree02(): Unit \ Assert = {
        let tuples = Vector.range(0, 100) |> Vector.map(x -> (x, x, 2 * x));
        let f = inject tuples into A/3;
        let r = #{
            R(x, y, z) :- A(x, y, z), if (x + y == z).
        };
        assertEq(expected = tuples, query f, r select (x, y, z) from R(x, y, z))
    }

    @Test
    def testIfGuardThree03(): Unit \ Assert = {
        let tuples = Vector.range(0, 100) |> Vector.map(x -> (x, x, 2 * x));
        let f = inject tuples into A/3;
        let r = #{
            R(x, y, z) :- A(x, y, z), if (x + y + z == 0).
        };
        assertEq(expected = Vector#{(0, 0, 0)}, query f, r select (x, y, z) from R(x, y, z))
    }

    @Test
    def testIfGuardThree04(): Unit \ Assert = {
        let tuples = Vector.range(0, 100) |> Vector.map(x -> (x, x * 2, x * 3));
        let tenth = zeroModuloUntil(10, 1000);
        let f = inject tuples into A/3;
        let r = #{
            R(x, y, z) :- A(x, y, z), if (Vector.memberOf(x + y + z, tenth)).
        };
        let res = tuples |> Vector.filter(match (x, y, z) -> Vector.memberOf(x + y + z, tenth)) |> Vector.sort;
        assertEq(expected = res, query f, r select (x, y, z) from R(x, y, z))
    }

    @Test
    def testIfGuardThree05(): Unit \ Assert = {
        let tuples = Vector.range(0, 100) |> Vector.map(x -> (Int32.toString(x), Int32.toString(2 * x), Int32.toString(3 * x)));
        let f = inject tuples into A/3;
        let r = #{
            R(x, y, z) :- A(x, y, z), if (String.exists(c -> c == '3', x + y + z)).
        };
        let res = tuples |> Vector.filter(match (x, y, z) -> String.exists(c -> c == '3', x + y + z)) |> Vector.sort;
        assertEq(expected = res, query f, r select (x, y, z) from R(x, y, z))
    }

    /////////////////////////////////////////////////////////////////////////////
    // Test if-guard with 4 parameters                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testIfGuardFour01(): Unit \ Assert = {
        let tuples = Vector.range(0, 100) |> Vector.map(x -> (x, x, x, x + x + x));
        let f = inject tuples into A/4;
        let r = #{
            R(x, y, z, w) :- A(x, y, z, w), if (x + y + z == w).
        };
        assertEq(expected = tuples, query f, r select (x, y, z, w) from R(x, y, z, w))
    }

    @Test
    def testIfGuardFour02(): Unit \ Assert = {
        let tuples = Vector.range(0, 100) |> Vector.map(x -> (x, 2 * x, 3 * x, 4 * x));
        let tenth = zeroModuloUntil(10, 1200);
        let f = inject tuples into A/4;
        let r = #{
            R(x, y, z, w) :- A(x, y, z, w), if (Vector.memberOf(x + y + z + w, tenth)).
        };
        let res = tuples |> Vector.filter(match (x, y, z, w) -> Vector.memberOf(x + y + z + w, tenth)) |> Vector.sort;
        assertEq(expected = res, query f, r select (x, y, z, w) from R(x, y, z, w))
    }

    @Test
    def testIfGuardFour03(): Unit \ Assert = {
        let tuples = Vector.range(0, 100) |> Vector.map(x -> (Int32.toString(x), Int32.toString(2 * x), Int32.toString(3 * x), Int32.toString(4 * x)));
        let f = inject tuples into A/4;
        let r = #{
            R(x, y, z, w) :- A(x, y, z, w), if (String.exists(c -> c == '3', x + y + z + w)).
        };
        let res = tuples |> Vector.filter(match (x, y, z, w) -> String.exists(c -> c == '3', x + y + z + w)) |> Vector.sort;
        assertEq(expected = res, query f, r select (x, y, z, w) from R(x, y, z, w))
    }

    /////////////////////////////////////////////////////////////////////////////
    // Test if-guard with 5 parameters                                         //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testIfGuardFive01(): Unit \ Assert = {
        let tuples = Vector.range(0, 100) |> Vector.map(x -> (x, 2 * x, 3 * x, 4 * x, 5 * x));
        let tenth = zeroModuloUntil(10, 2000);
        let f = inject tuples into A/5;
        let r = #{
            R(x, y, z, w, a) :- A(x, y, z, w, a), if (Vector.memberOf(x + y + z + w + a, tenth)).
        };
        let res = tuples |> Vector.filter(match (x, y, z, w, a) -> Vector.memberOf(x + y + z + w + a, tenth)) |> Vector.sort;
        assertEq(expected = res, query f, r select (x, y, z, w, a) from R(x, y, z, w, a))
    }

    @Test
    def testIfGuardFive02(): Unit \ Assert = {
        let tuples = Vector.range(0, 100) |> Vector.map(x -> (Int32.toString(x), Int32.toString(2 * x), Int32.toString(3 * x), Int32.toString(4 * x), Int32.toString(5 * x)));
        let f = inject tuples into A/5;
        let r = #{
            R(x, y, z, w, a) :- A(x, y, z, w, a), if (String.length(x + y + z + w + a) > 5).
        };
        let res = tuples |> Vector.filter(match (x, y, z, w, a) -> String.length(x + y + z + w + a) > 5) |> Vector.sort;
        assertEq(expected = res, query f, r select (x, y, z, w, a) from R(x, y, z, w, a))
    }

    /////////////////////////////////////////////////////////////////////////////
    // Test if-guard with lattice elements                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testIfGuardOneLattice01(): Unit \ Assert = {
        let tuples = Vector#{(0, 1), (1, 2), (2, 0)};
        let f = inject tuples into Edge/2;
        let r = #{
            Path(x, y; Down.Down(1)) :- Edge(x, y).
            Path(x, z; l + Down.Down(1)) :- Path(x, y; l), Edge(y, z), if (l == Down.Down(0)).
        };
        let res = query f, r select (x, y) from Path(x, y; l);
        assertEq(expected = res, tuples)
    }

    /////////////////////////////////////////////////////////////////////////////
    // Test that if-guard position in body is irrelevant                       //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testIfGuardBodyPosition01(): Unit \ Assert = {
        let tuples = Vector.range(0, 20);
        let f = inject tuples into Q/1;
        let r = #{
            P1(x) :- if (true), Q(x).
            P2(x) :- Q(x), if (true).
        };
        let p = solve f, r;
        let res1 = query p select x from P1(x);
        let res2 = query p select x from P2(x);
        assertEq(expected = res1, res2);
        assertEq(expected = res1, tuples)
    }

    @Test
    def testIfGuardBodyPosition02(): Unit \ Assert = {
        let tuples = Vector.range(0, 20);
        let f1 = inject tuples into Q1/1;
        let f2 = inject tuples into Q2/1;
        let r = #{
            P1(x, y) :- if (true), Q1(x), Q2(y).
            P2(x, y) :- Q1(x), if (true), Q2(y).
            P3(x, y) :- Q1(x), Q2(y), if (true).
        };
        let p = solve f1, f2, r;
        let res1 = query p select (x, y) from P1(x, y);
        let res2 = query p select (x, y) from P2(x, y);
        let res3 = query p select (x, y) from P3(x, y);
        let res = tuples |>
            Vector.map(x ->
                Vector.foldLeft((acc, y) -> (x, y) :: acc, Nil, tuples) |>
                List.reverse |>
                List.toVector
            ) |>
            Vector.fold;
        assertEq(expected = res1, res2);
        assertEq(expected = res2, res3);
        assertEq(expected = res3, res)
    }

    @Test
    def testIfGuardBodyPosition03(): Unit \ Assert = {
        let f = inject Vector#{("Alice", "Carol"), ("Bob", "Carol")} into Friend/2;
        let r = #{
            FriendInCommon(x, y) :- Friend(x, a), Friend(y, a), if (x != y).
        };
        let res = query f, r select (x, y) from FriendInCommon(x, y);
        assertEq(expected = res, Vector#{("Alice", "Bob"), ("Bob", "Alice")})
    }

    @Test
    def testIfGuardBodyPosition04(): Unit \ Assert = {
        let f = inject Vector#{("Alice", "Carol"), ("Bob", "Carol")} into Friend/2;
        let r = #{
            FriendInCommon(x, y) :- Friend(x, a), if (x != y), Friend(y, a).
        };
        let res = query f, r select (x, y) from FriendInCommon(x, y);
        assertEq(expected = res, Vector#{("Alice", "Bob"), ("Bob", "Alice")})
    }

    @Test
    def testIfGuardBodyPosition05(): Unit \ Assert = {
        let f = inject Vector#{("Alice", "Carol"), ("Bob", "Carol")} into Friend/2;
        let r = #{
            FriendInCommon(x, y) :- if (x != y), Friend(x, a), Friend(y, a).
        };
        let res = query f, r select (x, y) from FriendInCommon(x, y);
        assertEq(expected = res, Vector#{("Alice", "Bob"), ("Bob", "Alice")})
    }


    /////////////////////////////////////////////////////////////////////////////
    // Test if-guard work on lattice elements                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testIfGuardLattice01(): Unit \ Assert = {
        let r = #{
            Q(1; 2).
            P(x; y) :- Q(x; y), if (x != y).
        };
        let res = query r select (x, y) from P(x; y);
        assertEq(expected = res, Vector#{(1, 2)})
    }

    @Test
    def testIfGuardLattice02(): Unit \ Assert = {
        let r = #{
            Q(1; 2).
            P(x; y) :- if (x != y), Q(x; y).
        };
        let res = query r select (x, y) from P(x; y);
        assertEq(expected = res, Vector#{(1, 2)})
    }

    @Test
    def testIfGuardLattice03(): Unit \ Assert = {
        let r = #{
            Q(1; 2).
            P(x; y) :- Q(x; y), if (x == y).
        };
        let res = query r select (x, y) from P(x; y);
        assertEq(expected = res, Vector#{})
    }

    @Test
    def testIfGuardLattice04(): Unit \ Assert = {
        let r = #{
            Q(1; 2).
            P(x; y) :- if (x == y), Q(x; y).
        };
        let res = query r select (x, y) from P(x; y);
        assertEq(expected = res, Vector#{})
    }

}

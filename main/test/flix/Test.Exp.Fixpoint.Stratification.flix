mod Test.Exp.Fixpoint.Stratification {

    use Assert.assertEq;

    @Test
    def testStratification01(): Unit \ Assert = {
        let pr = #{
            A1(1).
            B1(x) :- A1(x).
            C1(x) :- B1(x).
            D1(x) :- C1(x).
        };
        let mm = solve pr;
        assertEq(expected = Vector#{1}, query mm select (x) from A1(x));
        assertEq(expected = Vector#{1}, query mm select (x) from B1(x));
        assertEq(expected = Vector#{1}, query mm select (x) from C1(x));
        assertEq(expected = Vector#{1}, query mm select (x) from D1(x))

    }

    @Test
    def testStratification02(): Unit \ Assert = {
        let pr = #{
            A2(1).
            B2(x) :- A2(x).
            C2(x + y) :- A2(x), B2(y).
            D2(x + y + z) :- A2(x), B2(y), C2(z).
        };
        let mm = solve pr;
        assertEq(expected = Vector#{1}, query mm select (x) from A2(x));
        assertEq(expected = Vector#{1}, query mm select (x) from B2(x));
        assertEq(expected = Vector#{1 + 1}, query mm select (x) from C2(x));
        assertEq(expected = Vector#{1 + 1 + 2}, query mm select (x) from D2(x))
    }

    @Test
    def testStratification03(): Unit \ Assert = {
        let pr = #{
            A(1, 2).
            A(2, 3).
            A(3, 4).
            B(x, 4) :- A(1, x).
            C(x, y) :- A(2, x), B(2, y).
            D(9) :- A(_, 3), C(_, 4).
        };
        let mm = solve pr;
        assertEq(expected = Vector#{(1, 2), (2, 3), (3, 4)}, query mm select (x, y) from A(x, y));
        assertEq(expected = Vector#{(2, 4)}, query mm select (x, y) from B(x, y));
        assertEq(expected = Vector#{(3, 4)}, query mm select (x, y) from C(x, y));
        assertEq(expected = Vector#{9}, query mm select (x) from D(x))
    }

    @Test
    def testStratification04(): Unit \ Assert = {
        let pr = #{
            A(1, 2).
            A(2, 3).
            A(3, 4).
            B(x, 4) :- A(1, x).
            C(x, y) :- B(2, y), A(2, x).
            D(9) :- C(_, 4), A(_, 3).
        };
        let mm = solve pr;
        assertEq(expected = Vector#{(1, 2), (2, 3), (3, 4)}, query mm select (x, y) from A(x, y));
        assertEq(expected = Vector#{(2, 4)}, query mm select (x, y) from B(x, y));
        assertEq(expected = Vector#{(3, 4)}, query mm select (x, y) from C(x, y));
        assertEq(expected = Vector#{9}, query mm select (x) from D(x))
    }

    @Test
    def testStratification05(): Unit \ Assert = {
        let pr = #{
            A("1", "2").
            A("2", "3").
            A("3", "4").
            B(x, "4") :- A("1", x).
            C(x, y) :- B("2", y), A("2", x).
            D("9") :- C(_, "4"), A(_, "3").
        };
        let mm = solve pr;
        assertEq(expected = Vector#{("1", "2"), ("2", "3"), ("3", "4")}, query mm select (x, y) from A(x, y));
        assertEq(expected = Vector#{("2", "4")}, query mm select (x, y) from B(x, y));
        assertEq(expected = Vector#{("3", "4")}, query mm select (x, y) from C(x, y));
        assertEq(expected = Vector#{"9"}, query mm select (x) from D(x))
    }

    @Test
    def testStratification06(): Unit \ Assert = {
        let pr = #{
            A(1).
            A(2).
            B(3).
            B(x) :- A(x).
            C(x) :- B(x), not A(x).
        };
        let mm = solve pr;
        assertEq(expected = Vector#{1, 2}, query mm select (x) from A(x));
        assertEq(expected = Vector#{1, 2, 3}, query mm select (x) from B(x));
        assertEq(expected = Vector#{3}, query mm select (x) from C(x))
    }

    @Test
    def testStratification07(): Unit \ Assert = {
        let pr = #{
            A(1).
            A(2).
            B(3).
            B(x) :- A(x).
            C(x) :- fix B(x), not A(x).
        };
        let mm = solve pr;
        assertEq(expected = Vector#{1, 2}, query mm select (x) from A(x));
        assertEq(expected = Vector#{1, 2, 3}, query mm select (x) from B(x));
        assertEq(expected = Vector#{3}, query mm select (x) from C(x))
    }

    @Test
    def testStratification08(): Unit \ Assert = {
        let pr = #{
            A(1).
            A(2).
            B(3).
            B(x) :- fix A(x).
            C(x) :- B(x), not A(x).
        };
        let mm = solve pr;
        assertEq(expected = Vector#{1, 2}, query mm select (x) from A(x));
        assertEq(expected = Vector#{1, 2, 3}, query mm select (x) from B(x));
        assertEq(expected = Vector#{3}, query mm select (x) from C(x))
    }

    @Test
    def testStratification09(): Unit \ Assert = {
        let pr = #{
            A("1").
            A("2").
            B("3").
            B(x) :- A(x).
            C(x) :- B(x), not A(x).
        };
        let mm = solve pr;
        assertEq(expected = Vector#{"1", "2"}, query mm select (x) from A(x));
        assertEq(expected = Vector#{"1", "2", "3"}, query mm select (x) from B(x));
        assertEq(expected = Vector#{"3"}, query mm select (x) from C(x))
    }
}

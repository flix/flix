mod Test.Term.Var.WildVar {

    use Assert.assertMemberOf

    @Test
    def testWildVar01(): Unit \ Assert =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(x, y, z), B(x, y, z), C(x, y, z).
            F() :- A(x, y, z), B(x, y, z), C(x, y, z), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        assertMemberOf(42, m)

    @Test
    def testWildVar02(): Unit \ Assert =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(_, y, z), B(x, y, z), C(x, y, z).
            F() :- A(_, y, z), B(x, y, z), C(x, y, z), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        assertMemberOf(42, m)

    @Test
    def testWildVar03(): Unit \ Assert =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(x, _, z), B(x, y, z), C(x, y, z).
            F() :- A(x, _, z), B(x, y, z), C(x, y, z), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        assertMemberOf(42, m)

    @Test
    def testWildVar04(): Unit \ Assert =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(x, y, _), B(x, y, z), C(x, y, z).
            F() :- A(x, y, _), B(x, y, z), C(x, y, z), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        assertMemberOf(42, m)

    @Test
    def testWildVar05(): Unit \ Assert =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(x, y, z), B(_, y, z), C(x, y, z).
            F() :- A(x, y, z), B(_, y, z), C(x, y, z), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        assertMemberOf(42, m)

    @Test
    def testWildVar06(): Unit \ Assert =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(x, y, z), B(x, _, z), C(x, y, z).
            F() :- A(x, y, z), B(x, _, z), C(x, y, z), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        assertMemberOf(42, m)

    @Test
    def testWildVar07(): Unit \ Assert =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(x, y, z), B(x, y, _), C(x, y, z).
            F() :- A(x, y, z), B(x, y, _), C(x, y, z), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        assertMemberOf(42, m)

    @Test
    def testWildVar08(): Unit \ Assert =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(x, y, z), B(x, y, z), C(_, y, z).
            F() :- A(x, y, z), B(x, y, z), C(_, y, z), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        assertMemberOf(42, m)

    @Test
    def testWildVar09(): Unit \ Assert =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(x, y, z), B(x, y, z), C(x, _, z).
            F() :- A(x, y, z), B(x, y, z), C(x, _, z), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        assertMemberOf(42, m)

    @Test
    def testWildVar10(): Unit \ Assert =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(x, y, z), B(x, y, z), C(x, y, _).
            F() :- A(x, y, z), B(x, y, z), C(x, y, _), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        assertMemberOf(42, m)

    @Test
    def testWildVar11(): Unit \ Assert =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(_, _, z), B(x, y, z), C(x, y, z).
            F() :- A(_, _, z), B(x, y, z), C(x, y, z), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        assertMemberOf(42, m)

    @Test
    def testWildVar12(): Unit \ Assert =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(x, _, _), B(x, y, z), C(x, y, z).
            F() :- A(x, _, _), B(x, y, z), C(x, y, z), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        assertMemberOf(42, m)

    @Test
    def testWildVar13(): Unit \ Assert =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(x, y, _), B(_, y, z), C(x, y, z).
            F() :- A(x, y, _), B(_, y, z), C(x, y, z), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        assertMemberOf(42, m)

    @Test
    def testWildVar14(): Unit \ Assert =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(x, y, z), B(_, _, z), C(x, y, z).
            F() :- A(x, y, z), B(_, _, z), C(x, y, z), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        assertMemberOf(42, m)

    @Test
    def testWildVar15(): Unit \ Assert =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(x, y, z), B(x, _, _), C(x, y, z).
            F() :- A(x, y, z), B(x, _, _), C(x, y, z), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        assertMemberOf(42, m)

    @Test
    def testWildVar16(): Unit \ Assert =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(x, y, z), B(x, y, _), C(_, y, z).
            F() :- A(x, y, z), B(x, y, _), C(_, y, z), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        assertMemberOf(42, m)

    @Test
    def testWildVar17(): Unit \ Assert =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(x, y, z), B(x, y, z), C(_, _, z).
            F() :- A(x, y, z), B(x, y, z), C(_, _, z), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        assertMemberOf(42, m)

    @Test
    def testWildVar18(): Unit \ Assert =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(x, y, z), B(x, y, z), C(x, _, _).
            F() :- A(x, y, z), B(x, y, z), C(x, _, _), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        assertMemberOf(42, m)

    @Test
    def testWildVar19(): Unit \ Assert =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(x, _, z), B(x, _y, z), C(x, _, z).
            F() :- A(x, _, z), B(x, y, z), C(x, _, z), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        assertMemberOf(42, m)

    @Test
    def testWildVar20(): Unit \ Assert =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(_x, _, _), B(_, _y, _), C(_, _, _z).
            F() :- A(x, _, _), B(_, y, _), C(_, _, z), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        assertMemberOf(42, m)

}

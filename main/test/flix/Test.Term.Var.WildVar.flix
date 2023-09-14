mod Test.Term.Var.WildVar {

    @Test
    def testWildVar01(): Bool =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(x, y, z), B(x, y, z), C(x, y, z).
            F() :- A(x, y, z), B(x, y, z), C(x, y, z), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        not Vector.isEmpty(m)

    @Test
    def testWildVar02(): Bool =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(_, y, z), B(x, y, z), C(x, y, z).
            F() :- A(_, y, z), B(x, y, z), C(x, y, z), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        not Vector.isEmpty(m)

    @Test
    def testWildVar03(): Bool =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(x, _, z), B(x, y, z), C(x, y, z).
            F() :- A(x, _, z), B(x, y, z), C(x, y, z), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        not Vector.isEmpty(m)

    @Test
    def testWildVar04(): Bool =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(x, y, _), B(x, y, z), C(x, y, z).
            F() :- A(x, y, _), B(x, y, z), C(x, y, z), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        not Vector.isEmpty(m)

    @Test
    def testWildVar05(): Bool =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(x, y, z), B(_, y, z), C(x, y, z).
            F() :- A(x, y, z), B(_, y, z), C(x, y, z), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        not Vector.isEmpty(m)


    @Test
    def testWildVar06(): Bool =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(x, y, z), B(x, _, z), C(x, y, z).
            F() :- A(x, y, z), B(x, _, z), C(x, y, z), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        not Vector.isEmpty(m)

    @Test
    def testWildVar07(): Bool =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(x, y, z), B(x, y, _), C(x, y, z).
            F() :- A(x, y, z), B(x, y, _), C(x, y, z), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        not Vector.isEmpty(m)

    @Test
    def testWildVar08(): Bool =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(x, y, z), B(x, y, z), C(_, y, z).
            F() :- A(x, y, z), B(x, y, z), C(_, y, z), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        not Vector.isEmpty(m)

    @Test
    def testWildVar09(): Bool =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(x, y, z), B(x, y, z), C(x, _, z).
            F() :- A(x, y, z), B(x, y, z), C(x, _, z), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        not Vector.isEmpty(m)

    @Test
    def testWildVar10(): Bool =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(x, y, z), B(x, y, z), C(x, y, _).
            F() :- A(x, y, z), B(x, y, z), C(x, y, _), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        not Vector.isEmpty(m)

    @Test
    def testWildVar11(): Bool =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(_, _, z), B(x, y, z), C(x, y, z).
            F() :- A(_, _, z), B(x, y, z), C(x, y, z), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        not Vector.isEmpty(m)

    @Test
    def testWildVar12(): Bool =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(x, _, _), B(x, y, z), C(x, y, z).
            F() :- A(x, _, _), B(x, y, z), C(x, y, z), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        not Vector.isEmpty(m)

    @Test
    def testWildVar13(): Bool =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(x, y, _), B(_, y, z), C(x, y, z).
            F() :- A(x, y, _), B(_, y, z), C(x, y, z), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        not Vector.isEmpty(m)

    @Test
    def testWildVar14(): Bool =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(x, y, z), B(_, _, z), C(x, y, z).
            F() :- A(x, y, z), B(_, _, z), C(x, y, z), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        not Vector.isEmpty(m)

    @Test
    def testWildVar15(): Bool =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(x, y, z), B(x, _, _), C(x, y, z).
            F() :- A(x, y, z), B(x, _, _), C(x, y, z), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        not Vector.isEmpty(m)

    @Test
    def testWildVar16(): Bool =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(x, y, z), B(x, y, _), C(_, y, z).
            F() :- A(x, y, z), B(x, y, _), C(_, y, z), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        not Vector.isEmpty(m)

    @Test
    def testWildVar17(): Bool =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(x, y, z), B(x, y, z), C(_, _, z).
            F() :- A(x, y, z), B(x, y, z), C(_, _, z), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        not Vector.isEmpty(m)

    @Test
    def testWildVar18(): Bool =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(x, y, z), B(x, y, z), C(x, _, _).
            F() :- A(x, y, z), B(x, y, z), C(x, _, _), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        not Vector.isEmpty(m)

    @Test
    def testWildVar19(): Bool =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(x, _, z), B(x, _y, z), C(x, _, z).
            F() :- A(x, _, z), B(x, y, z), C(x, _, z), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        not Vector.isEmpty(m)

    @Test
    def testWildVar20(): Bool =
        let p = #{
            A(1, 2, 3).
            B(1, 2, 3).
            C(1, 2, 3).
            T() :- A(_x, _, _), B(_, _y, _), C(_, _, _z).
            F() :- A(x, _, _), B(_, y, _), C(_, _, z), D(x, y, z).
        };
        let m = query p select 42 from T(), not F();
        not Vector.isEmpty(m)

}

mod Test.Exp.Fixpoint.Large {

    use Assert.assertEq;

    /////////////////////////////////////////////////////////////////////////////
    /// `query` should support large minimal models.
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testQueryLargeModel01(): Unit \ Assert =
        let db = inject Vector.range(0, 512) into A/1;
        let pr = #{
            R(x, y) :- A(x), A(y).
        };
        let result = query db, pr select (x, y) from R(x, y);
        assertEq(expected = 512 * 512, Vector.length(result))


    @Test
    def testQueryLargeModel02(): Unit \ Assert =
        let db = inject Vector.range(0, 1024) into A/1;
        let pr = #{
            R(x, y) :- A(x), A(y).
        };
        let result = query db, pr select (x, y) from R(x, y);
        assertEq(expected = 1024 * 1024, Vector.length(result))

    /////////////////////////////////////////////////////////////////////////////
    /// `pquery` should support large minimal models.
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testPQueryLargeModel01(): Unit \ Assert =
        let db = inject Vector.range(0, 512) into A/1;
        let pr = #{
            R(x, y) :- A(x), A(y).
        };
        let result = pquery db, pr select R(0, 0) with {A};
        assertEq(expected = 2, Vector.length(result))

    @Test
    def testPQueryLargeModel02(): Unit \ Assert =
        let db = inject Vector.range(0, 1024) into A/1;
        let pr = #{
            R(x, y) :- A(x), A(y).
        };
        let result = pquery db, pr select R(0, 0) with {A};
        assertEq(expected = 2, Vector.length(result))

    /////////////////////////////////////////////////////////////////////////////
    /// `pquery` should support large provenance paths.
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testPQueryLargePath01(): Unit \ Assert =
        let pr = #{
            R(1).
            R(x + 1) :- R(x), if (x < 1024).
        };
        let result = pquery pr select R(1024) with {R};
        assertEq(expected = 1024, Vector.length(result))

    @Test
    def testPQueryLargePath02(): Unit \ Assert =
        let pr = #{
            R(1).
            R(x + 1) :- R(x), if (x < 8192).
        };
        let result = pquery pr select R(8192) with {R};
        assertEq(expected = 8192, Vector.length(result))

    @Test
    def testPQueryLargePath03(): Unit \ Assert =
        let db = inject Vector.map(x -> (x, x + 1), Vector.range(0, 65536)) into Helper/2;
        let pr = #{
            R(0, 1).
            R(x, y) :- R(_, x), Helper(x, y).
        };
        let result = pquery db, pr select R(65535, 65536) with {R};
        assertEq(expected = 65536, Vector.length(result))

}

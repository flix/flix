mod Test.Exp.Fixpoint.Large {

    /////////////////////////////////////////////////////////////////////////////
    /// `query` should support large minimal models.
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testQueryLargeModel01(): Bool =
        let db = inject Vector.range(0, 512) into A/1;
        let pr = #{
            R(x, y) :- A(x), A(y).
        };
        let result = query db, pr select (x, y) from R(x, y);
        Assert.eq(512 * 512, Vector.length(result))


    @Test
    def testQueryLargeModel02(): Bool =
        let db = inject Vector.range(0, 1024) into A/1;
        let pr = #{
            R(x, y) :- A(x), A(y).
        };
        let result = query db, pr select (x, y) from R(x, y);
        Assert.eq(1024 * 1024, Vector.length(result))

    /////////////////////////////////////////////////////////////////////////////
    /// `pquery` should support large minimal models.
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testPQueryLargeModel01(): Bool =
        let db = inject Vector.range(0, 512) into A/1;
        let pr = #{
            R(x, y) :- A(x), A(y).
        };
        let result = pquery db, pr select R(0, 0) with {A};
        Assert.eq(2, Vector.length(result))

    @Test
    def testPQueryLargeModel02(): Bool =
        let db = inject Vector.range(0, 1024) into A/1;
        let pr = #{
            R(x, y) :- A(x), A(y).
        };
        let result = pquery db, pr select R(0, 0) with {A};
        Assert.eq(2, Vector.length(result))

    /////////////////////////////////////////////////////////////////////////////
    /// `pquery` should support large provenance paths.
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def testPQueryLargePath01(): Bool =
        let pr = #{
            R(1).
            R(x + 1) :- R(x), if (x < 1024).
        };
        let result = pquery pr select R(1024) with {R};
        Assert.eq(1024, Vector.length(result))

    @Test
    def testPQueryLargePath02(): Bool =
        let pr = #{
            R(1).
            R(x + 1) :- R(x), if (x < 8192).
        };
        let result = pquery pr select R(8192) with {R};
        Assert.eq(8192, Vector.length(result))

    @Test
    def testPQueryLargePath03(): Bool =
        let pr = #{
            R(1).
            R(x + 1) :- R(x), if (x < 65536).
        };
        let result = pquery pr select R(65536) with {R};
        Assert.eq(65536, Vector.length(result))

}

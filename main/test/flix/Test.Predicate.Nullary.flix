mod Test.Predicate.Nullary {

    @Test
    def testNullaryPredicate01(): Bool =
        let p = #{
            A(). B(). C().
        };
        let m = query p select 42 from A();
        Vector.memberOf(42, m)

    @Test
    def testNullaryPredicate02(): Bool =
        let p = #{
            A(). B(). C().
        };
        let m = query p select 42 from B();
        Vector.memberOf(42, m)

    @Test
    def testNullaryPredicate03(): Bool =
        let p = #{
            A(). B(). C().
        };
        let m = query p select 42 from C();
        Vector.memberOf(42, m)

    @Test
    def testNullaryPredicate04(): Bool =
        let p = #{
            A(). B(). C().
            R() :- A().
        };
        let m = query p select 42 from R();
        Vector.memberOf(42, m)

    @Test
    def testNullaryPredicate05(): Bool =
        let p = #{
            A(). B(). C().
            R() :- B().
        };
        let m = query p select 42 from R();
        Vector.memberOf(42, m)

    @Test
    def testNullaryPredicate06(): Bool =
        let p = #{
            A(). B(). C().
            R() :- C().
        };
        let m = query p select 42 from R();
        Vector.memberOf(42, m)

    @Test
    def testNullaryPredicate07(): Bool =
        let p = #{
            A(). B(). C().
            R() :- A(), B(), C().
        };
        let m = query p select 42 from R();
        Vector.memberOf(42, m)

}

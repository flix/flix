mod Test.Predicate.Nullary {

    use Assert.{assertEq, assertMemberOf}

    @Test
    def testNullaryPredicate01(): Unit \ Assert =
        let p = #{
            A. B. C.
        };
        let m = query p select 42 from A;
        assertMemberOf(42, m)

    @Test
    def testNullaryPredicate02(): Unit \ Assert =
        let p = #{
            A. B. C.
        };
        let m = query p select 42 from B;
        assertMemberOf(42, m)

    @Test
    def testNullaryPredicate03(): Unit \ Assert =
        let p = #{
            A. B. C.
        };
        let m = query p select 42 from C;
        assertMemberOf(42, m)

    @Test
    def testNullaryPredicate04(): Unit \ Assert =
        let p = #{
            A. B. C.
            R :- A.
        };
        let m = query p select 42 from R;
        assertMemberOf(42, m)

    @Test
    def testNullaryPredicate05(): Unit \ Assert =
        let p = #{
            A. B. C.
            R :- B.
        };
        let m = query p select 42 from R;
        assertMemberOf(42, m)

    @Test
    def testNullaryPredicate06(): Unit \ Assert =
        let p = #{
            A. B. C.
            R :- C.
        };
        let m = query p select 42 from R;
        assertMemberOf(42, m)

    @Test
    def testNullaryPredicate07(): Unit \ Assert =
        let p = #{
            A. B. C.
            R :- A, B, C.
        };
        let m = query p select 42 from R;
        assertMemberOf(42, m)

    @Test
    def testNullaryPredicate08(): Unit \ Assert =
        let p = #{
            R :- A.
        };
        let m = query p select 42 from R;
        assertEq(expected = Vector#{}, m)

}

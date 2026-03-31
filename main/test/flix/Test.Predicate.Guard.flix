mod Test.Predicate.Guard {

    use Assert.{assertMemberOf, assertFalse}

    @Test
    def testGuardConst01(): Unit \ Assert =
        let c = #{ R(1, 1) :- if (true). };
        let r = query c select (x, y) from R(x, y);
        assertMemberOf((1, 1), r)

    @Test
    def testGuardConst02(): Unit \ Assert =
        let c = #{ R(1, 1) :- if (true and true). };
        let r = query c select (x, y) from R(x, y);
        assertMemberOf((1, 1), r)

    @Test
    def testGuardConst03(): Unit \ Assert =
        let c = #{ R(1, 1) :- if (1 == 1). };
        let r = query c select (x, y) from R(x, y);
        assertMemberOf((1, 1), r)

    @Test
    def testGuardConst04(): Unit \ Assert =
        let c = #{ R(1, 1) :- if (false). };
        let r = query c select (x, y) from R(x, y);
        assertFalse(Vector.memberOf((1, 1), r))

    @Test
    def testGuardConst05(): Unit \ Assert =
        let c = #{ R(1, 1) :- if (true and false). };
        let r = query c select (x, y) from R(x, y);
        assertFalse(Vector.memberOf((1, 1), r))

    @Test
    def testGuardConst06(): Unit \ Assert =
        let c = #{ R(1, 1) :- if (1 == 2). };
        let r = query c select (x, y) from R(x, y);
        assertFalse(Vector.memberOf((1, 1), r))

    @Test
    def testGuardConst07(): Unit \ Assert =
        let c = #{ R(1, 1) :- if (true), if (true and true), if (1 == 1). };
        let r = query c select (x, y) from R(x, y);
        assertMemberOf((1, 1), r)

    @Test
    def testGuardConst08(): Unit \ Assert =
        let c = #{ R(1, 1) :- if (List.isEmpty(Nil)). };
        let r = query c select (x, y) from R(x, y);
        assertMemberOf((1, 1), r)

    @Test
    def testGuardQuantVar01(): Unit \ Assert =
        let f = #{ A(1). A(2). B(1). B(2). };
        let c = #{ R(x, y) :- if (x == 1), A(x), B(y). };
        let r = query f, c select (x, y) from R(x, y);
        assertMemberOf((1, 1), r);
        assertMemberOf((1, 2), r)

    @Test
    def testGuardQuantVar02(): Unit \ Assert =
        let f = #{ A(1). A(2). B(1). B(2). };
        let c = #{ R(x, y) :- if (y == 1), A(x), B(y). };
        let r = query f, c select (x, y) from R(x, y);
        assertMemberOf((1, 1), r);
        assertMemberOf((2, 1), r)

    @Test
    def testGuardQuantVar03(): Unit \ Assert =
        let f = #{ A(1). A(2). B(1). B(2). };
        let c = #{ R(x, y) :- if (x == y), A(x), B(y). };
        let r = query f, c select (x, y) from R(x, y);
        assertMemberOf((1, 1), r);
        assertMemberOf((2, 2), r)

    @Test
    def testGuardQuantVar04(): Unit \ Assert =
        let f = #{ A(1). A(2). B(1). B(2). };
        let c = #{ R(x, y) :- if (x != y), A(x), B(y). };
        let r = query f, c select (x, y) from R(x, y);
        assertMemberOf((1, 2), r);
        assertMemberOf((2, 1), r)

    @Test
    def testGuardQuantVar05(): Unit \ Assert =
        let f = #{ A(1). A(2). B(1). B(2). };
        let c = #{ R(x, y) :- if ((x + y) == 2), A(x), B(y). };
        let r = query f, c select (x, y) from R(x, y);
        assertMemberOf((1, 1), r)

    @Test
    def testGuardQuantVar06(): Unit \ Assert =
        let f = #{ A(1). A(2). B(1). B(2). };
        let c = #{ R(x, y) :- if (List.exists(z -> z != 1, x :: y :: Nil)), A(x), B(y). };
        let r = query f, c select (x, y) from R(x, y);
        assertMemberOf((2, 2), r)

    @Test
    def testGuardQuantVar07(): Unit \ Assert =
        let f = #{ A(1). A(2). B(1). B(2). };
        let c = #{ R(x, y) :- if (List.exists(z -> z == x + 1, x :: y :: Nil)), A(x), B(y). };
        let r = query f, c select (x, y) from R(x, y);
        assertMemberOf((1, 2), r)

    @Test
    def testGuardCapturedVar01(): Unit \ Assert =
        let k = true;
        let c = #{ R(1, 1) :- if (k). };
        let r = query c select (x, y) from R(x, y);
        assertMemberOf((1, 1), r)

    @Test
    def testGuardCapturedVar02(): Unit \ Assert =
        let k = true;
        let c = #{ R(1, 1) :- if (k and true). };
        let r = query c select (x, y) from R(x, y);
        assertMemberOf((1, 1), r)

    @Test
    def testGuardCapturedVar03(): Unit \ Assert =
        let k = true;
        let l = true;
        let c = #{ R(1, 1) :- if (k and l). };
        let r = query c select (x, y) from R(x, y);
        assertMemberOf((1, 1), r)

    @Test
    def testGuardCapturedVar04(): Unit \ Assert =
        let f = x -> not x;
        let c = #{ R(1, 1) :- if (f(false)). };
        let r = query c select (x, y) from R(x, y);
        assertMemberOf((1, 1), r)

    @Test
    def testGuardCapturedVar05(): Unit \ Assert =
        let f = (x, y) -> x and y;
        let c = #{ R(1, 1) :- if (f(true, true)). };
        let r = query c select (x, y) from R(x, y);
        assertMemberOf((1, 1), r)

    @Test
    def testGuardCapturedVar06(): Unit \ Assert =
        let k = true;
        let f = (x, y) -> x and y;
        let c = #{ R(1, 1) :- if (f(k, k)). };
        let r = query c select (x, y) from R(x, y);
        assertMemberOf((1, 1), r)

    @Test
    def testGuardQuantAndCapturedVar01(): Unit \ Assert =
        let k = 1;
        let f = #{ A(1). A(2). B(1). B(2). };
        let c = #{ R(x, y) :- if (x == k), A(x), B(y). };
        let r = query f, c select (x, y) from R(x, y);
        assertMemberOf((1, 1), r);
        assertMemberOf((1, 2), r)

    @Test
    def testGuardQuantAndCapturedVar02(): Unit \ Assert =
        let k = 1;
        let g = (x, y) -> (x: Int32) == y;
        let f = #{ A(1). A(2). B(1). B(2). };
        let c = #{ R(x, y) :- if (g(k, x)), A(x), B(y). };
        let r = query f, c select (x, y) from R(x, y);
        assertMemberOf((1, 1), r);
        assertMemberOf((1, 2), r)

    @Test
    def testGuardQuantAndCapturedVar03(): Unit \ Assert =
        let k = 1;
        let f = #{ A(1). A(2). B(1). B(2). };
        let c = #{ R(x, y) :- if (List.exists(z -> z == k, x :: k :: y :: Nil)), A(x), B(y). };
        let r = query f, c select (x, y) from R(x, y);
        assertMemberOf((1, 1), r);
        assertMemberOf((1, 2), r)

}

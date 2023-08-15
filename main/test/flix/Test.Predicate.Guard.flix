mod Test.Predicate.Guard {

    @test
    def testGuardConst01(): Bool =
        let c = #{ R(1, 1) :- if true. };
        let r = query c select (x, y) from R(x, y);
        Vector.memberOf((1, 1), r)

    @test
    def testGuardConst02(): Bool =
        let c = #{ R(1, 1) :- if true and true. };
        let r = query c select (x, y) from R(x, y);
        Vector.memberOf((1, 1), r)

    @test
    def testGuardConst03(): Bool =
        let c = #{ R(1, 1) :- if 1 == 1. };
        let r = query c select (x, y) from R(x, y);
        Vector.memberOf((1, 1), r)

    @test
    def testGuardConst04(): Bool =
        let c = #{ R(1, 1) :- if false. };
        let r = query c select (x, y) from R(x, y);
        not Vector.memberOf((1, 1), r)

    @test
    def testGuardConst05(): Bool =
        let c = #{ R(1, 1) :- if true and false. };
        let r = query c select (x, y) from R(x, y);
        not Vector.memberOf((1, 1), r)

    @test
    def testGuardConst06(): Bool =
        let c = #{ R(1, 1) :- if 1 == 2. };
        let r = query c select (x, y) from R(x, y);
        not Vector.memberOf((1, 1), r)

    @test
    def testGuardConst07(): Bool =
        let c = #{ R(1, 1) :- if true, if true and true, if 1 == 1. };
        let r = query c select (x, y) from R(x, y);
        Vector.memberOf((1, 1), r)

    @test
    def testGuardConst08(): Bool =
        let c = #{ R(1, 1) :- if List.isEmpty(Nil). };
        let r = query c select (x, y) from R(x, y);
        Vector.memberOf((1, 1), r)

    @test
    def testGuardQuantVar01(): Bool =
        let f = #{ A(1). A(2). B(1). B(2). };
        let c = #{ R(x, y) :- if x == 1, A(x), B(y). };
        let r = query f, c select (x, y) from R(x, y);
        Vector.memberOf((1, 1), r) and Vector.memberOf((1, 2), r)

    @test
    def testGuardQuantVar02(): Bool =
        let f = #{ A(1). A(2). B(1). B(2). };
        let c = #{ R(x, y) :- if y == 1, A(x), B(y). };
        let r = query f, c select (x, y) from R(x, y);
        Vector.memberOf((1, 1), r) and Vector.memberOf((2, 1), r)

    @test
    def testGuardQuantVar03(): Bool =
        let f = #{ A(1). A(2). B(1). B(2). };
        let c = #{ R(x, y) :- if x == y, A(x), B(y). };
        let r = query f, c select (x, y) from R(x, y);
        Vector.memberOf((1, 1), r) and Vector.memberOf((2, 2), r)

    @test
    def testGuardQuantVar04(): Bool =
        let f = #{ A(1). A(2). B(1). B(2). };
        let c = #{ R(x, y) :- if x != y, A(x), B(y). };
        let r = query f, c select (x, y) from R(x, y);
        Vector.memberOf((1, 2), r) and Vector.memberOf((2, 1), r)

    @test
    def testGuardQuantVar05(): Bool =
        let f = #{ A(1). A(2). B(1). B(2). };
        let c = #{ R(x, y) :- if (x + y) == 2, A(x), B(y). };
        let r = query f, c select (x, y) from R(x, y);
        Vector.memberOf((1, 1), r)

    @test
    def testGuardQuantVar06(): Bool =
        let f = #{ A(1). A(2). B(1). B(2). };
        let c = #{ R(x, y) :- if List.exists(z -> z != 1, x :: y :: Nil), A(x), B(y). };
        let r = query f, c select (x, y) from R(x, y);
        Vector.memberOf((2, 2), r)

    @test
    def testGuardQuantVar07(): Bool =
        let f = #{ A(1). A(2). B(1). B(2). };
        let c = #{ R(x, y) :- if List.exists(z -> z == x + 1, x :: y :: Nil), A(x), B(y). };
        let r = query f, c select (x, y) from R(x, y);
        Vector.memberOf((1, 2), r)

    @test
    def testGuardCapturedVar01(): Bool =
        let k = true;
        let c = #{ R(1, 1) :- if k. };
        let r = query c select (x, y) from R(x, y);
        Vector.memberOf((1, 1), r)

    @test
    def testGuardCapturedVar02(): Bool =
        let k = true;
        let c = #{ R(1, 1) :- if k and true. };
        let r = query c select (x, y) from R(x, y);
        Vector.memberOf((1, 1), r)

    @test
    def testGuardCapturedVar03(): Bool =
        let k = true;
        let l = true;
        let c = #{ R(1, 1) :- if k and l. };
        let r = query c select (x, y) from R(x, y);
        Vector.memberOf((1, 1), r)

    @test
    def testGuardCapturedVar04(): Bool =
        let f = (x: Bool) -> not x;
        let c = #{ R(1, 1) :- if f(false). };
        let r = query c select (x, y) from R(x, y);
        Vector.memberOf((1, 1), r)

    @test
    def testGuardCapturedVar05(): Bool =
        let f = (x: Bool, y: Bool) -> x and y;
        let c = #{ R(1, 1) :- if f(true, true). };
        let r = query c select (x, y) from R(x, y);
        Vector.memberOf((1, 1), r)

    @test
    def testGuardCapturedVar06(): Bool =
        let k = true;
        let f = (x: Bool, y: Bool) -> x and y;
        let c = #{ R(1, 1) :- if f(k, k). };
        let r = query c select (x, y) from R(x, y);
        Vector.memberOf((1, 1), r)

    @test
    def testGuardQuantAndCapturedVar01(): Bool =
        let k = 1;
        let f = #{ A(1). A(2). B(1). B(2). };
        let c = #{ R(x, y) :- if x == k, A(x), B(y). };
        let r = query f, c select (x, y) from R(x, y);
        Vector.memberOf((1, 1), r) and Vector.memberOf((1, 2), r)

    @test
    def testGuardQuantAndCapturedVar02(): Bool =
        let k = 1;
        let g = (x: Int32, y: Int32) -> x: Int32 == y;
        let f = #{ A(1). A(2). B(1). B(2). };
        let c = #{ R(x, y) :- if g(k, x), A(x), B(y). };
        let r = query f, c select (x, y) from R(x, y);
        Vector.memberOf((1, 1), r) and Vector.memberOf((1, 2), r)

    @test
    def testGuardQuantAndCapturedVar03(): Bool =
        let k = 1;
        let f = #{ A(1). A(2). B(1). B(2). };
        let c = #{ R(x, y) :- if List.exists(z -> z == k, x :: k :: y :: Nil), A(x), B(y). };
        let r = query f, c select (x, y) from R(x, y);
        Vector.memberOf((1, 1), r) and Vector.memberOf((1, 2), r)

}

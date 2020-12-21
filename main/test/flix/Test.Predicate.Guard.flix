namespace Test/Predicate/Guard {

    @test
    def testGuardConst01(): Bool =
        let c = R(1, 1) :- if true.;
        let r = solve c;
        r |= R(1, 1).

    @test
    def testGuardConst02(): Bool =
        let c = R(1, 1) :- if true and true.;
        let r = solve c;
        r |= R(1, 1).

    @test
    def testGuardConst03(): Bool =
        let c = R(1, 1) :- if 1 == 1.;
        let r = solve c;
        r |= R(1, 1).

    @test
    def testGuardConst04(): Bool =
        let c = R(1, 1) :- if false.;
        let r = solve c;
        not (r |= R(1, 1).)

    @test
    def testGuardConst05(): Bool =
        let c = R(1, 1) :- if true and false.;
        let r = solve c;
        not (r |= R(1, 1).)

    @test
    def testGuardConst06(): Bool =
        let c = R(1, 1) :- if 1 == 2.;
        let r = solve c;
        not (r |= R(1, 1).)

    @test
    def testGuardConst07(): Bool =
        let c = R(1, 1) :- if true, if true and true, if 1 == 1.;
        let r = solve c;
        r |= R(1, 1).

    @test
    def testGuardConst08(): Bool =
        let c = R(1, 1) :- if isEmpty(Nil).;
        let r = solve c;
        r |= R(1, 1).

    @test
    def testGuardQuantVar01(): Bool =
        let f = #{ A(1). A(2). B(1). B(2). };
        let c = R(x, y) :- if x == 1, A(x), B(y).;
        let r = solve f <+> c;
        (r |= R(1, 1).) and (r |= R(1, 2).)

    @test
    def testGuardQuantVar02(): Bool =
        let f = #{ A(1). A(2). B(1). B(2). };
        let c = R(x, y) :- if y == 1, A(x), B(y).;
        let r = solve f <+> c;
        (r |= R(1, 1).) and (r |= R(2, 1).)

    @test
    def testGuardQuantVar03(): Bool =
        let f = #{ A(1). A(2). B(1). B(2). };
        let c = R(x, y) :- if x == y, A(x), B(y).;
        let r = solve f <+> c;
        (r |= R(1, 1).) and (r |= R(2, 2).)

    @test
    def testGuardQuantVar04(): Bool =
        let f = #{ A(1). A(2). B(1). B(2). };
        let c = R(x, y) :- if x != y, A(x), B(y).;
        let r = solve f <+> c;
        (r |= R(1, 2).) and (r |= R(2, 1).)

    @test
    def testGuardQuantVar05(): Bool =
        let f = #{ A(1). A(2). B(1). B(2). };
        let c = R(x, y) :- if (x + y) == 2, A(x), B(y).;
        let r = solve f <+> c;
        r |= R(1, 1).

    @test
    def testGuardQuantVar06(): Bool =
        let f = #{ A(1). A(2). B(1). B(2). };
        let c = R(x, y) :- if exists(z -> z != 1, x :: y :: Nil), A(x), B(y).;
        let r = solve f <+> c;
        r |= R(2, 2).

    @test
    def testGuardQuantVar07(): Bool =
        let f = #{ A(1). A(2). B(1). B(2). };
        let c = R(x, y) :- if exists(z -> z == x + 1, x :: y :: Nil), A(x), B(y).;
        let r = solve f <+> c;
        r |= R(1, 2).

    @test
    def testGuardCapturedVar01(): Bool =
        let k = true;
        let c = R(1, 1) :- if k.;
        let r = solve c;
        r |= R(1, 1).

    @test
    def testGuardCapturedVar02(): Bool =
        let k = true;
        let c = R(1, 1) :- if k and true.;
        let r = solve c;
        r |= R(1, 1).

    @test
    def testGuardCapturedVar03(): Bool =
        let k = true;
        let l = true;
        let c = R(1, 1) :- if k and l.;
        let r = solve c;
        r |= R(1, 1).

    @test
    def testGuardCapturedVar04(): Bool =
        let f = (x: Bool) -> not x;
        let c = R(1, 1) :- if f(false).;
        let r = solve c;
        r |= R(1, 1).

    @test
    def testGuardCapturedVar05(): Bool =
        let f = (x: Bool, y: Bool) -> x and y;
        let c = R(1, 1) :- if f(true, true).;
        let r = solve c;
        r |= R(1, 1).

    @test
    def testGuardCapturedVar06(): Bool =
        let k = true;
        let f = (x: Bool, y: Bool) -> x and y;
        let c = R(1, 1) :- if f(k, k).;
        let r = solve c;
        r |= R(1, 1).

    @test
    def testGuardQuantAndCapturedVar01(): Bool =
        let k = 1;
        let f = #{ A(1). A(2). B(1). B(2). };
        let c = R(x, y) :- if x == k, A(x), B(y).;
        let r = solve f <+> c;
        (r |= R(1, 1).) and (r |= R(1, 2).)

///    @test
///    def testGuardQuantAndCapturedVar02(): Bool =
///        let k = 1;
///        let g = (x: Int, y: Int) -> x == y;
///        let f = #{ A(1). A(2). B(1). B(2). };
///        let c = R(x, y) :- if g(k, x), A(x), B(y).;
///        let r = solve f <+> c;
///        (r |= R(1, 1).) and (r |= R(1, 2).)

    @test
    def testGuardQuantAndCapturedVar03(): Bool =
        let k = 1;
        let f = #{ A(1). A(2). B(1). B(2). };
        let c = R(x, y) :- if exists(z -> z == k, x :: k :: y :: Nil), A(x), B(y).;
        let r = solve f <+> c;
        (r |= R(1, 1).) and (r |= R(1, 2).)

    enum List[t] {
        case Nil,
        case Cons(t, List[t])
    }

    def isEmpty(xs: List[a]): Bool = match xs {
        case Nil => true
        case _ => false
    }

    def exists(f: a -> Bool, xs: List[a]): Bool = match xs {
        case Nil => false
        case x :: rs => if (f(x)) true else exists(f, rs)
    }

}

@test
def testGuardConst01(): Bool =
    let c = R(1, 1) :- if true.;
    let r = solve c;
    r |= R(1, 1).

@test
def testGuardConst02(): Bool =
    let c = R(1, 1) :- if true && true.;
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
    !(r |= R(1, 1).)

@test
def testGuardConst05(): Bool =
    let c = R(1, 1) :- if true && false.;
    let r = solve c;
    !(r |= R(1, 1).)

@test
def testGuardConst06(): Bool =
    let c = R(1, 1) :- if 1 == 2.;
    let r = solve c;
    !(r |= R(1, 1).)

@test
def testGuardConst07(): Bool =
    let c = R(1, 1) :- if true, if true && true, if 1 == 1.;
    let r = solve c;
    r |= R(1, 1).

@test
def testGuardConst08(): Bool =
    let c = R(1, 1) :- if List.isEmpty(Nil).;
    let r = solve c;
    r |= R(1, 1).

@test
def testGuardQuantVar01(): Bool =
    let f = #{ A(1). A(2). B(1). B(2). };
    let c = R(x, y) :- if x == 1, A(x), B(y).;
    let r = solve f <+> c;
    (r |= R(1, 1).) && (r |= R(1, 2).)

@test
def testGuardQuantVar02(): Bool =
    let f = #{ A(1). A(2). B(1). B(2). };
    let c = R(x, y) :- if y == 1, A(x), B(y).;
    let r = solve f <+> c;
    (r |= R(1, 1).) && (r |= R(2, 1).)

@test
def testGuardQuantVar03(): Bool =
    let f = #{ A(1). A(2). B(1). B(2). };
    let c = R(x, y) :- if x == y, A(x), B(y).;
    let r = solve f <+> c;
    (r |= R(1, 1).) && (r |= R(2, 2).)

@test
def testGuardQuantVar04(): Bool =
    let f = #{ A(1). A(2). B(1). B(2). };
    let c = R(x, y) :- if x != y, A(x), B(y).;
    let r = solve f <+> c;
    (r |= R(1, 2).) && (r |= R(2, 1).)

@test
def testGuardQuantVar05(): Bool =
    let f = #{ A(1). A(2). B(1). B(2). };
    let c = R(x, y) :- if (x + y) == 2, A(x), B(y).;
    let r = solve f <+> c;
    r |= R(1, 1).

@test
def testGuardQuantVar06(): Bool =
    let f = #{ A(1). A(2). B(1). B(2). };
    let c = R(x, y) :- if List.exists(z -> z != 1, x :: y :: Nil), A(x), B(y).;
    let r = solve f <+> c;
    r |= R(2, 2).

@test
def testGuardQuantVar07(): Bool =
    let f = #{ A(1). A(2). B(1). B(2). };
    let c = R(x, y) :- if List.exists(z -> z == x + 1, x :: y :: Nil), A(x), B(y).;
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
    let c = R(1, 1) :- if k && true.;
    let r = solve c;
    r |= R(1, 1).

@test
def testGuardCapturedVar03(): Bool =
    let k = true;
    let l = true;
    let c = R(1, 1) :- if k && l.;
    let r = solve c;
    r |= R(1, 1).

@test
def testGuardCapturedVar04(): Bool =
    let f = (x: Bool) -> !x;
    let c = R(1, 1) :- if f(false).;
    let r = solve c;
    r |= R(1, 1).

@test
def testGuardCapturedVar05(): Bool =
    let f = (x: Bool, y: Bool) -> x && y;
    let c = R(1, 1) :- if f(true, true).;
    let r = solve c;
    r |= R(1, 1).

@test
def testGuardCapturedVar06(): Bool =
    let k = true;
    let f = (x: Bool, y: Bool) -> x && y;
    let c = R(1, 1) :- if f(k, k).;
    let r = solve c;
    r |= R(1, 1).

@test
def testGuardQuantAndCapturedVar01(): Bool =
    let k = 1;
    let f = #{ A(1). A(2). B(1). B(2). };
    let c = R(x, y) :- if x == k, A(x), B(y).;
    let r = solve f <+> c;
    (r |= R(1, 1).) && (r |= R(1, 2).)

@test
def testGuardQuantAndCapturedVar02(): Bool =
    let k = 1;
    let g = (x: Int, y: Int) -> x == y;
    let f = #{ A(1). A(2). B(1). B(2). };
    let c = R(x, y) :- if g(k, x), A(x), B(y).;
    let r = solve f <+> c;
    (r |= R(1, 1).) && (r |= R(1, 2).)

@test
def testGuardQuantAndCapturedVar03(): Bool =
    let k = 1;
    let f = #{ A(1). A(2). B(1). B(2). };
    let c = R(x, y) :- if List.exists(z -> z == k, x :: k :: y :: Nil), A(x), B(y).;
    let r = solve f <+> c;
    (r |= R(1, 1).) && (r |= R(1, 2).)

pub rel A(x: Int)
pub rel B(x: Int)
pub rel R(x: Int, y: Int)

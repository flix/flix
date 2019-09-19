@test
def testUnion01(): Bool =
    let p = #{
        union #{ A(1). B(1). }.
        R(x, y) :- A(x), B(y).
    };
    let r = solve p;
    r |= R(1, 1).

@test
def testUnion02(): Bool =
    let p = #{
        union #{ A(1). }.
        union #{ B(1). }.
        R(x, y) :- A(x), B(y).
    };
    let r = solve p;
    r |= R(1, 1).

@test
def testUnion03(): Bool =
    let p = #{
        union #{ A(1). A(2). B(1). B(2). }.
        R(x, y) :- A(x), B(y).
    };
    let r = solve p;
    r |= R(1, 2).

@test
def testUnion04(): Bool =
    let p = #{
        union #{ A(1). A(2). }.
        union #{ B(1). B(2). }.
        R(x, y) :- A(x), B(y).
    };
    let r = solve p;
    r |= R(1, 2).

@test
def testUnion10(): Bool =
    let f = (_: Unit) -> A(1).;
    let p = #{
        union f().
    };
    let r = solve p;
    r |= A(1).

@test
def testUnion11(): Bool =
    let f = (_: Unit) -> A(1).;
    let g = (_: Unit) -> B(1).;
    let p = #{
        union f().
        union g().
        R(x, x) :- A(x), B(x).
    };
    let r = solve p;
    r |= R(1, 1).

@test
def testUnion12(): Bool =
    let f = x -> R(x, x).;
    let p = #{
        A(1). A(2). A(3).
        B(1). B(2). B(3).
        union f(x) :- A(x), B(x).
    };
    let r = solve p;
    r |= R(2, 2).

@test
def testUnion13(): Bool =
    let f = x -> R(x, x).;
    let p = #{
        A(1). A(2). A(3).
        B(1). B(2). B(3).
        union f(x) :- A(x), B(x).
        R(42, 42) :- R(1, 1).
    };
    let r = solve p;
    r |= R(42, 42).

@test
def testUnion14(): Bool =
    let f = (x: Int, y: Int) -> {
        let z = x + y;
        #{C(z).}
    };
    let p = #{
        A(1). A(2). A(3).
        B(3). B(5). B(6).
        union f(x, y) :- A(x), B(y).
    };
    let r = solve p;
    r |= C(9).

@test
def testUnion15(): Bool =
    let f = _ -> B(2).;
    let p = #{
        A(1). A(2). A(3).
        B(1).       B(3).
        union f(x) :- A(x), not B(x).
    };
    let r = solve p;
    r |= B(2).

pub rel A(x: Int)
pub rel B(x: Int)
pub rel C(x: Int)
pub rel R(x: Int, y: Int)

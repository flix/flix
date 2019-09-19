@test
def testUnion01(): Bool =
    let f = (_: Unit) -> A(1).;
    let p = #{
        union f().
    };
    let r = solve p;
    r |= A(1).

@test
def testUnion02(): Bool =
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
def testUnion03(): Bool =
    let f = x -> R(x, x).;
    let p = #{
        A(1). A(2). A(3).
        B(1). B(2). B(3).
        union f(x) :- A(x), B(x).
    };
    let r = solve p;
    r |= R(2, 2).

@test
def testUnion04(): Bool =
    let f = x -> R(x, x).;
    let p = #{
        A(1). A(2). A(3).
        B(1). B(2). B(3).
        union f(x) :- A(x), B(x).
        R(42, 42) :- R(1, 1).
    };
    let r = solve p;
    r |= R(42, 42).






pub rel A(x: Int)
pub rel B(x: Int)
pub rel R(x: Int, y: Int)

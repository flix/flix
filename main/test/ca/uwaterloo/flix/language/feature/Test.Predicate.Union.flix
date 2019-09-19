@test
def testUnion01(): Bool =
    let f = x -> R(x, x).;
    let p = #{
        A(1). A(2). A(3).
        B(1). B(2). B(3).
        union f(x) :- A(x), B(x).
    };
    let r = solve p;
    r |= R(2, 2).






pub rel A(x: Int)
pub rel B(x: Int)
pub rel R(x: Int, y: Int)

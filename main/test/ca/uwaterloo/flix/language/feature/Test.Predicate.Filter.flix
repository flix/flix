@test
def testFilter01(): Schema { R } = solve {
    R("a") :- success().
}

@test
def testFilter02(): Schema { R } = solve {
    R("a") :- failure().
}

@test
def testFilter03(): Schema { R } = solve {
    R("a") :- isZero(0).
}

@test
def testFilter04(): Schema { R } = solve {
    R("b") :- isZero(1).
}

@test
def testFilter05(): Schema { R } = solve {
    R("c") :- isZero(2).
}

@test
def testFilter06(): Schema { R } = solve {
    R("a") :- isZero(0).
    R("b") :- isZero(1).
    R("c") :- isZero(2).
}

@test
def testFilter07(): Schema { R } = solve {
    R("a") :- isZero(0), isZero(1), isZero(2).
}

@test
def testFilter08(): Schema { A, R } = solve {
    A(0). A(1). A(2).
    R("a") :- isZero(x), A(x).
}

// TODO: Bugged: Problem with currying.
//@test
//def testFilter09(): Schema { A, B, R } = solve {
//    A(0). A(1). A(2).
//    B(0). B(1). B(2).
//    R("a") :- isSame(x, y), A(x), B(y).
//}
//
//@test
//def testFilter10(): Schema { A, B, R } = solve {
//    A(0). A(1). A(2).
//    B(0). B(1). B(2).
//    R("a") :- isZero(x), isZero(y), isSame(x, y), A(x), B(y).
//}

@test
def testFilter11(): Schema { R } = solve {
    R("a") :- isZero(increment(0)).
}

@test
def testFilter12(): Schema { R } = solve {
    R("a") :- isZero(increment(decrement(0))).
}

@test
def testFilter13(): Schema { R } = solve {
    R("a") :- isZero(increment(decrement(decrement(0)))).
}

//// TODO: Bugged
//@test
//def testFilter14(): Schema { R } = solve {
//    A(0). A(1). A(2).
//    R("a") :- isZero(increment(decrement(x))), A(x).
//}
//// TODO: Bugged
//@test
//def testFilter15(): Schema { R } = solve {
//    A(0). A(1). A(2).
//    R("a") :- isSame(increment(x), increment(y)), A(x), B(y).
//}

rel A(x: Int)
rel R(x: Str)

def success(): Bool = true
def failure(): Bool = false
def isZero(x: Int): Bool = x == 0
def increment(x: Int): Int = x + 1
def decrement(x: Int): Int = x - 1

@test
def testFilter01(): Bool = check {
    R("a") :- success().
}

@test
def testFilter02(): Bool = check {
    R("a") :- failure().
}

@test
def testFilter03(): Bool = check {
    R("a") :- isZero(0).
}

@test
def testFilter04(): Bool = check {
    R("b") :- isZero(1).
}

@test
def testFilter05(): Bool = check {
    R("c") :- isZero(2).
}

@test
def testFilter06(): Bool = check {
    R("a") :- isZero(0).
    R("b") :- isZero(1).
    R("c") :- isZero(2).
}

@test
def testFilter07(): Bool = check {
    R("a") :- isZero(0), isZero(1), isZero(2).
}

@test
def testFilter08(): Bool = check {
    A(0). A(1). A(2).
    R("a") :- isZero(x), A(x).
}

@test
def testFilter09(): Bool = check {
    A(0). A(1). A(2).
    B(0). B(1). B(2).
    R("a") :- isSame(x, y), A(x), B(y).
}

@test
def testFilter10(): Bool = check {
    A(0). A(1). A(2).
    B(0). B(1). B(2).
    R("a") :- isZero(x), isZero(y), isSame(x, y), A(x), B(y).
}

//// TODO: Bugged
//@test
//def testFilter11(): Bool = check {
//    R("a") :- isZero(increment(0)).
//}
//
//// TODO: Bugged
//@test
//def testFilter12(): Bool = check {
//    R("a") :- isZero(increment(decrement(0))).
//}
//
//// TODO: Bugged
//@test
//def testFilter13(): Bool = check {
//    R("a") :- isZero(increment(decrement(decrement(0)))).
//}
//// TODO: Bugged
//@test
//def testFilter14(): Bool = check {
//    A(0). A(1). A(2).
//    R("a") :- isZero(increment(decrement(x))), A(x).
//}
//// TODO: Bugged
//@test
//def testFilter15(): Bool = check {
//    A(0). A(1). A(2).
//    R("a") :- isSame(increment(x), increment(y)), A(x), B(y).
//}

rel A(x: Int)
rel B(x: Int)
rel R(x: Str)

def success(): Bool = true
def failure(): Bool = false
def isZero(x: Int): Bool = x == 0
def isSame(x: Int, y: Int): Bool = x == y
def increment(x: Int): Int = x + 1
def decrement(x: Int): Int = x - 1

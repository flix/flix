@test
def testTransfer01(): Bool = check {
    R(zer()).
    R(one()).
    R(two()).
}

@test
def testTransfer02(): Bool = check {
    R(increment(1)).
    R(decrement(2)).
}

@test
def testTransfer03(): Bool = check {
    R(increment(decrement(1))).
    R(decrement(increment(2))).
}

@test
def testTransfer04(): Bool = check {
    A(0). A(1). A(3).
    B(1). B(2). B(3).

    R(increment(x)) :- A(x).
    R(decrement(x)) :- B(x).
}

@test
def testTransfer05(): Bool = check {
    A(0). A(1). A(3).
    B(1). B(2). B(3).

    R(increment(decrement(x))) :- A(x), B(x).
}


def zer(): Int = 0
def one(): Int = 1
def two(): Int = 2
def increment(x: Int): Int = x + 1
def decrement(x: Int): Int = x - 1

rel A(x: Int)
rel B(x: Int)
rel R(x: Int)


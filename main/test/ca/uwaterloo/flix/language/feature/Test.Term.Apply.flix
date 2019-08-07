@test
def testTransfer01(): #{ R } = solve {
    R(zer()).
    R(one()).
    R(two()).
}

@test
def testTransfer02(): #{ R } = solve {
    R(inc(1)).
    R(dec(2)).
}

@test
def testTransfer03(): #{ R } = solve {
    R(inc(dec(1))).
    R(dec(inc(2))).
}

@test
def testTransfer04(): #{ A, B, R } = solve {
    A(0). A(1). A(3).
    B(1). B(2). B(3).

    R(inc(x)) :- A(x).
    R(dec(x)) :- B(x).
}

@test
def testTransfer05(): #{ A, B, R } = solve {
    A(0). A(1). A(3).
    B(1). B(2). B(3).

    R(inc(dec(x))) :- A(x), B(x).
}

@test
def testTransfer06(): #{ A, B, R } = solve {
    A(0). A(1). A(3).
    B(1). B(2). B(3).

    R(add(x, y)) :- A(x), B(y).
}

@test
def testTransfer07(): #{ A, B, R } = solve {
    A(0). A(1). A(3).
    B(1). B(2). B(3).

    R(add(x)(y)) :- A(x), B(y).
}

@test
def testTransfer08(): #{ A, B, R } = solve {
    A(0). A(1). A(3).
    B(1). B(2). B(3).

    R(add(inc(x), dec(y))) :- A(x), B(y).
}

@test
def testTransfer09(): #{ R } = solve {
    R(identity(42)).
}

@test
def testTransfer10(): #{ R } = solve {
    R(fst((1, 2))).
    R(snd((1, 2))).
}

@test
def testTransfer11(): #{ R } = solve {
    R(constant(42)(42)).
}

def zer(): Int = 0
def one(): Int = 1
def two(): Int = 2
def inc(x: Int): Int = x + 1
def dec(x: Int): Int = x - 1
def add(x: Int, y: Int): Int = x + y

rel A(x: Int)
rel B(x: Int)
rel R(x: Int)

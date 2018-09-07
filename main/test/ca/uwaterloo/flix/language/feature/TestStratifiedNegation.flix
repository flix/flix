rel A(x: Int)
rel B(x: Int)
rel C(x: Int)
rel D(x: Int)
rel E(x: Int)
rel R(x: Int)

@test
def testStratification01(): Bool = check {
    A(1). A(2). A(3).

    B(3).

    R(x) :- A(x), not B(x).
}

//@test // TODO
def testStratification02(): Bool = check {
    A(1). A(2). A(3).

    B(3).

    R(x) :- not A(x), B(x).
}

@test
def testStratification03(): Bool = check {
    A(1). A(2). A(3).

    B(3).

    C(3).

    R(x) :- A(x), not B(x), not C(x).
}

//@test // TODO
def testStratification04(): Bool = check {
    A(1). A(2). A(3).

    B(3).

    C(3).

    R(x) :- not A(x), B(x), not C(x).
}

@test
def testStratification05(): Bool = check {
    A(1). A(2).
    B(3).

    C(x) :- A(x).
    C(x) :- B(x).
    R(x) :- C(x), not A(x).
}

@test
def testStratification06(): Bool = check {
    A(1). A(2).
    B(3).

    C(x) :- A(x).
    C(x) :- B(x).
    D(x) :- C(x), not A(x).
    R(x) :- A(x), not D(x).
}

@test
def testStratification07(): Bool = check {
    A(1). A(2).
    B(3).

    C(x) :- A(x).
    C(x) :- B(x).
    D(x) :- C(x), not A(x).
    E(x) :- B(x), not D(x).
    R(x) :- E(x).
}

@test
def testStratification08(): Bool = check {
    A(1).
    A(2).
    A(3).

    false :- not A(1).
    false :- not A(2).
    false :- not A(3).
}

@test
def testStratification09(): Bool = check {
    A(1). A(2).
    B(1).  B(2).

    false :- not A(1), not B(1).
    false :- not A(2), not B(2).
}

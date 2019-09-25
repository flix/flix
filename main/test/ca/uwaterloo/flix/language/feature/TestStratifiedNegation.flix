rel A(x: Int)
rel B(x: Int)
rel C(x: Int)
rel D(x: Int)
rel E(x: Int)
rel R(x: Int)
rel N()

@test
def testStratification01(): #{ A, B, R } = solve #{
    A(1). A(2). A(3).

    B(3).

    R(x) :- A(x), not B(x).
}

@test
def testStratification02(): #{ A, B, R } = solve #{
    A(1). A(2). A(3).

    B(3).

    R(x) :- not A(x), B(x).
}

@test
def testStratification03(): #{ A, B, C, R } = solve #{
    A(1). A(2). A(3).

    B(3).

    C(3).

    R(x) :- A(x), not B(x), not C(x).
}

@test
def testStratification04(): #{ A, B, C, R } = solve #{
    A(1). A(2). A(3).

    B(3).

    C(3).

    R(x) :- not A(x), B(x), not C(x).
}

@test
def testStratification05(): #{ A, B, C, R } = solve #{
    A(1). A(2).
    B(3).

    C(x) :- A(x).
    C(x) :- B(x).
    R(x) :- C(x), not A(x).
}

@test
def testStratification06(): #{ A, B, C, D, R } = solve #{
    A(1). A(2).
    B(3).

    C(x) :- A(x).
    C(x) :- B(x).
    D(x) :- C(x), not A(x).
    R(x) :- A(x), not D(x).
}

@test
def testStratification07(): #{ A, B, C, D, E, R } = solve #{
    A(1). A(2).
    B(3).

    C(x) :- A(x).
    C(x) :- B(x).
    D(x) :- C(x), not A(x).
    E(x) :- B(x), not D(x).
    R(x) :- E(x).
}

@test
def testStratification08(): #{ A, N } = solve #{
    A(1).
    A(2).
    A(3).

    N() :- not A(1).
    N() :- not A(2).
    N() :- not A(3).
}

@test
def testStratification09(): #{ A, B, N } = solve #{
    A(1). A(2).
    B(1).  B(2).

    N() :- not A(1), not B(1).
    N() :- not A(2), not B(2).
}

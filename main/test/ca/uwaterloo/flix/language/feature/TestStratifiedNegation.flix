namespace StratifiedNegation {

    namespace Test01 {
        rel A(x: Int)
        rel B(x: Int)
        rel R(x: Int)

        A(1).
        A(2).
        A(3).

        B(3).

        R(x) :- A(x), not B(x).
    }

    namespace Test02 {
        rel A(x: Int)
        rel B(x: Int)
        rel R(x: Int)

        A(1).
        A(2).
        A(3).

        B(3).

        R(x) :- not A(x), B(x).
    }

    namespace Test03 {
        rel A(x: Int)
        rel B(x: Int)
        rel C(x: Int)
        rel R(x: Int)

        A(1).
        A(2).
        A(3).

        B(3).

        C(3).

        R(x) :- A(x), not B(x), not C(x).
    }

    namespace Test04 {
        rel A(x: Int)
        rel B(x: Int)
        rel C(x: Int)
        rel R(x: Int)

        A(1).
        A(2).
        A(3).

        B(3).

        C(3).

        R(x) :- not A(x), B(x), not C(x).
    }

    namespace Test05 {
        rel A(x: Int, y: Int)
        rel B(x: Int, y: Int)
        rel R(x: Int, y: Int)

        A(1, 1).
        A(2, 2).
        A(3, 3).

        B(3, 3).

        R(x, y) :- A(x, y), not B(x, y).
    }

    namespace Test06 {
        rel A(x: Int)
        rel B(x: Int)
        rel C(x: Int)
        rel R(x: Int)

        A(1).
        A(2).
        B(3).

        C(x) :- A(x).
        C(x) :- B(x).
        R(x) :- C(x), not A(x).
    }

    namespace Test07 {
        rel A(x: Int)
        rel B(x: Int)
        rel C(x: Int)
        rel D(x: Int)
        rel R(x: Int)

        A(1).
        A(2).
        B(3).

        C(x) :- A(x).
        C(x) :- B(x).
        D(x) :- C(x), not A(x).
        R(x) :- A(x), not D(x).
    }

    namespace Test08 {
        rel A(x: Int)
        rel B(x: Int)
        rel C(x: Int)
        rel D(x: Int)
        rel E(x: Int)
        rel R(x: Int)

        A(1).
        A(2).
        B(3).

        C(x) :- A(x).
        C(x) :- B(x).
        D(x) :- C(x), not A(x).
        E(x) :- B(x), not D(x).
        R(x) :- E(x).
    }

    namespace Test09 {
        rel A(x: Int)

        A(1).
        A(2).
        A(3).

        false :- not A(1).
        false :- not A(2).
        false :- not A(3).
    }

    namespace Test10 {
        rel A(x: Int)
        rel B(x: Int)

        A(1).
        A(2).
        B(1).
        B(2).

        false :- not A(1), not B(1).
        false :- not A(2), not B(2).
    }

}

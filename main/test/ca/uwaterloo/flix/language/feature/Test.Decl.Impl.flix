namespace Test01 {

    class X[a]

    impl X[Bool]

    impl X[Char]

    class Y[a, b]

    impl Y[Bool, Bool]

    impl Y[Bool, Char]

    class Z[a, b, c]

    impl Z[Bool, Bool, Bool]

    impl Z[Bool, Char, Str]

}

namespace Test02 {

    class X[a] {
        def f(x: a): Bool
    }

    impl X[Bool] {
        def f(x: Bool): Bool = true
    }

    impl X[Char] {
        def f(x: Char): Bool = true
    }

    impl X[(Bool, Char)] {
        def f(x: (Bool, Char)): Bool = true
    }

    impl X[Option[Bool]] {
        def f(x: Option[Bool]): Bool = true
    }

    class Y[a, b] {
        def g(x: a, y: b): Bool
    }

    impl Y[Bool, Bool] {
        def g(x: Bool, y: Bool): Bool = true
    }

    impl Y[Char, Char] {
        def g(x: Char, y: Char): Bool = true
    }

    impl Y[(Bool, Bool), (Char, Char)] {
       def g(x: (Bool, Bool), y: (Char, Char)): Bool = true
    }

    impl Y[Option[Bool], List[Char]] {
       def g(x: Option[Bool], y: List[Char]): Bool = true
    }

}

namespace Test03 {

    class X[a]

    class Y[a]

    class Z[a]

    class W[a] <= X[a], Y[a], Z[a]

    impl X[Bool]

    impl Y[Bool]

    impl Z[Bool]

    impl W[Bool]
}

namespace Test04 {

    class X[a]

    class Y[a]

    class Z[a]

    impl Z[Bool] <= X[Bool], Y[Bool]

    impl Z[(Bool, Char)] <= X[Bool], Y[Char]

    impl Z[(a, b)] <= X[a], Y[b]

    impl Z[Option[a]] <= X[a], Y[a]

}

namespace Test05 {

    class X[a]

    class Y[a]

    class Z[a]

    impl Z[Bool] <= X[Bool], not Y[Bool]

    impl Z[Bool] <= not X[Bool], Y[Bool]

}

namespace Test06 {

    class X[a]

    class Y[a]

    class Z[a]

    disallow X[a], Y[a]

    disallow X[a], not Y[a]

    disallow X[a], Y[a], Z[a]

}

@test
def empty(): Bool = true

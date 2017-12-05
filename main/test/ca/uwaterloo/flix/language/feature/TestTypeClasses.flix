namespace Test01 {

    class X[a]

    class Y[a, b]

    class Z[a, b, c]

}

namespace Test02 {

    class X[a] {
        def f(x: a): Bool
    }

    class Y[a, b] {
        def f(x: a): Bool
        def g(x: a, y: b): Bool
    }

    class Z[a, b, c] {
        def f(x: a): Bool
        def g(x: a, y: b): Bool
        def h(x: a, y: b, z: c): Bool
    }

}

namespace Test03 {

    class X[a] {
        def f(x: a): a
    }

    class Y[a, b] {
        def f(x: a): a
        def g(x: a, y: b): b
    }

    class Z[a, b, c] {
        def f(x: a): a
        def g(x: a, y: b): b
        def h(x: a, y: b, z: c): c
    }

}

namespace Test04 {

    class W[a, b, c] {
        def f[d, e, f](x: a, y: f): Bool
    }

}

namespace Test05 {

    class X[a] {
        law f(): Bool = ???
        law g(h: a -> a): Bool = ???
    }

}

namespace Test06 {

    namespace A {
        class X[a] {
            def f(x: a): Bool
        }
    }

    namespace B {
        class X[a] {
            def f(x: a): Bool
        }
    }

    namespace A/B {
        class X[a] {
            def f(x: a): Bool
        }
    }

    namespace A/B/C {
        class X[a] {
            def f(x: a): Bool
        }
    }

}

@test
def empty(): Bool = true

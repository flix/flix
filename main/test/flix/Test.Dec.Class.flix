namespace Test01 {

    class X[a]

    class Y[a, b]

    class Z[a, b, c]

}

namespace Test02 {

    class X[a] {
        def f(x: a): Bool
    }

}

namespace Test03 {
    class X[a] {
        def f(x: a): Bool
    }
    // TODO instance
    pub def g(x: Int): Bool = f(x)
}

namespace Test04 {
    class C[a] {
        def f(x: a): Bool
    }

    class D[a] {
        def g(x: a): Bool
    }

    pub def h[a : C : D](x: a): Bool = f(x) && g(x)
}

@test
def empty(): Bool = true

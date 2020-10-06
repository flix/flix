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

    instance X[Int] {
        def f(x: Int): Bool = x == 0
    }

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

namespace Test05 {
    class C[a]

    instance C[List[a]] with [a : C, Option[a] : C]
}

@test
def empty(): Bool = true

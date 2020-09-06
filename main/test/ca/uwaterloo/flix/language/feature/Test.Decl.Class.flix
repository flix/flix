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

@test
def empty(): Bool = true

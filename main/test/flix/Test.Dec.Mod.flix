mod A {
    pub def f(): Bool = true
    pub def g(): Bool = true

    mod B {
        pub def f(): Bool = true
        pub def g(): Bool = true

        mod C {
            pub def f(): Bool = true
            pub def g(): Bool = true
        }
    }

    // `B` is private to `A`, so `A` itself can reach into it.
    pub def h(): Bool = B.f() and B.g()
}

mod X {
    pub def f(): Bool = true

    mod Y {
        pub def f(): Bool = X.f()

        mod Z {
            pub def f(): Bool = X.Y.f()
        }
    }

    // `W` is a sibling of `Y`. Both are declared in `X`, so `W` can reach `Y`.
    mod W {
        pub def f(): Bool = X.Y.f()
    }

    pub def w(): Bool = W.f()
}

mod Test.Dec.Mod {

    @CompileTest
    def testMod01(): Bool = A.f()

    @CompileTest
    def testMod02(): Bool = A.h()

    @CompileTest
    def testMod03(): Bool = X.w()

    @CompileTest
    def testMod04(): Bool = A.g()

}

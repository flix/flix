mod A {
    pub def f(): Bool = true
}

mod A.B {
    pub def f(): Bool = true
}

mod A.B.C {
    pub def f(): Bool = true
}

mod A {
    mod B {
        mod C {
            pub def g(): Bool = true
        }
        pub def g(): Bool = true
    }
    pub def g(): Bool = true
}

mod X {
    mod Y {
        mod Z {
            pub def f(): Bool = X.Y.f()
        }
        pub def f(): Bool = X.f()
    }
    pub def f(): Bool = true
}

mod Test.Dec.Mod {

    @CompileTest
    def testMod01(): Bool = A.f()

    @CompileTest
    def testMod04(): Bool = A.g()

}

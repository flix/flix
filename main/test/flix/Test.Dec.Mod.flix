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
}

mod X {
    pub def f(): Bool = true

    mod Y {
        pub def f(): Bool = X.f()

        mod Z {
            pub def f(): Bool = X.Y.f()
        }
    }
}

mod Test.Dec.Mod {

    @CompileTest
    def testMod01(): Bool = A.f()

    @CompileTest
    def testMod02(): Bool = A.B.f()

    @CompileTest
    def testMod03(): Bool = A.B.C.f()

    @CompileTest
    def testMod04(): Bool = A.g()

    @CompileTest
    def testMod05(): Bool = A.B.g()

    @CompileTest
    def testMod06(): Bool = A.B.C.g()

    @CompileTest
    def testMod07(): Bool = X.Y.Z.f()

}

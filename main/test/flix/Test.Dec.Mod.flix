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

    @Test
    def testMod01(): Bool = A.f()

    @Test
    def testMod02(): Bool = A.B.f()

    @Test
    def testMod03(): Bool = A.B.C.f()

    @Test
    def testMod04(): Bool = A.g()

    @Test
    def testMod05(): Bool = A.B.g()

    @Test
    def testMod06(): Bool = A.B.C.g()

    @Test
    def testMod07(): Bool = X.Y.Z.f()

}

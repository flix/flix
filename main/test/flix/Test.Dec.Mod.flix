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

    @test
    def testMod01(): Bool = A.f()

    @test
    def testMod02(): Bool = A.B.f()

    @test
    def testMod03(): Bool = A.B.C.f()

    @test
    def testMod04(): Bool = A.g()

    @test
    def testMod05(): Bool = A.B.g()

    @test
    def testMod06(): Bool = A.B.C.g()

    @test
    def testMod07(): Bool = X.Y.Z.f()

}

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
    def testmod01(): Bool = A.f()

    @test
    def testmod02(): Bool = A.B.f()

    @test
    def testmod03(): Bool = A.B.C.f()

    @test
    def testmod04(): Bool = A.g()

    @test
    def testmod05(): Bool = A.B.g()

    @test
    def testmod06(): Bool = A.B.C.g()

    @test
    def testmod07(): Bool = X.Y.Z.f()

}

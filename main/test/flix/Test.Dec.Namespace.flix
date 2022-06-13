namespace A {
    pub def f(): Bool = true
}

namespace A/B {
    pub def f(): Bool = true
}

namespace A/B/C {
    pub def f(): Bool = true
}

namespace A {
    namespace B {
        namespace C {
            pub def g(): Bool = true
        }
        pub def g(): Bool = true
    }
    pub def g(): Bool = true
}

namespace X {
    namespace Y {
        namespace Z {
            pub def f(): Bool = X/Y.f()
        }
        pub def f(): Bool = X.f()
    }
    pub def f(): Bool = true
}

namespace Test/Dec/Namespace {

    @test
    def testNamespace01(): Bool = A.f()

    @test
    def testNamespace02(): Bool = A/B.f()

    @test
    def testNamespace03(): Bool = A/B/C.f()

    @test
    def testNamespace04(): Bool = A.g()

    @test
    def testNamespace05(): Bool = A/B.g()

    @test
    def testNamespace06(): Bool = A/B/C.g()

    @test
    def testNamespace07(): Bool = X/Y/Z.f()

}

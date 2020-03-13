namespace A {
    pub def f(): Bool = true

    @test
    def testNamespace01(): Bool =
        use A.f;
        f()

    @test
    def testNamespace02(): Bool =
        use B.f;
        f()

    @test
    def testNamespace03(): Bool =
        use C.f;
        f()
}

namespace B {
    pub def f(): Bool = true

    @test
    def testNamespace04(): Bool =
        use A.{f => fa};
        fa()

    @test
    def testNamespace05(): Bool =
        use B.{f => fb};
        fb()

    @test
    def testNamespace06(): Bool =
        use C.{f => fc};
        fc()

}

namespace C {
    pub def f(): Bool = true

    @test
    def testNamespace07(): Bool =
        use A.{f => fa};
        use B.{f => fb};
        use C.{f => fc};
        fa() && fb() && fc()
}

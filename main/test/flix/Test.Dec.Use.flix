namespace A {
    pub def f(): Int = 1

    @test
    def testNamespace01(): Bool =
        use A.f;
        f() == 1

    @test
    def testNamespace02(): Bool =
        use B.f;
        f() == 2

    @test
    def testNamespace03(): Bool =
        use C.f;
        f() == 3
}

namespace B {
    pub def f(): Int = 2

    @test
    def testNamespace04(): Bool =
        use A.{f => fa};
        fa() == 1

    @test
    def testNamespace05(): Bool =
        use B.{f => fb};
        fb() == 2

    @test
    def testNamespace06(): Bool =
        use C.{f => fc};
        fc() == 3

}

namespace C {
    pub def f(): Int = 3

    @test
    def testNamespace07(): Bool =
        use A.{f => fa};
        use B.{f => fb};
        use C.{f => fc};
        (fa() + fb() + fc()) == 6
}

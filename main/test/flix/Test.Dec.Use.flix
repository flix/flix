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

pub def f(): Int = 4
namespace X {
    pub def f(): Int = 5
    namespace Y {
        pub def f(): Int = 6
        namespace Z {
            pub def f(): Int = 7
        }
    }
}

@test
def testNamespace08(): Bool =
    f() == 4

@test
def testNamespace09(): Bool =
    use X.f;
    f() == 5

@test
def testNamespace10(): Bool =
    use X/Y.f;
    f() == 6

@test
def testNamespace11(): Bool =
    use X/Y/Z.f;
    f() == 7

@test
def testNamespace12(): Bool =
    use X.{f => fx};
    use X/Y.{f => fxy};
    use X/Y/Z.{f => fxyz};
    (f() + fx() + fxy() + fxyz()) == 22


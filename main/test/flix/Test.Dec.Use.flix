use A.f
use B.{f => fb}
use C.{f => fc}
use X.g
use X/Y.{g => gxy}
use X/Y/Z.{g => gxyz}

@test
def testNamespace01(): Bool =
    f() == 1

@test
def testNamespace02(): Bool =
    fb() == 2

@test
def testNamespace03(): Bool =
    fc() == 3

@test
def testNamespace04(): Bool =
    g() == 5

@test
def testNamespace05(): Bool =
    gxy() == 6

@test
def testNamespace06(): Bool =
    gxyz() == 7

namespace A {
    pub def f(): Int = 1

    @test
    def testNamespace07(): Bool =
        use A.f;
        f() == 1

    @test
    def testNamespace08(): Bool =
        use B.f;
        f() == 2

    @test
    def testNamespace09(): Bool =
        use C.f;
        f() == 3
}

namespace B {
    pub def f(): Int = 2

    @test
    def testNamespace10(): Bool =
        use A.{f => fa};
        fa() == 1

    @test
    def testNamespace11(): Bool =
        use B.{f => fb};
        fb() == 2

    @test
    def testNamespace12(): Bool =
        use C.{f => fc};
        fc() == 3

}

namespace C {
    pub def f(): Int = 3

    @test
    def testNamespace13(): Bool =
        use A.{f => fa};
        use B.{f => fb};
        use C.{f => fc};
        (fa() + fb() + fc()) == 6
}

pub def g(): Int = 4
namespace X {
    pub def g(): Int = 5
    namespace Y {
        pub def g(): Int = 6
        namespace Z {
            pub def g(): Int = 7
        }
    }
}

@test
def testNamespace14(): Bool =
    use X.g;
    g() == 5

@test
def testNamespace15(): Bool =
    use X/Y.g;
    g() == 6

@test
def testNamespace16(): Bool =
    use X/Y/Z.g;
    g() == 7

@test
def testNamespace17(): Bool =
    use X.{g => gx};
    use X/Y.{g => gxy};
    use X/Y/Z.{g => gxyz};
    (gx() + gxy() + gxyz()) == 18


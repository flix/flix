namespace A {
    pub def f(): Int32 = 1

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
    pub def f(): Int32 = 2

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
    pub def f(): Int32 = 3

    @test
    def testNamespace07(): Bool =
        use A.{f => fa};
        use B.{f => fb};
        use C.{f => fc};
        (fa() + fb() + fc()) == 6
}

namespace D {
    use A.f;
    use B.{f => fb};
    use X/Y.g;
    use X/Y/Z.{g => gz};

    @test
    def testNamespace08(): Bool = f() == 1

    @test
    def testNamespace09(): Bool = fb() == 2

    @test
    def testNamespace10(): Bool = g() == 6

    @test
    def testNamespace11(): Bool = gz() == 7
}

pub def g(): Int32 = 4
namespace X {
    pub def g(): Int32 = 5
    namespace Y {
        pub def g(): Int32 = 6
        namespace Z {
            pub def g(): Int32 = 7
        }
    }
}

@test
def testNamespace12(): Bool =
    use X.g;
    g() == 5

@test
def testNamespace13(): Bool =
    use X/Y.g;
    g() == 6

@test
def testNamespace14(): Bool =
    use X/Y/Z.g;
    g() == 7

@test
def testNamespace15(): Bool =
    use X.{g => gx};
    use X/Y.{g => gxy};
    use X/Y/Z.{g => gxyz};
    (gx() + gxy() + gxyz()) == 18

namespace Op {
    pub def <><(left: String, right: String): String  = "${left} fish ${right}"
    pub def prefish(left: String, right: String): String = "${left} fish ${right}"
}

namespace Test/Use/Def {

    @test
    def testNamespace16(): Bool =
        use Op.{<><};
        "one" <>< "two" == "one fish two"

    @test
    def testNamespace17(): Bool =
        use Op.{<>< => ><>};
        "one" ><> "two" == "one fish two"

    @test
    def testNamespace18(): Bool =
        use Op.{<>< => fish};
        fish("one", "two") == "one fish two"

    @test
    def testNamespace19(): Bool =
        use Op.{prefish => ><>};
        "one" ><> "two" == "one fish two"

}

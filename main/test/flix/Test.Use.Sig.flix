namespace A {
    pub class FClass[a] {
        def f(): a
    }

    instance FClass[Int] {
        def f(): Int = 1
    }

    @test
    def testNamespace01(): Bool =
        use A.FClass.f;
        f() == 1

    @test
    def testNamespace02(): Bool =
        use B.FClass.f;
        f() == 2

    @test
    def testNamespace03(): Bool =
        use C.FClass.f;
        f() == 3
}

namespace B {
    pub class FClass[a] {
        def f(): a
    }

    instance FClass[Int] {
        def f(): Int = 2
    }

    @test
    def testNamespace04(): Bool =
        use A.FClass.{f => fa};
        fa() == 1

    @test
    def testNamespace05(): Bool =
        use B.FClass.{f => fb};
        fb() == 2

    @test
    def testNamespace06(): Bool =
        use C.FClass.{f => fc};
        fc() == 3

}

namespace C {
    pub class FClass[a] {
        def f(): a
    }

    instance FClass[Int] {
        def f(): Int = 3
    }

    @test
    def testNamespace07(): Bool =
        use A.FClass.{f => fa};
        use B.FClass.{f => fb};
        use C.FClass.{f => fc};
        (fa() + fb() + fc()) == 6
}

namespace D {
    use A.FClass.f;
    use B.FClass.{f => fb};
    use X/Y.GClass.g;
    use X/Y/Z.GClass.{g => gz};

    @test
    def testNamespace08(): Bool = f() == 1

    @test
    def testNamespace09(): Bool = fb() == 2

    @test
    def testNamespace10(): Bool = g() == 6

    @test
    def testNamespace11(): Bool = gz() == 7
}

pub class GClass[a] {
    def g(): a
}

instance GClass[Int] {
    def g(): Int = 4
}

namespace X {
    pub class GClass[a] {
        def g(): a
    }

    instance GClass[Int] {
        def g(): Int = 5
    }

    namespace Y {
        pub class GClass[a] {
            def g(): a
        }

        instance GClass[Int] {
            def g(): Int = 6
        }

        namespace Z {
            pub class GClass[a] {
                def g(): a
            }

            instance GClass[Int] {
                def g(): Int = 7
            }
        }
    }
}

@test
def testNamespace12(): Bool =
    use X.GClass.g;
    g() == 5

@test
def testNamespace13(): Bool =
    use X/Y.GClass.g;
    g() == 6

@test
def testNamespace14(): Bool =
    use X/Y/Z.GClass.g;
    g() == 7

@test
def testNamespace15(): Bool =
    use X.GClass.{g => gx};
    use X/Y.GClass.{g => gxy};
    use X/Y/Z.GClass.{g => gxyz};
    (gx() + gxy() + gxyz()) == 18

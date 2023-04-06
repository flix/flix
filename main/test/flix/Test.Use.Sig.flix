mod A {
    pub class FClass[a] {
        pub def f(): a
    }

    instance FClass[Int32] {
        pub def f(): Int32 = 1
    }

    @test
    def testMod01(): Bool =
        use A.FClass.f;
        f() == 1

    @test
    def testMod02(): Bool =
        use B.FClass.f;
        f() == 2

    @test
    def testMod03(): Bool =
        use C.FClass.f;
        f() == 3
}

mod B {
    pub class FClass[a] {
        pub def f(): a
    }

    instance FClass[Int32] {
        pub def f(): Int32 = 2
    }

    @test
    def testMod04(): Bool =
        use A.FClass.{f => fa};
        fa() == 1

    @test
    def testMod05(): Bool =
        use B.FClass.{f => fb};
        fb() == 2

    @test
    def testMod06(): Bool =
        use C.FClass.{f => fc};
        fc() == 3

}

mod C {
    pub class FClass[a] {
        pub def f(): a
    }

    instance FClass[Int32] {
        pub def f(): Int32 = 3
    }

    @test
    def testMod07(): Bool =
        use A.FClass.{f => fa};
        use B.FClass.{f => fb};
        use C.FClass.{f => fc};
        (fa() + fb() + fc()) == 6
}

mod D {
    use A.FClass.f
    use B.FClass.{f => fb}
    use X.Y.GClass.g
    use X.Y.Z.GClass.{g => gz}

    @test
    def testMod08(): Bool = f() == 1

    @test
    def testMod09(): Bool = fb() == 2

    @test
    def testMod10(): Bool = g() == 6

    @test
    def testMod11(): Bool = gz() == 7
}

pub class GClass[a] {
    pub def g(): a
}

instance GClass[Int32] {
    pub def g(): Int32 = 4
}

mod X {
    pub class GClass[a] {
        pub def g(): a
    }

    instance GClass[Int32] {
        pub def g(): Int32 = 5
    }

    mod Y {
        pub class GClass[a] {
            pub def g(): a
        }

        instance GClass[Int32] {
            pub def g(): Int32 = 6
        }

        mod Z {
            pub class GClass[a] {
                pub def g(): a
            }

            instance GClass[Int32] {
                pub def g(): Int32 = 7
            }
        }
    }
}

mod Test.Use.Sig {

    @test
    def testMod12(): Bool =
        use X.GClass.g;
        g() == 5

    @test
    def testMod13(): Bool =
        use X.Y.GClass.g;
        g() == 6

    @test
    def testMod14(): Bool =
        use X.Y.Z.GClass.g;
        g() == 7

    @test
    def testMod15(): Bool =
        use X.GClass.{g => gx};
        use X.Y.GClass.{g => gxy};
        use X.Y.Z.GClass.{g => gxyz};
        (gx() + gxy() + gxyz()) == 18

}

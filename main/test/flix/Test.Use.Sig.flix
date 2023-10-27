mod A {
    pub trait FTrait[a] {
        pub def f(): a
    }

    instance FTrait[Int32] {
        pub def f(): Int32 = 1
    }

    @test
    def testMod01(): Bool =
        use A.FTrait.f;
        f() == 1

    @test
    def testMod02(): Bool =
        use B.FTrait.f;
        f() == 2

    @test
    def testMod03(): Bool =
        use C.FTrait.f;
        f() == 3
}

mod B {
    pub trait FTrait[a] {
        pub def f(): a
    }

    instance FTrait[Int32] {
        pub def f(): Int32 = 2
    }

    @test
    def testMod04(): Bool =
        use A.FTrait.{f => fa};
        fa() == 1

    @test
    def testMod05(): Bool =
        use B.FTrait.{f => fb};
        fb() == 2

    @test
    def testMod06(): Bool =
        use C.FTrait.{f => fc};
        fc() == 3

}

mod C {
    pub trait FTrait[a] {
        pub def f(): a
    }

    instance FTrait[Int32] {
        pub def f(): Int32 = 3
    }

    @test
    def testMod07(): Bool =
        use A.FTrait.{f => fa};
        use B.FTrait.{f => fb};
        use C.FTrait.{f => fc};
        (fa() + fb() + fc()) == 6
}

mod D {
    use A.FTrait.f
    use B.FTrait.{f => fb}
    use X.Y.GTrait.g
    use X.Y.Z.GTrait.{g => gz}

    @test
    def testMod08(): Bool = f() == 1

    @test
    def testMod09(): Bool = fb() == 2

    @test
    def testMod10(): Bool = g() == 6

    @test
    def testMod11(): Bool = gz() == 7
}

pub trait GTrait[a] {
    pub def g(): a
}

instance GTrait[Int32] {
    pub def g(): Int32 = 4
}

mod X {
    pub trait GTrait[a] {
        pub def g(): a
    }

    instance GTrait[Int32] {
        pub def g(): Int32 = 5
    }

    mod Y {
        pub trait GTrait[a] {
            pub def g(): a
        }

        instance GTrait[Int32] {
            pub def g(): Int32 = 6
        }

        mod Z {
            pub trait GTrait[a] {
                pub def g(): a
            }

            instance GTrait[Int32] {
                pub def g(): Int32 = 7
            }
        }
    }
}

mod Test.Use.Sig {

    @test
    def testMod12(): Bool =
        use X.GTrait.g;
        g() == 5

    @test
    def testMod13(): Bool =
        use X.Y.GTrait.g;
        g() == 6

    @test
    def testMod14(): Bool =
        use X.Y.Z.GTrait.g;
        g() == 7

    @test
    def testMod15(): Bool =
        use X.GTrait.{g => gx};
        use X.Y.GTrait.{g => gxy};
        use X.Y.Z.GTrait.{g => gxyz};
        (gx() + gxy() + gxyz()) == 18

}

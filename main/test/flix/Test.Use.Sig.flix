mod A {
    pub trait FTrait[a] {
        pub def f(): a
    }

    instance FTrait[Int32] {
        pub def f(): Int32 = 1
    }

    @CompileTest
    def testMod01(): Bool =
        use A.FTrait.f;
        f() == 1

    @CompileTest
    def testMod02(): Bool =
        use B.FTrait.f;
        f() == 2

    @CompileTest
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

    @CompileTest
    def testMod04(): Bool =
        use A.FTrait.{f => fa};
        fa() == 1

    @CompileTest
    def testMod05(): Bool =
        use B.FTrait.{f => fb};
        fb() == 2

    @CompileTest
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

    @CompileTest
    def testMod07(): Bool =
        use A.FTrait.{f => fa};
        use B.FTrait.{f => fb};
        use C.FTrait.{f => fc};
        (fa() + fb() + fc()) == 6
}

mod D {
    use A.FTrait.f
    use B.FTrait.{f => fb}

    @CompileTest
    def testMod08(): Bool = f() == 1

    @CompileTest
    def testMod09(): Bool = fb() == 2
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
}

mod Test.Use.Sig {

    @CompileTest
    def testMod12(): Bool =
        use X.GTrait.g;
        g() == 5

}

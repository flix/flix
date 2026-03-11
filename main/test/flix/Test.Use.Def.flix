mod A {
    pub def f(): Int32 = 1

    @CompileTest
    def testMod01(): Bool =
        use A.f;
        f() == 1

    @CompileTest
    def testMod02(): Bool =
        use B.f;
        f() == 2

    @CompileTest
    def testMod03(): Bool =
        use C.f;
        f() == 3
}

mod B {
    pub def f(): Int32 = 2

    @CompileTest
    def testMod04(): Bool =
        use A.{f => fa};
        fa() == 1

    @CompileTest
    def testMod05(): Bool =
        use B.{f => fb};
        fb() == 2

    @CompileTest
    def testMod06(): Bool =
        use C.{f => fc};
        fc() == 3

}

mod C {
    pub def f(): Int32 = 3

    @CompileTest
    def testMod07(): Bool =
        use A.{f => fa};
        // Comment
        use B.{f => fb};
        /* More comment */
        use C.{f => fc};
        (fa() + fb() + fc()) == 6
}

mod D {
    use A.f
    use B.{f => fb}
    use X.Y.g
    use X.Y.Z.{g => gz}

    @CompileTest
    def testMod08(): Bool = f() == 1

    @CompileTest
    def testMod09(): Bool = fb() == 2

    @CompileTest
    def testMod10(): Bool = g() == 6

    @CompileTest
    def testMod11(): Bool = gz() == 7
}

pub def g(): Int32 = 4
mod X {
    pub def g(): Int32 = 5
    mod Y {
        pub def g(): Int32 = 6
        mod Z {
            pub def g(): Int32 = 7
        }
    }
}

@CompileTest
def testMod12(): Bool =
    use X.g;
    g() == 5

@CompileTest
def testMod13(): Bool =
    use X.Y.g;
    g() == 6

@CompileTest
def testMod14(): Bool =
    use X.Y.Z.g;
    g() == 7

@CompileTest
def testMod15(): Bool =
    use X.{g => gx};
    use X.Y.{g => gxy};
    use X.Y.Z.{g => gxyz};
    (gx() + gxy() + gxyz()) == 18

mod Op {
    pub def <><(left: String, right: String): String  = "${left} fish ${right}"
    pub def prefish(left: String, right: String): String = "${left} fish ${right}"
}

mod Test.Use.Def {

    @CompileTest
    def testMod16(): Bool =
        use Op.{<><};
        "one" <>< "two" == "one fish two"

    @CompileTest
    def testMod17(): Bool =
        use Op.{<>< => ><>};
        "one" ><> "two" == "one fish two"

    @CompileTest
    def testMod18(): Bool =
        use Op.{<>< => fish};
        fish("one", "two") == "one fish two"

    @CompileTest
    def testMod19(): Bool =
        use Op.{prefish => ><>};
        "one" ><> "two" == "one fish two"

}

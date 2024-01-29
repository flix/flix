mod Test.Exp.TryChainedHandlers {

    eff A {
        pub def a(): String
    }

    eff B {
        pub def b(): String
    }

    eff C {
        pub def c(): Int32
    }

    eff D {
        pub def d1(): Int32
        pub def d2(): Int32
    }

    eff E {
        pub def e(): Int32
    }

    @Test
    def testTryChained01(): Bool =
        let result = try {
            String.concat(do A.a(), do B.b())
        } with A {
            def a(k) = k("a")
        } with B {
            def b(k) = k("b")
        };
        Assert.eq("ab", result)

    @Test
    def testTryChained02(): Bool =
        let result = try {
            String.concat(do A.a(), do B.b())
        } with B {
            def b(k) = k("b")
        } with A {
            def a(k) = k("a")
        };
        Assert.eq("ab", result)

    @Test
    def testTryChained03(): Bool =
        let result = try {
            do C.c() + do D.d1() + do D.d2() + do E.e()
        } with C {
            def c(k) = k(42)
        } with D {
            def d1(k) = k(42)
            def d2(k) = k(42)
        } with E {
            def e(k) = k(42)
        };
        Assert.eq(4 * 42, result)

    @Test
    def testTryChained04(): Bool =
        let result = try {
            do C.c() + do D.d1() + do D.d2() + do E.e()
        } with C {
            def c(k) = k(42)
        } with D {
            def d1(k) = k(42)
            def d2(k) = k(42)
        } with E {
            def e(k) = 42
        };
        Assert.eq(42, result)

    @Test
    def testTryChained05(): Bool =
        let result = try {
            do C.c()
        } with C {
            def c(k) = k(do D.d1())
        } with D {
            def d1(k) = k(do D.d2())
            def d2(k) = k(do E.e())
        } with E {
            def e(k) = 42
        };
        Assert.eq(42, result)

}

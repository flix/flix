mod Test.Exp.TryChainedHandlers {

    eff A {
        pub def a(): String
    }

    eff B {
        pub def b(): String
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

}

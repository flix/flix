mod Test.Exp.TryChainedHandlers.flix {

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
            def a() = k("a")
        } with B {
            def b() = k("b")
        };
        Assert.eq("ab", result)

    @Test
    def testTryChained02(): Bool =
        let result = try {
            String.concat(do A.a(), do B.b())
        } with B {
            def b() = k("b")
        } with A {
            def a() = k("a")
        };
        Assert.eq("ba", result)

}

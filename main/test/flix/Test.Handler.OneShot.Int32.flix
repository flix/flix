mod Test.Handler.OneShot.Int32 {

    eff Ask {
        pub def ask(x: Int32): Int32
    }

    @Test
    def testLinear01(): Bool =
        def f1() = do Ask.ask(42);
        let result = try {
            f1()
        } with Ask {
            def ask(x, k) = k(x + x)
        };
        Assert.eq(84, result)

}

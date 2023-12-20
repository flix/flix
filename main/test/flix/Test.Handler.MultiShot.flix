mod Test.Handler.MultiShot {

    eff E {
        pub def e(): String
    }

    @Test
    def testLinear01(): Bool =
        def f1() = { "x" + do E.e() + "z" };
        let result = try {
            f1()
        } with E {
            def e(k) =  { k("y") + k("y") }
        };
        Assert.eq("xyzxyz", result)

}

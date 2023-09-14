mod Test.Equality.Float64 {

    @Test
    def testEqFloat64_01(): Bool = +0.0f64 == +0.0f64

    @Test
    def testEqFloat64_02(): Bool = -0.0f64 == -0.0f64

    @Test
    def testEqFloat64_03(): Bool = +1.0f64 == +1.0f64

    @Test
    def testEqFloat64_04(): Bool = -1.0f64 == -1.0f64

    @Test
    def testNeqFloat64_01(): Bool = -1.0f64 != +1.0f64

    @Test
    def testNeqFloat64_02(): Bool = +1.0f64 != -1.0f64

}

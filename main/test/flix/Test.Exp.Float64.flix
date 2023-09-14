mod Test.Exp.Float64 {

    @Test
    def testFloat64_01(): Float64 = 0.0f64

    @Test
    def testFloat64_02(): Float64 = -0.0f64

    @Test
    def testFloat64_03(): Float64 = 1.0f64

    @Test
    def testFloat64_04(): Float64 = 2.0f64

    @Test
    def testFloat64_05(): Float64 = 4.2f64

    @Test
    def testFloat64_06(): Float64 = 99999999999999999999999999999999999999999999999999999999999999999999999999999999.0f64

    @Test
    def testFloat64_07(): Float64 = 0.000000000000000000000000000000000000000000000000000000000000000000000000000000001f64

    @Test
    def testFloat64_08(): Float64 = -99999999999999999999999999999999999999999999999999999999999999999999999999999999.0f64

    @Test
    def testFloat64_09(): Float64 = -0.000000000000000000000000000000000000000000000000000000000000000000000000000000001f64

    @Test
    def testFloat64_10(): Float64 = 1_0.0_0f64

    @Test
    def testFloat64_11(): Float64 = 1_000.123_456_789f64

    @Test
    def testFloat64_12(): Float64 = 42_000.0f64

    @Test
    def testFloat64_13(): Float64 = 1_000_000_000_000_000_000_000_000_000.0f64

    @Test
    def testFloat64_14(): Float64 = 0.000_000_000_000_000_000_000_000_000_1f64

}

mod Test.Lit.Float64 {

    @CompileTest
    def float64Lit01(): Float64 = 0.0f64

    @CompileTest
    def float64Lit02(): Float64 = -0.0f64

    @CompileTest
    def float64Lit03(): Float64 = 1.0f64

    @CompileTest
    def float64Lit04(): Float64 = 2.0f64

    @CompileTest
    def float64Lit05(): Float64 = 4.2f64

    @CompileTest
    def float64Lit06(): Float64 = 99999999999999999999999999999999999999999999999999999999999999999999999999999999.0f64

    @CompileTest
    def float64Lit07(): Float64 = 0.000000000000000000000000000000000000000000000000000000000000000000000000000000001f64

    @CompileTest
    def float64Lit08(): Float64 = -99999999999999999999999999999999999999999999999999999999999999999999999999999999.0f64

    @CompileTest
    def float64Lit09(): Float64 = -0.000000000000000000000000000000000000000000000000000000000000000000000000000000001f64

    @CompileTest
    def float64Lit10(): Float64 = 1_0.0_0f64

    @CompileTest
    def float64Lit11(): Float64 = 1_000.123_456_789f64

    @CompileTest
    def float64Lit12(): Float64 = 42_000.0f64

    @CompileTest
    def float64Lit13(): Float64 = 1_000_000_000_000_000_000_000_000_000.0f64

    @CompileTest
    def float64Lit14(): Float64 = 0.000_000_000_000_000_000_000_000_000_1f64

}

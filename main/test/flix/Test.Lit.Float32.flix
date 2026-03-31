mod Test.Lit.Float32 {

    @CompileTest
    def float32Lit01(): Float32 = 0.0f32

    @CompileTest
    def float32Lit02(): Float32 = -0.0f32

    @CompileTest
    def float32Lit03(): Float32 = 1.0f32

    @CompileTest
    def float32Lit04(): Float32 = 2.0f32

    @CompileTest
    def float32Lit05(): Float32 = 4.2f32

    @CompileTest
    def float32Lit06(): Float32 = 999999999999999999999999999999.0f32

    @CompileTest
    def float32Lit07(): Float32 = 0.0000000000000000000000000000001f32

    @CompileTest
    def float32Lit08(): Float32 = -999999999999999999999999999999.0f32

    @CompileTest
    def float32Lit09(): Float32 = -0.0000000000000000000000000000001f32

    @CompileTest
    def float32Lit10(): Float32 = 1_0.0_0f32

    @CompileTest
    def float32Lit11(): Float32 = 1_000.123_456_789f32

    @CompileTest
    def float32Lit12(): Float32 = 42_000.0f32

    @CompileTest
    def float32Lit13(): Float32 = 1_000_000_000_000_000_000_000_000_000.0f32

    @CompileTest
    def float32Lit14(): Float32 = 0.000_000_000_000_000_000_000_000_000_1f32

}

mod Test.Exp.Float32 {

    @Test
    def testFloat32_01(): Float32 = +0.0f32

    @Test
    def testFloat32_02(): Float32 = -0.0f32

    @Test
    def testFloat32_03(): Float32 = 1.0f32

    @Test
    def testFloat32_04(): Float32 = 2.0f32

    @Test
    def testFloat32_05(): Float32 = 4.2f32

    @Test
    def testFloat32_06(): Float32 = 999999999999999999999999999999.0f32

    @Test
    def testFloat32_07(): Float32 = 0.0000000000000000000000000000001f32

    @Test
    def testFloat32_08(): Float32 = -999999999999999999999999999999.0f32

    @Test
    def testFloat32_09(): Float32 = -0.0000000000000000000000000000001f32

    @Test
    def testFloat32_10(): Float32 = 1_0.0_0f32

    @Test
    def testFloat32_11(): Float32 = 1_000.123_456_789f32

    @Test
    def testFloat32_12(): Float32 = 42_000.0f32

    @Test
    def testFloat32_13(): Float32 = 1_000_000_000_000_000_000_000_000_000.0f32

    @Test
    def testFloat32_14(): Float32 = 0.000_000_000_000_000_000_000_000_000_1f32

}

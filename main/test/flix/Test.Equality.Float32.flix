mod Test.Equality.Float32 {

    @Test
    def testEqFloat32_01(): Bool = +0.0f32 == +0.0f32

    @Test
    def testEqFloat32_02(): Bool = -0.0f32 == -0.0f32

    @Test
    def testEqFloat32_03(): Bool = +1.0f32 == +1.0f32

    @Test
    def testEqFloat32_04(): Bool = -1.0f32 == -1.0f32

    @Test
    def testNeqFloat32_01(): Bool = -1.0f32 != +1.0f32

    @Test
    def testNeqFloat32_02(): Bool = +1.0f32 != -1.0f32

}

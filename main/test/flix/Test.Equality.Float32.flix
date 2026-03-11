mod Test.Equality.Float32 {

    use Assert.{assertEq, assertNeq}

    @Test
    def testEqFloat32_01(): Unit \ Assert =
        assertEq(expected = 0.0f32, 0.0f32)

    @Test
    def testEqFloat32_02(): Unit \ Assert =
        assertEq(expected = -0.0f32, -0.0f32)

    @Test
    def testEqFloat32_03(): Unit \ Assert =
        assertEq(expected = 1.0f32, 1.0f32)

    @Test
    def testEqFloat32_04(): Unit \ Assert =
        assertEq(expected = -1.0f32, -1.0f32)

    @Test
    def testEqFloat32_05(): Unit \ Assert =
        assertEq(expected = 0.0f32, -0.0f32)

    @Test
    def testNeqFloat32_01(): Unit \ Assert =
        assertNeq(unexpected = -1.0f32, 1.0f32)

    @Test
    def testNeqFloat32_02(): Unit \ Assert =
        assertNeq(unexpected = 1.0f32, -1.0f32)

    @Test
    def testNeqFloat32_03(): Unit \ Assert =
        assertNeq(unexpected = 1.0f32, 2.0f32)

}

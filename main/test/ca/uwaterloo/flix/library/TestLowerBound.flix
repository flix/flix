mod TestLowerBound {

    use Assert.{assertEq};
    use LowerBound.minValue;

    @Test
    def minValue_Unit01(): Unit \ Assert =
        assertEq(expected = (), minValue())

    @Test
    def minValue_Bool01(): Unit \ Assert =
        assertEq(expected = false, minValue())

    @Test
    def minValue_Int801(): Unit \ Assert =
        assertEq(expected = -128i8, minValue())

    @Test
    def minValue_Int1601(): Unit \ Assert =
        assertEq(expected = -32_768i16, minValue())

    @Test
    def minValue_Int3201(): Unit \ Assert =
        assertEq(expected = -2_147_483_648, minValue())

    @Test
    def minValue_Int6401(): Unit \ Assert =
        assertEq(expected = -9_223_372_036_854_775_808i64, minValue())

    @Test
    def minValue_Float3201(): Unit \ Assert =
        assertEq(expected = Float32.minValue(), minValue())

    @Test
    def minValue_Float6401(): Unit \ Assert =
        assertEq(expected = Float64.minValue(), minValue())

    @Test
    def minValue_Char01(): Unit \ Assert =
        assertEq(expected = Char.minValue(), minValue())

    @Test
    def minValue_Tuple201(): Unit \ Assert =
        assertEq(expected = ('\u0000', '\u0000'), minValue())

    @Test
    def minValue_Tuple301(): Unit \ Assert =
        assertEq(expected = ('\u0000', '\u0000', '\u0000'), minValue())

    @Test
    def minValue_Tuple401(): Unit \ Assert =
        assertEq(expected = ('\u0000', '\u0000', '\u0000', '\u0000'), minValue())

    @Test
    def minValue_Tuple501(): Unit \ Assert =
        assertEq(expected = ('\u0000', '\u0000', '\u0000', '\u0000', '\u0000'), minValue())

    @Test
    def minValue_Tuple601(): Unit \ Assert =
        assertEq(expected = ('\u0000', '\u0000', '\u0000', '\u0000', '\u0000', '\u0000'), minValue())

    @Test
    def minValue_Tuple701(): Unit \ Assert =
        assertEq(expected = ('\u0000', '\u0000', '\u0000', '\u0000', '\u0000', '\u0000', '\u0000'), minValue())

    @Test
    def minValue_Tuple801(): Unit \ Assert =
        assertEq(expected = ('\u0000', '\u0000', '\u0000', '\u0000', '\u0000', '\u0000', '\u0000', '\u0000'), minValue())

    @Test
    def minValue_Tuple901(): Unit \ Assert =
        assertEq(expected = ('\u0000', '\u0000', '\u0000', '\u0000', '\u0000', '\u0000', '\u0000', '\u0000', '\u0000'), minValue())

    @Test
    def minValue_Tuple1001(): Unit \ Assert =
        assertEq(expected = ('\u0000', '\u0000', '\u0000', '\u0000', '\u0000', '\u0000', '\u0000', '\u0000', '\u0000', '\u0000'), minValue())

}


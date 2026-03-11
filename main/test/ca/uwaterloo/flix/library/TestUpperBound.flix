mod TestUpperBound {

    use Assert.{assertEq};
    use UpperBound.maxValue;

    @Test
    def maxValue_Unit01(): Unit \ Assert =
        assertEq(expected = (), maxValue())

    @Test
    def maxValue_Bool01(): Unit \ Assert =
        assertEq(expected = true, maxValue())

    @Test
    def maxValue_Int801(): Unit \ Assert =
        assertEq(expected = 127i8, maxValue())

    @Test
    def maxValue_Int1601(): Unit \ Assert =
        assertEq(expected = 32_767i16, maxValue())

    @Test
    def maxValue_Int3201(): Unit \ Assert =
        assertEq(expected = 2_147_483_647, maxValue())

    @Test
    def maxValue_Int6401(): Unit \ Assert =
        assertEq(expected = 9_223_372_036_854_775_807i64, maxValue())

    @Test
    def maxValue_Float3201(): Unit \ Assert =
        assertEq(expected = Float32.maxValue(), maxValue())

    @Test
    def maxValue_Float6401(): Unit \ Assert =
        assertEq(expected = Float64.maxValue(), maxValue())

    @Test
    def maxValue_Char01(): Unit \ Assert =
        assertEq(expected = '\uffff', maxValue())

    @Test
    def maxValue_Tuple201(): Unit \ Assert =
        assertEq(expected = ('\uffff', '\uffff'), maxValue())

    @Test
    def maxValue_Tuple301(): Unit \ Assert =
        assertEq(expected = ('\uffff', '\uffff', '\uffff'), maxValue())

    @Test
    def maxValue_Tuple401(): Unit \ Assert =
        assertEq(expected = ('\uffff', '\uffff', '\uffff', '\uffff'), maxValue())

    @Test
    def maxValue_Tuple501(): Unit \ Assert =
        assertEq(expected = ('\uffff', '\uffff', '\uffff', '\uffff', '\uffff'), maxValue())

    @Test
    def maxValue_Tuple601(): Unit \ Assert =
        assertEq(expected = ('\uffff', '\uffff', '\uffff', '\uffff', '\uffff', '\uffff'), maxValue())

    @Test
    def maxValue_Tuple701(): Unit \ Assert =
        assertEq(expected = ('\uffff', '\uffff', '\uffff', '\uffff', '\uffff', '\uffff', '\uffff'), maxValue())

    @Test
    def maxValue_Tuple801(): Unit \ Assert =
        assertEq(expected = ('\uffff', '\uffff', '\uffff', '\uffff', '\uffff', '\uffff', '\uffff', '\uffff'), maxValue())

    @Test
    def maxValue_Tuple901(): Unit \ Assert =
        assertEq(expected = ('\uffff', '\uffff', '\uffff', '\uffff', '\uffff', '\uffff', '\uffff', '\uffff', '\uffff'), maxValue())

    @Test
    def maxValue_Tuple1001(): Unit \ Assert =
        assertEq(expected = ('\uffff', '\uffff', '\uffff', '\uffff', '\uffff', '\uffff', '\uffff', '\uffff', '\uffff', '\uffff'), maxValue())

}

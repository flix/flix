mod TestOrder {

    use Assert.{assertEq};

    /////////////////////////////////////////////////////////////////////////////
    // fromInt                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def fromInt01(): Unit \ Assert =
        assertEq(expected = Comparison.LessThan, Comparison.fromInt32(-1))

    @Test
    def fromInt02(): Unit \ Assert =
        assertEq(expected = Comparison.EqualTo, Comparison.fromInt32(0))

    @Test
    def fromInt03(): Unit \ Assert =
        assertEq(expected = Comparison.GreaterThan, Comparison.fromInt32(1))

    /////////////////////////////////////////////////////////////////////////////
    // min                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def min01(): Unit \ Assert =
        assertEq(expected = 0, Order.min(0, 1))

    @Test
    def min02(): Unit \ Assert =
        assertEq(expected = (), Order.min((), ()))

    @Test
    def min03(): Unit \ Assert =
        assertEq(expected = false, Order.min(true, false))

    @Test
    def min04(): Unit \ Assert =
        assertEq(expected = 42, Order.min(342, 42))

    @Test
    def min05(): Unit \ Assert =
        assertEq(expected = 'a', Order.min('a', 'z'))

    /////////////////////////////////////////////////////////////////////////////
    // max                                                                     //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def max01(): Unit \ Assert =
        assertEq(expected = 1, Order.max(0, 1))

    @Test
    def max02(): Unit \ Assert =
        assertEq(expected = (), Order.max((), ()))

    @Test
    def max03(): Unit \ Assert =
        assertEq(expected = true, Order.max(true, false))

    @Test
    def max04(): Unit \ Assert =
        assertEq(expected = 3, Order.max(3, -3))

    @Test
    def max05(): Unit \ Assert =
        assertEq(expected = 'z', Order.max('a', 'z'))

}

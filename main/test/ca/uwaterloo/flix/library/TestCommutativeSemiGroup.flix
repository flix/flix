mod TestCommutativeSemiGroup {

    use Assert.assertEq;

    /////////////////////////////////////////////////////////////////////////////
    // Syntax                                                                  //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def operator01(): Unit \ Assert = assertEq(expected = 2, 1 |+| 1)

    @Test
    def operator02(): Unit \ Assert = assertEq(expected = Set#{1, 2, 3}, Set#{1, 2} |+| Set#{2, 3})

}
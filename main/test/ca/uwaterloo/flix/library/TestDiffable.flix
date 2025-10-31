mod TestDiffable {

    use Assert.{assertEq, assertTrue};

    /////////////////////////////////////////////////////////////////////////////
    // Set.diff                                                                //
    /////////////////////////////////////////////////////////////////////////////

    @Test
    def diffSet01(): Unit \ Assert =
        let expected = Set#{1, 2, 3};
        let actual = Set#{1, 2, 3};
        let result = Diffable.diff(expected = expected, actual);
        assertEq(expected = RichString.toString(Diffable.formatNoDiff()), RichString.toString(result))

    @Test
    def diffSet02(): Unit \ Assert =
        let expected = Set#{1, 2, 3};
        let actual = Set#{1, 2, 3, 4};
        let result = Diffable.diff(expected = expected, actual);
        assertTrue(String.contains(substr = "+ 4", RichString.toString(result)))

    @Test
    def diffSet03(): Unit \ Assert =
        let expected = Set#{1, 2, 3};
        let actual = Set#{1, 2};
        let result = Diffable.diff(expected = expected, actual);
        assertTrue(String.contains(substr = "- 3", RichString.toString(result)))

    @Test
    def diffSet04(): Unit \ Assert =
        let expected = Set#{1, 2, 3};
        let actual = Set#{1, 2, 4};
        let result = Diffable.diff(expected = expected, actual);
        let str = RichString.toString(result);
        assertTrue(String.contains(substr = "- 3", str) and String.contains(substr = "+ 4", str))

    @Test
    def diffSet05(): Unit \ Assert =
        let expected = Set#{1, 2, 3, 5, 7};
        let actual = Set#{2, 4, 6, 7, 8};
        let result = Diffable.diff(expected = expected, actual);
        let str = RichString.toString(result);
        assertTrue(
            String.contains(substr = "- 1", str) and
            String.contains(substr = "- 3", str) and
            String.contains(substr = "- 5", str) and
            String.contains(substr = "+ 4", str) and
            String.contains(substr = "+ 6", str) and
            String.contains(substr = "+ 8", str)
        )

}

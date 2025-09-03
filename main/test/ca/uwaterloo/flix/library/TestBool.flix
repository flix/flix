mod TestBool {

    use Assert.{assertEq};

    @Test
    def runIf01(): Unit \ {IO, Assert} =
        assertEq(expected = (), runIf(_ -> println("Hello, World!")) (true))

    @Test
    def runIf02(): Unit \ {IO, Assert} =
        assertEq(expected = (), runIf(_ -> println("Hello, World!")) (false))

    @Test
    def runIf03(): Unit \ Assert =
        assertEq(expected = (), runIf(_ -> ()) (true))

    @Test
    def runIf04(): Unit \ Assert =
        assertEq(expected = (), runIf(_ -> ()) (false))

}

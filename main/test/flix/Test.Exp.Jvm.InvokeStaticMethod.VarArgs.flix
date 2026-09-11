mod Test.Exp.Jvm.InvokeStaticMethod.VarArgs {

    import java.nio.file.Path
    import java.util.Arrays
    import java.util.{List => JList}

    use Assert.assertEq;

    @Test
    def testInvokeVarArgs01(): Unit \ Assert + IO =
        // Single varargs argument
        let p = Path.of("hello", "world");
        assertEq(expected = "world", p.getFileName().toString())

    @Test
    def testInvokeVarArgs02(): Unit \ Assert + IO =
        // Multiple varargs arguments
        let p = Path.of("hello", "foo", "bar");
        assertEq(expected = "bar", p.getFileName().toString())

    @Test
    def testInvokeVarArgs03(): Unit \ Assert + IO =
        // Varargs omitted entirely
        let p = Path.of("hello");
        assertEq(expected = "hello", p.getFileName().toString())

    @Test
    def testInvokeVarArgs04(): Unit \ Assert + IO =
        // Explicit varargs vector
        let p = Path.of("hello", Vector#{"foo", "bar.txt"});
        assertEq(expected = "bar.txt", p.getFileName().toString())

    @Test
    def testInvokeVarArgs05(): Unit \ Assert + IO =
        // A primitive array is not an Object[], so it is a single element of the generic varargs `T...`
        let l = Arrays.asList(Vector#{1, 2});
        assertEq(expected = 1, l.size())

    @Test
    def testInvokeVarArgs06(): Unit \ Assert + IO =
        // The element instantiates `T`, so the list holds vectors
        let l: JList[Vector[Int32]] = Arrays.asList(Vector#{1, 2});
        assertEq(expected = 1, l.size())

    @Test
    def testInvokeVarArgs07(): Unit \ Assert + IO =
        // Several primitive arrays are wrapped into one array of arrays
        let l = Arrays.asList(Vector#{1, 2}, Vector#{3});
        assertEq(expected = 2, l.size())

}

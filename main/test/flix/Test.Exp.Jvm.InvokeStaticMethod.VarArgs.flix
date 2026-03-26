mod Test.Exp.Jvm.InvokeMethod.VarArgs {

    import java.nio.file.Path

    use Assert.assertEq;

    @Test
    def testInvokeVarArgs01(): Unit \ Assert + IO =
        let p = Path.of("hello", (...{}: Vector[String]));
        assertEq(expected = "hello", p.toString())

    @Test
    def testInvokeVarArgs02(): Unit \ Assert + IO =
        let p = Path.of("hello", ...{"foo.txt"});
        assertEq(expected = "foo.txt", p.getFileName().toString())

    @Test
    def testInvokeVarArgs03(): Unit \ Assert + IO =
        let p = Path.of("hello", ...{"foo.txt", "bar.txt"});
        assertEq(expected = "bar.txt", p.getFileName().toString())

    // Natural varargs (without ...{} syntax)

    @Test
    def testInvokeVarArgsNatural01(): Unit \ Assert + IO =
        // Single varargs argument
        let p = Path.of("hello", "world");
        assertEq(expected = "world", p.getFileName().toString())

    @Test
    def testInvokeVarArgsNatural02(): Unit \ Assert + IO =
        // Multiple varargs arguments
        let p = Path.of("hello", "foo", "bar");
        assertEq(expected = "bar", p.getFileName().toString())

    @Test
    def testInvokeVarArgsNatural03(): Unit \ Assert + IO =
        // Varargs omitted entirely (no ...{}, no explicit args)
        let p = Path.of("hello");
        assertEq(expected = "hello", p.getFileName().toString())

}

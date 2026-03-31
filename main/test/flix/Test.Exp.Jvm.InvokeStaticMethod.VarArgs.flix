mod Test.Exp.Jvm.InvokeMethod.VarArgs {

    import java.nio.file.Path

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

}

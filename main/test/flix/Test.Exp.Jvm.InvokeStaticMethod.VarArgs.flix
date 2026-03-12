mod Test.Exp.Jvm.InvokeStaticMethod.VarArgs {

    import java.lang.{String => JString}
    import java.nio.file.Path

    use Assert.assertEq

    @Test
    def testStaticVarArgs01(): Unit \ Assert + IO =
        // Single varargs argument with Path.of (static method)
        let p = Path.of("hello", "world");
        assertEq(expected = "world", p.getFileName().toString())

    @Test
    def testStaticVarArgs02(): Unit \ Assert + IO =
        // Multiple varargs arguments
        let p = Path.of("hello", "foo", "bar");
        assertEq(expected = "bar", p.getFileName().toString())

    @Test
    def testStaticVarArgs03(): Unit \ Assert + IO =
        // Varargs with ...{} syntax (existing behavior)
        let p = Path.of("hello", ...{"world"});
        assertEq(expected = "world", p.getFileName().toString())

    @Test
    def testStaticVarArgs04(): Unit \ Assert + IO =
        // Varargs omitted
        let p = Path.of("hello");
        assertEq(expected = "hello", p.getFileName().toString())

}

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

}

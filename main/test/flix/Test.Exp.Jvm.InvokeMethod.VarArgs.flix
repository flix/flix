mod Test.Exp.Jvm.InvokeMethod.VarArgs {

    import java.nio.file.Path

    @Test
    def testInvokeVarArgs01(): Path \ IO =
        Path.of("hello", (...{}: Vector[String]))

    @Test
    def testInvokeVarArgs02(): Path \ IO =
        Path.of("hello", ...{"foo.txt"})

    @Test
    def testInvokeVarArgs03(): Path \ IO =
        Path.of("hello", ...{"foo.txt", "bar.txt"})

}

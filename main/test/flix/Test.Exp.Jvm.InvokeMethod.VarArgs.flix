mod Test.Exp.Jvm.InvokeMethod.VarArgs {

    import java.lang.ProcessBuilder

    use Assert.assertEq;

    @Test
    def testInvokeVarArgs01(): Unit \ Assert + IO =
        // Individual varargs arguments
        let pb = new ProcessBuilder("placeholder").command("a", "b", "c");
        assertEq(expected = "[a, b, c]", pb.command().toString())

    @Test
    def testInvokeVarArgs02(): Unit \ Assert + IO =
        // Explicit varargs vector
        let pb = new ProcessBuilder("placeholder").command(Vector#{"x", "y"});
        assertEq(expected = "[x, y]", pb.command().toString())

    @Test
    def testInvokeVarArgs03(): Unit \ Assert + IO =
        // Explicit empty varargs vector
        let pb = new ProcessBuilder("placeholder").command((Vector.empty(): Vector[String]));
        assertEq(expected = "[]", pb.command().toString())

}

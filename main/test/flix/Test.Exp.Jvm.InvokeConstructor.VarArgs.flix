mod Test.Exp.Jvm.InvokeConstructor.VarArgs {

    import java.lang.ProcessBuilder

    use Assert.assertEq;

    @Test
    def testNewVarArgs01(): Unit \ Assert + IO =
        // Individual varargs arguments
        let pb = new ProcessBuilder("echo", "hello", "world");
        assertEq(expected = "[echo, hello, world]", pb.command().toString())

    @Test
    def testNewVarArgs02(): Unit \ Assert + IO =
        // Explicit varargs vector
        let pb = new ProcessBuilder(Vector#{"echo", "hello"});
        assertEq(expected = "[echo, hello]", pb.command().toString())

    @Test
    def testNewVarArgs03(): Unit \ Assert + IO =
        // Varargs omitted entirely (auto empty array)
        let pb = new ProcessBuilder();
        assertEq(expected = "[]", pb.command().toString())

}

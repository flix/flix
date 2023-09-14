mod Test.Exp.Jvm.InvokeStaticMethod {

    @Test
    def testInvokeStaticMethod01(): Bool \ IO =
        import static java.lang.String.valueOf(Bool): String \ IO;
        valueOf(true) == "true"

    @Test
    def testInvokeStaticMethod02(): Bool \ IO =
        import static java.lang.String.valueOf(Char): String \ IO;
        valueOf('A') == "A"

    @Test
    def testInvokeStaticMethod03(): Bool \ IO =
        import static java.lang.String.valueOf(Float32): String \ IO;
        valueOf(21.42f32) == "21.42"

    @Test
    def testInvokeStaticMethod04(): Bool \ IO =
        import static java.lang.String.valueOf(Float64): String \ IO;
        valueOf(21.42f64) == "21.42"

    @Test
    def testInvokeStaticMethod05(): Bool \ IO =
        import static java.lang.String.valueOf(Int32): String \ IO;
        valueOf(42i32) == "42"

    @Test
    def testInvokeStaticMethod06(): Bool \ IO =
        import static java.lang.String.valueOf(Int64): String \ IO;
        valueOf(42i64) == "42"

    @Test
    def testInvokePureStaticMethod01(): Bool =
        import static java.lang.String.valueOf(Bool): String \ {};
        valueOf(true) == "true"

    @Test
    def testInvokeObjectStaticMethod01(): ##java.lang.String \ IO =
        import static java.lang.String.valueOf(Bool): ##java.lang.String \ IO;
        valueOf(true)

    @Test
    def testInvokeInterfaceStaticMethod02(): ##java.nio.file.Path \ IO = region rc {
        import static java.nio.file.Path.of(String, Array[String, rc]): ##java.nio.file.Path \ IO as of1;
        try {
            of1("end", Array#{"p1", "p2", "p3"} @ rc)
        } catch {
            case _: ##java.nio.file.InvalidPathException => checked_cast(null)
        }
    }

    @Test
    def testInvokeInheritedStaticMethod01(): Bool =
        import static dev.flix.test.TestClassWithInheritedMethod.staticMethod(Int32): Int32 \ {};
        staticMethod(1) == 2

    @Test
    def testInvokeStaticMethodWithArrayParam(): Bool \ IO = region rc {
        import static java.util.Arrays.fill(Array[##java.lang.Object, rc], ##java.lang.Object): Unit \ IO;
        let a = Array#{"this", "that"} @ rc;
        fill(unchecked_cast(a as Array[##java.lang.Object, rc]), checked_cast("foo"));
        true
    }
}

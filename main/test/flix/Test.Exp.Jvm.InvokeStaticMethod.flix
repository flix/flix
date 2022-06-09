namespace Test/Exp/Jvm/InvokeStaticMethod {

    @test
    def testInvokeStaticMethod01(): Bool & Impure =
        import static java.lang.String.valueOf(Bool): String & Impure;
        valueOf(true) == "true"

    @test
    def testInvokeStaticMethod02(): Bool & Impure =
        import static java.lang.String.valueOf(Char): String & Impure;
        valueOf('A') == "A"

    @test
    def testInvokeStaticMethod03(): Bool & Impure =
        import static java.lang.String.valueOf(Float32): String & Impure;
        valueOf(21.42f32) == "21.42"

    @test
    def testInvokeStaticMethod04(): Bool & Impure =
        import static java.lang.String.valueOf(Float64): String & Impure;
        valueOf(21.42f64) == "21.42"

    @test
    def testInvokeStaticMethod05(): Bool & Impure =
        import static java.lang.String.valueOf(Int32): String & Impure;
        valueOf(42i32) == "42"

    @test
    def testInvokeStaticMethod06(): Bool & Impure =
        import static java.lang.String.valueOf(Int64): String & Impure;
        valueOf(42i64) == "42"

    @test
    def testInvokePureStaticMethod01(): Bool =
        import static java.lang.String.valueOf(Bool): _ & Pure;
        valueOf(true) == "true"

    @test
    def testInvokeObjectStaticMethod01(): ##java.lang.Object & Impure =
        import static java.lang.String.valueOf(Bool): ##java.lang.Object & Impure;
        valueOf(true)

    @test
    def testInvokeInterfaceStaticMethod02(): ##java.nio.file.Path & Impure =
        import static java.nio.file.Path.of(String, Array[String, Static]): ##java.nio.file.Path & Impure as of1;
        try {
            of1("end", ["p1", "p2", "p3"])
        } catch {
            case _: ##java.nio.file.InvalidPathException => null as ##java.nio.file.Path
        }
}

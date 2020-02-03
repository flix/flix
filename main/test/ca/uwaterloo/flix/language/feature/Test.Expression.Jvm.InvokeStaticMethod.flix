namespace Test/Expression/Jvm/InvokeStaticMethod {

    @test
    def testInvokeStaticMethod01(): Bool =
        import java.lang.String:valueOf(Bool);
        valueOf(true) == "true"

    @test
    def testInvokeStaticMethod02(): Bool =
        import java.lang.String:valueOf(Char);
        valueOf('A') == "A"

    @test
    def testInvokeStaticMethod03(): Bool =
        import java.lang.String:valueOf(Float32);
        valueOf(21.42f32) == "21.42"

    @test
    def testInvokeStaticMethod04(): Bool =
        import java.lang.String:valueOf(Float64);
        valueOf(21.42f64) == "21.42"

    @test
    def testInvokeStaticMethod05(): Bool =
        import java.lang.String:valueOf(Int32);
        valueOf(42i32) == "42"

    @test
    def testInvokeStaticMethod06(): Bool =
        import java.lang.String:valueOf(Int64);
        valueOf(42i64) == "42"

}

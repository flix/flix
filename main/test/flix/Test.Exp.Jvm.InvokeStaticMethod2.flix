mod Test.Exp.Jvm.InvokeStaticMethod2 {

    import java.lang.{String => JString}
    // import java.nio.file.Path
    import java.util.Arrays
    //import dev.flix.test.TestClassWithInheritedMethod

    @test
    def testInvokeStaticMethod2_01(): Bool \ IO =
        JString.valueOf(true) == "true"

    @test
    def testInvokeStaticMethod2_02(): Bool \ IO =
        JString.valueOf('A') == "A"

    @test
    def testInvokeStaticMethod2_03(): Bool \ IO =
        JString.valueOf(21.42f32) == "21.42"

    @test
    def testInvokeStaticMethod2_04(): Bool \ IO =
        JString.valueOf(21.42f64) == "21.42"

    @test
    def testInvokeStaticMethod2_05(): Bool \ IO =
        JString.valueOf(42i32) == "42"

    @test
    def testInvokeStaticMethod2_06(): Bool \ IO =
        JString.valueOf(42i64) == "42"

    @test
    def testInvokeObjectStaticMethod01(): JString \ IO =
        JString.valueOf(true)

    //@test
    //def testInvokeInterfaceStaticMethod02(): Path \ IO = region rc {
    //    try {
    //        Path.of("end", Array#{"p1", "p2", "p3"} @ rc)
    //    } catch {
    //        case _: ##java.nio.file.InvalidPathException => checked_cast(null)
    //    }
    //}

    //@test
    //def testInvokeInheritedStaticMethod01(): Bool =
    //    TestClassWithInheritedMethod.staticMethod(1) == 2

    //@test
    //def testInvokeStaticMethod2_WithArrayParam(): Bool \ IO = region rc {
    //    let a = Array#{"this", "that"} @ rc;
    //    fill(unchecked_cast(a as Array[##java.lang.Object, rc]), checked_cast("foo"));
    //    true
    //}
}

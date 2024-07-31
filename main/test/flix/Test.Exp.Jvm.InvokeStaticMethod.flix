mod Test.Exp.Jvm.InvokeStaticMethod {

    import java.lang.{String => JString}
    import java.nio.file.InvalidPathException
    import java.lang.Object
    import java.util.Arrays
    import dev.flix.test.TestClassWithInheritedMethod
    import java.nio.file.Path

    @test
    def testInvokeStaticMethod01(): Bool \ IO =
        JString.valueOf(true) == "true"

    @test
    def testInvokeStaticMethod02(): Bool \ IO =
        JString.valueOf('A') == "A"

    @test
    def testInvokeStaticMethod03(): Bool \ IO =
        JString.valueOf(21.42f32) == "21.42"

    @test
    def testInvokeStaticMethod04(): Bool \ IO =
        JString.valueOf(21.42f64) == "21.42"

    @test
    def testInvokeStaticMethod05(): Bool \ IO =
        JString.valueOf(42i32) == "42"

    @test
    def testInvokeStaticMethod06(): Bool \ IO =
        JString.valueOf(42i64) == "42"

    @test
    def testInvokePureStaticMethod01(): Bool =
        unsafe JString.valueOf(true) == "true"

    @test
    def testInvokeObjectStaticMethod01(): JString \ IO =
        JString.valueOf(true)

    @test
    def testInvokeInterfaceStaticMethod02(): Path \ IO = region rc {
        try {
            let arr : Array[String, rc] = Array#{"p1", "p2", "p3"} @ rc;
            Path.of("end", arr)
        } catch {
            case _: InvalidPathException => (checked_cast(null) : Path)
        }
    }

    @test
    def testInvokeInheritedStaticMethod01(): Bool =
        unsafe TestClassWithInheritedMethod.staticMethod(1) == 2

    @test
    def testInvokeStaticMethodWithArrayParam(): Bool \ IO = region rc {
        let a : Array[Object, rc] = Array#{(checked_cast("this") : Object), (checked_cast("that") : Object)} @ rc;
        Arrays.fill(a, (checked_cast("foo") : Object));
        true
    }
}

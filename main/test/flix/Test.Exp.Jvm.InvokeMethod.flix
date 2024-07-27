mod Test.Exp.Jvm.InvokeMethod {

    import java.lang.{String => JString}

    @test
    def testInvokeMethod01(): Bool \ IO =
        import java.lang.String.toLowerCase(): String \ IO;
        toLowerCase("HELLO WORLD") == "hello world"

    @test
    def testInvokeMethod02(): Bool \ IO =
        import java.lang.String.toUpperCase(): String \ IO;
        toUpperCase("hello world") == "HELLO WORLD"

    @test
    def testInvokeMethod03(): Bool \ IO =
        import java.lang.String.length(): Int32 \ IO;
        length("hello world") == 11

    @test
    def testInvokeMethod04(): Bool \ IO =
        import java.lang.String.indexOf(String): Int32 \ IO;
        indexOf("hello world", "world") == 6

    @test
    def testInvokeMethod05(): Bool \ IO =
        import java.lang.String.replaceAll(String, String): String \ IO;
        replaceAll("hello world", "hello", "goodbye") == "goodbye world"

    type alias MyAliasedString = JString

    @test
    def testInvokeMethod06(): Bool \ IO =
        import java.lang.String.toUpperCase(): MyAliasedString \ IO;
        toUpperCase("hello world") == "HELLO WORLD"

    @test
    def testInvokePureMethod01(): Bool =
        import java.lang.String.toLowerCase(): String \ {};
        toLowerCase("HELLO WORLD") == "hello world"

    @test
    def testInvokeObjectMethod01(): JString \ IO =
        import java.lang.String.toLowerCase(): JString \ IO;
        toLowerCase("HELLO WORLD")

    @test
    def testInvokeInheritedMethod01(): Bool \ IO =
        import java_new dev.flix.test.TestClassWithInheritedMethod(): ##dev.flix.test.TestClassWithInheritedMethod as newObj;
        import dev.flix.test.TestClassWithInheritedMethod.concreteMethod(String): String \ {};
        let obj = newObj();
        concreteMethod(obj, "bar") == "foobar"
}

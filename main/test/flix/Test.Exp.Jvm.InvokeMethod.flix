mod Test.Exp.Jvm.InvokeMethod {

    @Test
    def testInvokeMethod01(): Bool \ IO =
        import java.lang.String.toLowerCase(): String \ IO;
        toLowerCase("HELLO WORLD") == "hello world"

    @Test
    def testInvokeMethod02(): Bool \ IO =
        import java.lang.String.toUpperCase(): String \ IO;
        toUpperCase("hello world") == "HELLO WORLD"

    @Test
    def testInvokeMethod03(): Bool \ IO =
        import java.lang.String.length(): Int32 \ IO;
        length("hello world") == 11

    @Test
    def testInvokeMethod04(): Bool \ IO =
        import java.lang.String.indexOf(String): Int32 \ IO;
        indexOf("hello world", "world") == 6

    @Test
    def testInvokeMethod05(): Bool \ IO =
        import java.lang.String.replaceAll(String, String): String \ IO;
        replaceAll("hello world", "hello", "goodbye") == "goodbye world"

    type alias MyAliasedString = ##java.lang.String

    @Test
    def testInvokeMethod06(): Bool \ IO =
        import java.lang.String.toUpperCase(): MyAliasedString \ IO;
        toUpperCase("hello world") == "HELLO WORLD"

    @Test
    def testInvokePureMethod01(): Bool =
        import java.lang.String.toLowerCase(): String \ {};
        toLowerCase("HELLO WORLD") == "hello world"

    @Test
    def testInvokeObjectMethod01(): ##java.lang.String \ IO =
        import java.lang.String.toLowerCase(): ##java.lang.String \ IO;
        toLowerCase("HELLO WORLD")

    @Test
    def testInvokeInheritedMethod01(): Bool \ IO =
        import new dev.flix.test.TestClassWithInheritedMethod(): ##dev.flix.test.TestClassWithInheritedMethod as newObj;
        import dev.flix.test.TestClassWithInheritedMethod.concreteMethod(String): String \ {};
        let obj = newObj();
        concreteMethod(obj, "bar") == "foobar"
}

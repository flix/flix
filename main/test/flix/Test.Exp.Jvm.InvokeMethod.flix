namespace Test/Exp/Jvm/InvokeMethod {

    @test
    def testInvokeMethod01(): Bool & Impure =
        import java.lang.String.toLowerCase(): String & Impure;
        toLowerCase("HELLO WORLD") == "hello world"

    @test
    def testInvokeMethod02(): Bool & Impure =
        import java.lang.String.toUpperCase();
        toUpperCase("hello world") == "HELLO WORLD"

    @test
    def testInvokeMethod03(): Bool & Impure =
        import java.lang.String.length();
        length("hello world") == 11

    @test
    def testInvokeMethod04(): Bool & Impure =
        import java.lang.String.indexOf(String);
        indexOf("hello world", "world") == 6

    @test
    def testInvokeMethod05(): Bool & Impure =
        import java.lang.String.replaceAll(String, String);
        replaceAll("hello world", "hello", "goodbye") == "goodbye world"

    @test
    def testInvokePureMethod01(): Bool =
        import java.lang.String.toLowerCase(): _ & Pure;
        toLowerCase("HELLO WORLD") == "hello world"

    @test
    def testInvokeObjectMethod01(): ##java.lang.Object & Impure =
        import java.lang.String.toLowerCase(): ##java.lang.Object;
        toLowerCase("HELLO WORLD")
}

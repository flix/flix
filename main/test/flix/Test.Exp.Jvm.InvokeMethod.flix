mod Test.Exp.Jvm.InvokeMethod {

    import dev.flix.test.TestClassWithInheritedMethod
    import java.lang.{String => JString}

    @test
    def testInvokeMethod01(): Bool =
        "HELLO WORLD".toLowerCase() == "hello world"

    @test
    def testInvokeMethod02(): Bool =
        "hello world".toUpperCase() == "HELLO WORLD"

    @test
    def testInvokeMethod03(): Bool =
        "hello world".length() == 11

    @test
    def testInvokeMethod04(): Bool =
        "hello world".indexOf("world") == 6

    @test
    def testInvokeMethod05(): Bool =
        "hello world".replaceAll("hello", "goodbye") == "goodbye world"

    @test
    def testInvokePureMethod01(): Bool =
        "HELLO WORLD".toLowerCase() == "hello world"

    @test
    def testInvokeObjectMethod01(): JString =
        "HELLO WORLD".toLowerCase()

    @test
    def testInvokeInheritedMethod01(): Bool \ IO =
        let obj = new TestClassWithInheritedMethod();
        obj.concreteMethod("bar") == "foobar"

   @test
   def testInvokeMethod_01(): Bool =
       let obj = "HELLO WORLD";
       obj.indexOf("E") == 1

   @test
   def testInvokeMethod_02(): Bool =
       let obj = "Hello";
       obj.concat(" world") == "Hello world"

   @test
   def testInvokeMethod_03(): Bool =
       let obj = "Hello world";
       obj.compareTo("Goodbye world") == 1

   @test
   def testInvokeMethod_04(): Bool =
       let obj = "HELLO WORLD";
       obj.charAt(3) == 'L'

   @test
   def testInvokeMethod_05(): Bool =
       let val = -32ii;
       val.add(500ii) == 468ii

   @test
   def testInvokeMethod_06(): Bool =
       let val = 500ii;
       val.compareTo(-80ii) == 1

   @test
   def testInvokeMethod_07(): Bool =
       let val = 150ii;
       val.$and(-100ii) == 148ii

    @test
    def testInvokeMethod_08(): Bool \ IO =
       let val = 123488ii;
       val.$mod(23ii) == 1ii

   @test
   def testInvokeMethod_09(): Bool \ IO =
       let val = 123488ii;
       val.modPow(10ii, 32023ii) == 8247ii

   @test
   def testInvokeMethod_10(): Bool \ IO =
       let val = 9999.999999999ff;
       val.max(9999.99999999ff) == 9999.999999999ff

   @test
   def testInvokeMethod_11(): Bool =
       let val = -98432.23ff;
       val.multiply(42.15ff) == -4148918.4945ff

   @test
   def testInvokeMethod_12(): Bool =
       let val = 9834.003001ff;
       val.pow(4) == 9352362803567718.561955395843171390012001ff

   @test
   def testInvokeMethod_13(): Bool \ IO =
       let val = 9012111.9834ff;
       val.remainder(429.01ff) == 327.9234ff

}

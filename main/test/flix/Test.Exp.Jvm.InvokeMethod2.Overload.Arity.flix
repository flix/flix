mod Test.Exp.Jvm.Overload.Arity {

   @test
   def testInvokeMethod2_01(): Bool \ IO =
      let obj = "HELLO WORLD";
      obj.indexOf("O") == 4

   @test
   def testInvokeMethod2_02(): Bool \ IO =
      let obj = "HELLO WORLD";
      obj.indexOf("Z") == -1

   @test
   def testInvokeMethod2_03(): Bool \ IO =
      let obj = "HELLO WORLD";
      obj.indexOf("W", 5) == 6

   @test
   def testInvokeMethod2_04(): Bool \ IO =
      let obj = "HELLO WORLD";
      obj.indexOf("W", 7) == -1

   @test
   def testInvokeMethod2_05(): Bool \ IO =
      let obj = "HELLO WORLD";
      obj.lastIndexOf("O") == 7

   @test
   def testInvokeMethod2_06(): Bool \ IO =
      let obj = "HELLO WORLD";
      obj.lastIndexOf("Z") == -1

   @test
   def testInvokeMethod2_07(): Bool \ IO =
      let obj = "HELLO WORLD";
      obj.lastIndexOf("L", 10) == 9

   @test
   def testInvokeMethod2_08(): Bool \ IO =
      let obj = "HELLO WORLD";
      obj.lastIndexOf("W", 3) == -1

   @test
   def testInvokeMethod2_09(): Bool \ IO =
      let obj = "Hello World";
      let arr : Array[String, IO] = obj.split(" ");
      Array.get(0, arr) == "Hello"
}

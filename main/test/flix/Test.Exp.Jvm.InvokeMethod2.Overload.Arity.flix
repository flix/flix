mod Test.Exp.Jvm.Overload.Arity {

   @test
   def testInvokeMethod_01(): Bool \ IO =
      let obj = "HELLO WORLD";
      obj.indexOf("O") == 4

   @test
   def testInvokeMethod_02(): Bool \ IO =
      let obj = "HELLO WORLD";
      obj.indexOf("Z") == -1

   @test
   def testInvokeMethod_03(): Bool \ IO =
      let obj = "HELLO WORLD";
      obj.indexOf("W", 5) == 6

   @test
   def testInvokeMethod_04(): Bool \ IO =
      let obj = "HELLO WORLD";
      obj.indexOf("W", 7) == -1

   @test
   def testInvokeMethod_05(): Bool \ IO =
      let obj = "HELLO WORLD";
      obj.lastIndexOf("O") == 7

   @test
   def testInvokeMethod_06(): Bool \ IO =
      let obj = "HELLO WORLD";
      obj.lastIndexOf("Z") == -1

   @test
   def testInvokeMethod_07(): Bool \ IO =
      let obj = "HELLO WORLD";
      obj.lastIndexOf("L", 10) == 9

   @test
   def testInvokeMethod_08(): Bool \ IO =
      let obj = "HELLO WORLD";
      obj.lastIndexOf("W", 3) == -1

   // TODO: support for split with arrays
}

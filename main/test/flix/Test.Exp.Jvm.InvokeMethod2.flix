mod Test.Exp.Jvm.InvokeMethod2 {

   @test
   def testInvokeMethod2_01(): Bool \ IO =
       let obj = "HELLO WORLD";
       obj#indexOf("E") == 1

   @test
   def testInvokeMethod2_02(): Bool \ IO =
       let obj = "HELLO WORLD";
       obj#indexOf("W", 5) == 6

   @test
   def testInvokeMethod2_03(): Bool \ IO =
       let obj = "HELLO WORLD";
       obj#indexOf("W", 7) == -1

   @test
   def testInvokeMethod2_04(): Bool \ IO =
       let obj = "HELLO WORLD";
       obj#charAt(3) == 'L'
}

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

   @test
   def testInvokeMethod2_05(): Bool \ IO =
       let val = -32ii;
       val#add(500ii) == 468ii

   @test
   def testInvokeMethod2_06(): Bool \ IO =
       let val = 500ii;
       val#compareTo(-80ii) == 1

   // TODO: this test doesn't pass because of keyword ambiguity
   //@test
   //def testInvokeMethod2_07(): Bool \ IO =
   //    let val = 150ii;
   //    val#and(-100ii) == 148ii

   // TODO: this doesn't pass for the same reason as and method
   // @test
   // def testInvokeMethod2_08(): Bool \ IO =
   //    let val = 123488ii;
   //    val#mod(23ii) == 1

   @test
   def testInvokeMethod2_09(): Bool \ IO =
       let val = 123488ii;
       val#modPow(10ii, 32023ii) == 8247ii
}

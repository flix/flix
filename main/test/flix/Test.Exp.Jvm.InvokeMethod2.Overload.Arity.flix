mod Test.Exp.Jvm.InvokeMethod2 {

   @test
   def testInvokeMethod2_01(): Bool \ IO =
      let obj = "HELLO WORLD";
      obj#indexOf("O") == 4

   @test
   def testInvokeMethod2_02(): Bool \ IO =
      let obj = "HELLO WORLD";
      obj#indexOf("Z") == -1

   @test
   def testInvokeMethod2_03(): Bool \ IO =
      let obj = "HELLO WORLD";
      obj#indexOf("W", 5) == 6

   @test
   def testInvokeMethod2_04(): Bool \ IO =
      let obj = "HELLO WORLD";
      obj#indexOf("W", 7) == -1

   @test
   def testInvokeMethod2_05(): Bool \ IO =
      let obj = "HELLO WORLD";
      obj#lastIndexOf("O") == 7

   @test
   def testInvokeMethod2_06(): Bool \ IO =
      let obj = "HELLO WORLD";
      obj#lastIndexOf("Z") == -1

   @test
   def testInvokeMethod2_07(): Bool \ IO =
      let obj = "HELLO WORLD";
      obj#lastIndexOf("L", 10) == 9

   @test
   def testInvokeMethod2_08(): Bool \ IO =
      let obj = "HELLO WORLD";
      obj#lastIndexOf("W", 3) == -1

   @test
   def testInvokeMethod2_09(): Bool \ IO =
      let obj = "hello world";
      obj#replace('O', 'i') == "hello world"

   @test
   def testInvokeMethod2_10(): Bool \ IO =
      let obj = "HELLO WORLD";
      obj#replace('L', 'l') == "HEllO WORlD"

   @test
   def testInvokeMethod2_11(): Bool \ IO =
      let obj = "HELLO WORLD";
      obj#replace("LLO", "YO") == "HEYO WORLD"

   @test
   def testInvokeMethod2_12(): Bool \ IO =
      let obj = "HELLO WORLD";
      obj#replace("AZERTY", "QWERTY") == "HELLO WORLD"

   // TODO: support for split with arrays
}

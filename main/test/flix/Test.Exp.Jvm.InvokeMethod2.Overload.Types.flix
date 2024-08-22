mod Test.Exp.Jvm.InvokeMethod2.Overload.Types {

   @test
   def testInvokeMethod2_01(): Bool \ IO =
      let obj = "hello world";
      obj.replace('O', 'i') == "hello world"

   @test
   def testInvokeMethod2_02(): Bool \ IO =
      let obj = "HELLO WORLD";
      obj.replace('L', 'l') == "HEllO WORlD"

   @test
   def testInvokeMethod2_03(): Bool \ IO =
      let obj = "HELLO WORLD";
      obj.replace("LLO", "YO") == "HEYO WORLD"

   @test
   def testInvokeMethod2_04(): Bool \ IO =
      let obj = "HELLO WORLD";
      obj.replace("AZERTY", "QWERTY") == "HELLO WORLD"
}

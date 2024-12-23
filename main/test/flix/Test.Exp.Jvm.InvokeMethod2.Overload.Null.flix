mod Test.Exp.Jvm.InvokeMethod.Overload.Null {


    import java.lang.StringBuilder

   @test
   def testInvokeMethod_01(): Bool \ IO =
      let obj = "hello world";
      not obj.equals(null)

   @test
   def testInvokeMethod_02(): Bool \ IO =
      let obj = "null";
      not obj.equals(null)

   @test
   def testInvokeMethod_03(): Bool \ IO =
      let obj = "txt";
      let val : String = (checked_cast(null) : String);
      not obj.equals(val)

   @test
   def testInvokeMethod_04(): Bool \ IO =
      let sb = new StringBuilder("hello ");
      sb.append((checked_cast(null) : String));
      sb.toString() == "hello null"
}

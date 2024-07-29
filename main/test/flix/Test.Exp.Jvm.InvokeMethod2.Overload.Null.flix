mod Test.Exp.Jvm.InvokeMethod2.Overload.Null {
    import java.lang.{String, StringBuilder};

   @test
   def testInvokeMethod2_01(): Bool \ IO =
      let obj = "hello world";
      not obj.equals(null)

   @test
   def testInvokeMethod2_02(): Bool \ IO =
      let obj = "null";
      not obj.equals(null)

   @test
   def testInvokeMethod2_03(): Bool \ IO =
      let obj = "txt";
      let val : String = (checked_cast(null) : String);
      not obj.equals(val)

   @test
   def testInvokeMethod2_04(): Bool \ IO =
      let sb = new StringBuilder("hello ");
      sb.append((checked_cast(null) : String));
      sb.toString() == "hello null"
}

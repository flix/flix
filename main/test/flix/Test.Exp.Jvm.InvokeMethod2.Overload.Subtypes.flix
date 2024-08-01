mod Test.Exp.Jvm.InvokeMethod2.Overload.Subtypes {

    import java.lang.{Object, StringBuilder, StringBuffer, CharSequence};
    import java.nio.CharBuffer;

   @test
   def testInvokeMethod2_02(): Bool \ IO =
      let cs = CharBuffer.allocate(5);
      cs.append('h');
      cs.append('e');
      cs.append('l');
      cs.append('l');
      cs.append('o');
      let obj = "hello world";
      obj.contains(cs)

   @test
   def testInvokeMethod2_03(): Bool \ IO =
      let sb = new StringBuilder("world");
      let obj = "Hello world";
      obj.contains(sb) and obj.contains(sb.toString())

   @test
   def testInvokeMethod2_04(): Bool \ IO =
      let sb = new StringBuffer("llo ");
      let obj = "Hello world";
      obj.contains(sb) and obj.contains(sb.toString())

   @test
   def testInvokeMethod2_05(): Bool \ IO =
      let txt = "HeLlO WorlD";
      let obj : Object = (checked_cast("HeLlO WorlD") : Object);
      txt.equals(obj)

   @test
   def testInvokeMethod2_06(): Bool \ IO =
      let txt = "HeLlO WorlD";
      let obj : CharSequence = (checked_cast("HeLlO WorlD") : CharSequence);
      txt.equals(obj)
}

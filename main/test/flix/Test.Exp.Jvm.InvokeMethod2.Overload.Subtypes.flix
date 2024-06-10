mod Test.Exp.Jvm.InvokeMethod2.Overload.Subtypes {

   @test
   def testInvokeMethod2_01(): Bool \ IO =
       let obj = "Hello world";
       obj#contains("o w")

   // TODO: use CharBuffer with static call once available

   //def testInvokeMethod2_02(): Bool \ IO =
   //   import java_new java.lang.StringBuilder(String): ##java.lang.StringBuilder \ IO as newSB;
   //   let sb = newSB("world");
   //   let obj = "Hello world";
   //   obj#contains(sb#toString())
}

mod Test.Exp.Jvm.InvokeMethod2.Interfaces {

   // Test possible Object's duplicate definitions

   import java.util.{Map => JMap}

   @test
   def testInvokeMethod2Interfaces_01(): Bool \ IO =
       let m = Adaptor.toMap(Map#{"a" => "hello"});
       unsafe JMap.of("a", "hello").equals(m)

   @test
   def testInvokeMethod2Interfaces_02(): Int32 \ IO =
       unsafe JMap.of("b", "d").hashCode()
}

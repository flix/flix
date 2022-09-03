import java.lang.{StringBuffer, CharSequence => ASeqOfChars}

namespace Test/Import {

  @test
  def testImport01(): Bool \ IO =
    import new java.lang.StringBuffer(String): StringBuffer as newStringBuffer;
    import java.lang.StringBuffer.subSequence(Int32, Int32): ASeqOfChars;
    let sb = newStringBuffer("Hello world");
    let subStr = subSequence(sb, 6, 11) as String;
    Assert.eq(subStr, "world")
}

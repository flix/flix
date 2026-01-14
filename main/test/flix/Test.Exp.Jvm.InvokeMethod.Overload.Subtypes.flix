mod Test.Exp.Jvm.InvokeMethod.Overload.Subtypes {

    use Assert.assertTrue

    import java.lang.CharSequence
    import java.lang.Object
    import java.lang.StringBuffer
    import java.lang.StringBuilder
    import java.nio.CharBuffer

    @Test
    def testInvokeMethod_01(): Unit \ Assert + IO =
        let cs = CharBuffer.allocate(5);
        cs.append('h');
        cs.append('e');
        cs.append('l');
        cs.append('l');
        cs.append('o');
        let obj = "hello world";
        assertTrue(obj.contains(cs))

    @Test
    def testInvokeMethod_02(): Unit \ Assert + IO =
        let sb = new StringBuilder("world");
        let obj = "Hello world";
        assertTrue(obj.contains(sb) and obj.contains(sb.toString()))

    @Test
    def testInvokeMethod_03(): Unit \ Assert + IO =
        let sb = new StringBuffer("llo ");
        let obj = "Hello world";
        assertTrue(obj.contains(sb) and obj.contains(sb.toString()))

    @Test
    def testInvokeMethod_04(): Unit \ Assert =
        let txt = "HeLlO WorlD";
        let obj: Object = (checked_cast("HeLlO WorlD"): Object);
        assertTrue(txt.equals(obj))

    @Test
    def testInvokeMethod_05(): Unit \ Assert =
        let txt = "HeLlO WorlD";
        let obj: CharSequence = (checked_cast("HeLlO WorlD"): CharSequence);
        assertTrue(txt.equals(obj))

}

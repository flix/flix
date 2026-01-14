mod Test.Exp.Jvm.Instanceof {

    use Assert.{assertTrue, assertFalse}

    import java.lang.CharSequence
    import java.lang.Enum
    import java.lang.Long
    import java.lang.Number
    import java.lang.Object
    import java.lang.{String => JString}
    import java.lang.StringBuilder
    import java.math.BigInteger
    import java.nio.file.OpenOption
    import java.nio.file.StandardOpenOption

    @Test
    def testInstanceof01(): Unit \ Assert =
        assertTrue("Hello World" instanceof JString)

    @Test
    def testInstanceof02(): Unit \ Assert =
        assertTrue("Hello World" instanceof CharSequence)

    @Test
    def testInstanceof03(): Unit \ Assert =
        assertTrue("Hello World" instanceof Object)

    @Test
    def testInstanceof04(): Unit \ Assert =
        assertTrue(100ii instanceof BigInteger)

    @Test
    def testInstanceof05(): Unit \ Assert =
        assertTrue(100ii instanceof Number)

    @Test
    def testInstanceof06(): Unit \ Assert =
        assertTrue(100ii instanceof Object)

    @Test
    def testInstanceof07(): Unit \ Assert =
        assertTrue(StandardOpenOption.DELETE_ON_CLOSE instanceof StandardOpenOption)

    @Test
    def testInstanceof08(): Unit \ Assert =
        assertTrue(StandardOpenOption.DELETE_ON_CLOSE instanceof OpenOption)

    @Test
    def testInstanceof09(): Unit \ Assert =
        assertTrue(StandardOpenOption.DELETE_ON_CLOSE instanceof Enum)

    @Test
    def testInstanceof10(): Unit \ Assert =
        assertTrue(StandardOpenOption.DELETE_ON_CLOSE instanceof Object)

    @Test
    def testInstanceof11(): Unit \ Assert =
        assertFalse(100ii instanceof Long)

    @Test
    def testInstanceof12(): Unit \ Assert =
        assertFalse("Hello World" instanceof StringBuilder)

    @Test
    def testInstanceof13(): Unit \ Assert + IO = {
        let arr = Array#{"hi"} @ Static;
        assertTrue(Array.get(0, arr) instanceof JString)
    }

    @Test
    def testInstanceof14(): Unit \ Assert =
        assertFalse(123 instanceof Number)

    @Test
    def testInstanceof15(): Unit \ Assert =
        assertFalse(true instanceof Object)
}

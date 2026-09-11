mod Test.Exp.Jvm.PrimitiveWidening {

    use Assert.assertEq

    import java.lang.{Double => JDouble}
    import java.lang.{Float => JFloat}
    import java.lang.{Integer => JInteger}
    import java.lang.{Long => JLong}
    import java.lang.Math
    import java.lang.StringBuilder
    import java.util.concurrent.atomic.AtomicLong

    //
    // Static methods
    //

    @Test
    def testStaticInt32ToInt64(): Unit \ Assert =
        // Long.valueOf(long)
        assertEq(expected = 42i64, JLong.valueOf(42).longValue())

    @Test
    def testStaticInt8ToInt64(): Unit \ Assert + IO =
        // Math.max(long, long)
        assertEq(expected = 2i64, Math.max(1i8, 2i64))

    @Test
    def testStaticInt16ToInt64(): Unit \ Assert + IO =
        assertEq(expected = 300i64, Math.max(300i16, 2i64))

    @Test
    def testStaticCharToInt32(): Unit \ Assert =
        // Integer.valueOf(int)
        assertEq(expected = 65, JInteger.valueOf('A').intValue())

    @Test
    def testStaticInt32ToFloat32(): Unit \ Assert =
        // Float.valueOf(float)
        assertEq(expected = 42.0f32, JFloat.valueOf(42).floatValue())

    @Test
    def testStaticInt32ToFloat64(): Unit \ Assert + IO =
        // Math.sqrt(double)
        assertEq(expected = 4.0f64, Math.sqrt(16))

    @Test
    def testStaticInt64ToFloat32(): Unit \ Assert =
        assertEq(expected = 42.0f32, JFloat.valueOf(42i64).floatValue())

    @Test
    def testStaticInt64ToFloat64(): Unit \ Assert + IO =
        assertEq(expected = 4.0f64, Math.sqrt(16i64))

    @Test
    def testStaticFloat32ToFloat64(): Unit \ Assert + IO =
        assertEq(expected = 4.0f64, Math.sqrt(16.0f32))

    //
    // Instance methods
    //

    @Test
    def testInstanceInt32ToInt64(): Unit \ Assert + IO =
        // AtomicLong.set(long)
        let a = new AtomicLong(0i64);
        a.set(42);
        assertEq(expected = 42i64, a.get())

    @Test
    def testInstanceInt8ToInt64(): Unit \ Assert + IO =
        // AtomicLong.addAndGet(long)
        let a = new AtomicLong(40i64);
        assertEq(expected = 42i64, a.addAndGet(2i8))

    @Test
    def testInstanceInt8ToInt32(): Unit \ Assert + IO =
        // StringBuilder.append(int) needs no instruction since a byte is an int on the stack
        let sb = new StringBuilder();
        assertEq(expected = "1", sb.append(1i8).toString())

    //
    // Constructors
    //

    @Test
    def testConstructorInt32ToInt64(): Unit \ Assert + IO =
        // AtomicLong(long)
        let a = new AtomicLong(42);
        assertEq(expected = 42i64, a.get())

    @Test
    def testConstructorInt16ToInt64(): Unit \ Assert =
        // Long(long)
        assertEq(expected = 42i64, (new JLong(42i16)).longValue())

    @Test
    def testConstructorInt32ToFloat64(): Unit \ Assert =
        // Double(double)
        assertEq(expected = 42.0f64, (new JDouble(42)).doubleValue())

}

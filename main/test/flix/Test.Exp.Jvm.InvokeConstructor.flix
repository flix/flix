mod Test.Exp.Jvm.InvokeConstructor {

    import java.lang.Boolean
    import java.lang.Byte
    import java.lang.Character
    import java.lang.Double
    import java.lang.Float
    import java.lang.Integer
    import java.lang.Long
    import java.lang.Object
    import java.lang.ProcessBuilder
    import java.lang.Short
    import java.lang.{String => JString}
    import java.io.File
    import java.math.BigDecimal
    import java.math.BigInteger
    import java.net.InetSocketAddress
    import java.net.URL
    import java.net.URLClassLoader
    import java.util.HashMap
    import java.util.{Locale$Builder => Builder}
    import java.util.StringTokenizer

    @CompileTest
    def testInvokeBigDecimalConstructor01(): BigDecimal =
        new BigDecimal(new BigInteger("12345"), 2)

    @CompileTest
    def testInvokeBigIntegerConstructor01(): BigInteger =
        new BigInteger("FF", 16)

    @CompileTest
    def testInvokeBooleanConstructor01(): Boolean =
        new Boolean(true)

    @CompileTest
    def testInvokeByteConstructor01(): Byte =
        new Byte(123i8)

    @CompileTest
    def testInvokeCharacterConstructor01(): Character =
        new Character('a')

    @CompileTest
    def testInvokeDoubleConstructor01(): Double =
        new Double(123.456f64)

    @CompileTest
    def testInvokeFileConstructor01(): File \ IO =
        new File("/tmp", "test.txt")

    @CompileTest
    def testInvokeFloatConstructor01(): Float =
        new Float(123.456f32)

    @CompileTest
    def testInvokeHashMapConstructor01(): HashMap \ IO =
        new HashMap(16, 0.75f32)

    @CompileTest
    def testInvokeInetSocketAddressConstructor01(): InetSocketAddress \ IO =
        new InetSocketAddress("localhost", 8080)

    @CompileTest
    def testInvokeIntegerConstructor01(): Integer =
        new Integer(123i32)

    @CompileTest
    def testInvokeLocaleBuilderConstructor01(): Builder \ IO =
        new Builder()

    @CompileTest
    def testInvokeLongConstructor01(): Long =
        new Long(123i64)

    @CompileTest
    def testInvokeObjectConstructor01(): Object \ IO =
        let obj = new JString("Hello World");
        (checked_cast(obj) : Object)

    @CompileTest
    def testInvokeProcessBuilderConstructor01(): ProcessBuilder \ IO =
        new ProcessBuilder(...{"echo"})

    @CompileTest
    def testInvokeProcessBuilderConstructor02(): ProcessBuilder \ IO =
        new ProcessBuilder(...{"echo", "hello"})

    @CompileTest
    def testInvokeProcessBuilderConstructor03(): ProcessBuilder \ IO =
        new ProcessBuilder(...{"echo", "hello", "world"})

    @CompileTest
    def testInvokeShortConstructor01(): Short =
        new Short(123i16)

    @CompileTest
    def testInvokeStringConstructor01(): JString \ IO =
        new JString("Hello World")

    @CompileTest
    def testInvokeStringTokenizerConstructor01(): StringTokenizer \ IO =
        new StringTokenizer("hello,world", ",")

    @CompileTest
    def testInvokeURLClassLoaderConstructor01(): URLClassLoader \ IO =
        new URLClassLoader((...{}: Vector[URL]))

    @CompileTest
    def testInvokeURLConstructor01(): URL \ IO =
        new URL("http", "example.com", 80, "/path")

}

mod Test.Import.Class {

    import java.io.File
    import java.lang.Runtime
    import java.lang.StringBuffer
    import java.lang.StringBuilder
    import java.lang.Thread
    import java.math.BigDecimal
    import java.math.BigInteger
    import java.nio.file.Path
    import java.util.Locale
    import java.util.concurrent.atomic.AtomicLong

    @CompileTest
    def importClass01(x: File): File = x

    @CompileTest
    def importClass02(x: Runtime): Runtime = x

    @CompileTest
    def importClass03(x: StringBuffer): StringBuffer = x

    @CompileTest
    def importClass04(x: StringBuilder): StringBuilder = x

    @CompileTest
    def importClass05(x: Thread): Thread = x

    @CompileTest
    def importClass06(x: BigDecimal): BigDecimal = x

    @CompileTest
    def importClass07(x: BigInteger): BigInteger = x

    @CompileTest
    def importClass08(x: Path): Path = x

    @CompileTest
    def importClass09(x: Locale): Locale = x

    @CompileTest
    def importClass10(x: AtomicLong): AtomicLong = x

    pub enum E {
        case C01(File)
        case C02(Runtime)
        case C03(StringBuffer)
        case C04(StringBuilder)
        case C05(Thread)
        case C06(BigDecimal)
        case C07(BigInteger)
        case C08(Path)
        case C09(Locale)
        case C10(AtomicLong)
    }

}

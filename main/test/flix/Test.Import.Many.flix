mod Test.Import.Many {

    import java.io.{File, InputStream, OutputStream, Reader, Writer}
    import java.lang.{Boolean, Byte, Character, Double, Float, Integer, Long, Number, Short, System}
    import java.nio.charset.{Charset, CharsetDecoder, CharsetEncoder}
    import java.util.{Date, Locale, Random, TimeZone, UUID}
    import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicLong}

    // java.io
    @CompileTest
    def importMany01(x: File): File = x

    @CompileTest
    def importMany02(x: InputStream): InputStream = x

    @CompileTest
    def importMany03(x: OutputStream): OutputStream = x

    @CompileTest
    def importMany04(x: Reader): Reader = x

    @CompileTest
    def importMany05(x: Writer): Writer = x

    // java.lang
    @CompileTest
    def importMany06(x: Boolean): Boolean = x

    @CompileTest
    def importMany07(x: Byte): Byte = x

    @CompileTest
    def importMany08(x: Character): Character = x

    @CompileTest
    def importMany09(x: Double): Double = x

    @CompileTest
    def importMany10(x: Float): Float = x

    @CompileTest
    def importMany11(x: Integer): Integer = x

    @CompileTest
    def importMany12(x: Long): Long = x

    @CompileTest
    def importMany13(x: Number): Number = x

    @CompileTest
    def importMany14(x: Short): Short = x

    @CompileTest
    def importMany15(x: System): System = x

    // java.nio.charset
    @CompileTest
    def importMany16(x: Charset): Charset = x

    @CompileTest
    def importMany17(x: CharsetDecoder): CharsetDecoder = x

    @CompileTest
    def importMany18(x: CharsetEncoder): CharsetEncoder = x

    // java.util
    @CompileTest
    def importMany19(x: Date): Date = x

    @CompileTest
    def importMany20(x: Locale): Locale = x

    @CompileTest
    def importMany21(x: Random): Random = x

    @CompileTest
    def importMany22(x: TimeZone): TimeZone = x

    @CompileTest
    def importMany23(x: UUID): UUID = x

    // java.util.concurrent.atomic
    @CompileTest
    def importMany24(x: AtomicBoolean): AtomicBoolean = x

    @CompileTest
    def importMany25(x: AtomicInteger): AtomicInteger = x

    @CompileTest
    def importMany26(x: AtomicLong): AtomicLong = x

    pub enum E {
        case C01(File)
        case C02(InputStream)
        case C03(OutputStream)
        case C04(Reader)
        case C05(Writer)
        case C06(Boolean)
        case C07(Byte)
        case C08(Character)
        case C09(Double)
        case C10(Float)
        case C11(Integer)
        case C12(Long)
        case C13(Number)
        case C14(Short)
        case C15(System)
        case C16(Charset)
        case C17(CharsetDecoder)
        case C18(CharsetEncoder)
        case C19(Date)
        case C20(Locale)
        case C21(Random)
        case C22(TimeZone)
        case C23(UUID)
        case C24(AtomicBoolean)
        case C25(AtomicInteger)
        case C26(AtomicLong)
    }

}

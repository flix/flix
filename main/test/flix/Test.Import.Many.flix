mod Test.Import.Many {

    import java.io.{File, InputStream, OutputStream, Reader, Writer}
    import java.lang.{Boolean, Byte, Character, Double, Float, Integer, Long, Number, Short, System}
    import java.util.{Date, Locale, Random, TimeZone, UUID}

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

    // java.util
    @CompileTest
    def importMany16(x: Date): Date = x

    @CompileTest
    def importMany17(x: Locale): Locale = x

    @CompileTest
    def importMany18(x: Random): Random = x

    @CompileTest
    def importMany19(x: TimeZone): TimeZone = x

    @CompileTest
    def importMany20(x: UUID): UUID = x

}

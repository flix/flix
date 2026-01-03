mod Test.Import.Many {

    import java.io.{File, InputStream, OutputStream, Reader, Writer}
    import java.lang.{Boolean, Byte, Character, Double, Float, Integer, Long, Number, Short, System}
    import java.nio.charset.{Charset, CharsetDecoder, CharsetEncoder}
    import java.util.{Date, Locale, Random, TimeZone, UUID}
    import java.util.concurrent.atomic.{AtomicBoolean, AtomicInteger, AtomicLong}

    // java.io
    @CompileTest
    def importMany01(x: File): File = (x: File)

    @CompileTest
    def importMany02(x: InputStream): InputStream = (x: InputStream)

    @CompileTest
    def importMany03(x: OutputStream): OutputStream = (x: OutputStream)

    @CompileTest
    def importMany04(x: Reader): Reader = (x: Reader)

    @CompileTest
    def importMany05(x: Writer): Writer = (x: Writer)

    // java.lang
    @CompileTest
    def importMany06(x: Boolean): Boolean = (x: Boolean)

    @CompileTest
    def importMany07(x: Byte): Byte = (x: Byte)

    @CompileTest
    def importMany08(x: Character): Character = (x: Character)

    @CompileTest
    def importMany09(x: Double): Double = (x: Double)

    @CompileTest
    def importMany10(x: Float): Float = (x: Float)

    @CompileTest
    def importMany11(x: Integer): Integer = (x: Integer)

    @CompileTest
    def importMany12(x: Long): Long = (x: Long)

    @CompileTest
    def importMany13(x: Number): Number = (x: Number)

    @CompileTest
    def importMany14(x: Short): Short = (x: Short)

    @CompileTest
    def importMany15(x: System): System = (x: System)

    // java.nio.charset
    @CompileTest
    def importMany16(x: Charset): Charset = (x: Charset)

    @CompileTest
    def importMany17(x: CharsetDecoder): CharsetDecoder = (x: CharsetDecoder)

    @CompileTest
    def importMany18(x: CharsetEncoder): CharsetEncoder = (x: CharsetEncoder)

    // java.util
    @CompileTest
    def importMany19(x: Date): Date = (x: Date)

    @CompileTest
    def importMany20(x: Locale): Locale = (x: Locale)

    @CompileTest
    def importMany21(x: Random): Random = (x: Random)

    @CompileTest
    def importMany22(x: TimeZone): TimeZone = (x: TimeZone)

    @CompileTest
    def importMany23(x: UUID): UUID = (x: UUID)

    // java.util.concurrent.atomic
    @CompileTest
    def importMany24(x: AtomicBoolean): AtomicBoolean = (x: AtomicBoolean)

    @CompileTest
    def importMany25(x: AtomicInteger): AtomicInteger = (x: AtomicInteger)

    @CompileTest
    def importMany26(x: AtomicLong): AtomicLong = (x: AtomicLong)

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

    type alias A01 = File
    type alias A02 = InputStream
    type alias A03 = OutputStream
    type alias A04 = Reader
    type alias A05 = Writer
    type alias A06 = Boolean
    type alias A07 = Byte
    type alias A08 = Character
    type alias A09 = Double
    type alias A10 = Float
    type alias A11 = Integer
    type alias A12 = Long
    type alias A13 = Number
    type alias A14 = Short
    type alias A15 = System
    type alias A16 = Charset
    type alias A17 = CharsetDecoder
    type alias A18 = CharsetEncoder
    type alias A19 = Date
    type alias A20 = Locale
    type alias A21 = Random
    type alias A22 = TimeZone
    type alias A23 = UUID
    type alias A24 = AtomicBoolean
    type alias A25 = AtomicInteger
    type alias A26 = AtomicLong

    pub struct S[r] {
        f01: File,
        f02: InputStream,
        f03: OutputStream,
        f04: Reader,
        f05: Writer,
        f06: Boolean,
        f07: Byte,
        f08: Character,
        f09: Double,
        f10: Float,
        f11: Integer,
        f12: Long,
        f13: Number,
        f14: Short,
        f15: System,
        f16: Charset,
        f17: CharsetDecoder,
        f18: CharsetEncoder,
        f19: Date,
        f20: Locale,
        f21: Random,
        f22: TimeZone,
        f23: UUID,
        f24: AtomicBoolean,
        f25: AtomicInteger,
        f26: AtomicLong
    }

    pub trait T[a] {
        pub def f01(x: a): File
        pub def f02(x: a): InputStream
        pub def f03(x: a): OutputStream
        pub def f04(x: a): Reader
        pub def f05(x: a): Writer
        pub def f06(x: a): Boolean
        pub def f07(x: a): Byte
        pub def f08(x: a): Character
        pub def f09(x: a): Double
        pub def f10(x: a): Float
        pub def f11(x: a): Integer
        pub def f12(x: a): Long
        pub def f13(x: a): Number
        pub def f14(x: a): Short
        pub def f15(x: a): System
        pub def f16(x: a): Charset
        pub def f17(x: a): CharsetDecoder
        pub def f18(x: a): CharsetEncoder
        pub def f19(x: a): Date
        pub def f20(x: a): Locale
        pub def f21(x: a): Random
        pub def f22(x: a): TimeZone
        pub def f23(x: a): UUID
        pub def f24(x: a): AtomicBoolean
        pub def f25(x: a): AtomicInteger
        pub def f26(x: a): AtomicLong
    }

    instance T[Unit] {
        pub def f01(_x: Unit): File = ???
        pub def f02(_x: Unit): InputStream = ???
        pub def f03(_x: Unit): OutputStream = ???
        pub def f04(_x: Unit): Reader = ???
        pub def f05(_x: Unit): Writer = ???
        pub def f06(_x: Unit): Boolean = ???
        pub def f07(_x: Unit): Byte = ???
        pub def f08(_x: Unit): Character = ???
        pub def f09(_x: Unit): Double = ???
        pub def f10(_x: Unit): Float = ???
        pub def f11(_x: Unit): Integer = ???
        pub def f12(_x: Unit): Long = ???
        pub def f13(_x: Unit): Number = ???
        pub def f14(_x: Unit): Short = ???
        pub def f15(_x: Unit): System = ???
        pub def f16(_x: Unit): Charset = ???
        pub def f17(_x: Unit): CharsetDecoder = ???
        pub def f18(_x: Unit): CharsetEncoder = ???
        pub def f19(_x: Unit): Date = ???
        pub def f20(_x: Unit): Locale = ???
        pub def f21(_x: Unit): Random = ???
        pub def f22(_x: Unit): TimeZone = ???
        pub def f23(_x: Unit): UUID = ???
        pub def f24(_x: Unit): AtomicBoolean = ???
        pub def f25(_x: Unit): AtomicInteger = ???
        pub def f26(_x: Unit): AtomicLong = ???
    }

    pub eff F {
        def f01(x: File): File
        def f02(x: InputStream): InputStream
        def f03(x: OutputStream): OutputStream
        def f04(x: Reader): Reader
        def f05(x: Writer): Writer
        def f06(x: Boolean): Boolean
        def f07(x: Byte): Byte
        def f08(x: Character): Character
        def f09(x: Double): Double
        def f10(x: Float): Float
        def f11(x: Integer): Integer
        def f12(x: Long): Long
        def f13(x: Number): Number
        def f14(x: Short): Short
        def f15(x: System): System
        def f16(x: Charset): Charset
        def f17(x: CharsetDecoder): CharsetDecoder
        def f18(x: CharsetEncoder): CharsetEncoder
        def f19(x: Date): Date
        def f20(x: Locale): Locale
        def f21(x: Random): Random
        def f22(x: TimeZone): TimeZone
        def f23(x: UUID): UUID
        def f24(x: AtomicBoolean): AtomicBoolean
        def f25(x: AtomicInteger): AtomicInteger
        def f26(x: AtomicLong): AtomicLong
    }

}

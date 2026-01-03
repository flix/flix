mod Test.Import.Class {

    import java.io.File
    import java.lang.Runtime
    import java.lang.StringBuffer
    import java.lang.StringBuilder
    import java.lang.Thread
    import java.math.BigDecimal
    import java.math.BigInteger
    import java.util.Date
    import java.util.Locale
    import java.util.Random

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
    def importClass08(x: Date): Date = x

    @CompileTest
    def importClass09(x: Locale): Locale = x

    @CompileTest
    def importClass10(x: Random): Random = x

}

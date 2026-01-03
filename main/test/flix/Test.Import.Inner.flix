mod Test.Import.Inner {

    import java.lang.{Character$Subset => CharSubset}
    import java.lang.{Character$UnicodeBlock => UnicodeBlock}
    import java.lang.{ProcessBuilder$Redirect => Redirect}
    import java.lang.{System$Logger => SystemLogger}
    import java.lang.{Thread$State => ThreadState}
    import java.lang.{Thread$UncaughtExceptionHandler => UncaughtExceptionHandler}
    import java.lang.invoke.{MethodHandles$Lookup => Lookup}
    import java.util.{Formatter$BigDecimalLayoutForm => BigDecimalLayoutForm}
    import java.util.{Locale$Builder => LocaleBuilder}
    import java.util.{Locale$Category => LocaleCategory}

    @CompileTest
    def importInner01(x: CharSubset): CharSubset = x

    @CompileTest
    def importInner02(x: UnicodeBlock): UnicodeBlock = x

    @CompileTest
    def importInner03(x: Redirect): Redirect = x

    @CompileTest
    def importInner04(x: SystemLogger): SystemLogger = x

    @CompileTest
    def importInner05(x: ThreadState): ThreadState = x

    @CompileTest
    def importInner06(x: UncaughtExceptionHandler): UncaughtExceptionHandler = x

    @CompileTest
    def importInner07(x: Lookup): Lookup = x

    @CompileTest
    def importInner08(x: BigDecimalLayoutForm): BigDecimalLayoutForm = x

    @CompileTest
    def importInner09(x: LocaleBuilder): LocaleBuilder = x

    @CompileTest
    def importInner10(x: LocaleCategory): LocaleCategory = x

    pub enum E {
        case C01(CharSubset)
        case C02(UnicodeBlock)
        case C03(Redirect)
        case C04(SystemLogger)
        case C05(ThreadState)
        case C06(UncaughtExceptionHandler)
        case C07(Lookup)
        case C08(BigDecimalLayoutForm)
        case C09(LocaleBuilder)
        case C10(LocaleCategory)
    }

    type alias A01 = CharSubset
    type alias A02 = UnicodeBlock
    type alias A03 = Redirect
    type alias A04 = SystemLogger
    type alias A05 = ThreadState
    type alias A06 = UncaughtExceptionHandler
    type alias A07 = Lookup
    type alias A08 = BigDecimalLayoutForm
    type alias A09 = LocaleBuilder
    type alias A10 = LocaleCategory

    pub struct S[r] {
        f01: CharSubset,
        f02: UnicodeBlock,
        f03: Redirect,
        f04: SystemLogger,
        f05: ThreadState,
        f06: UncaughtExceptionHandler,
        f07: Lookup,
        f08: BigDecimalLayoutForm,
        f09: LocaleBuilder,
        f10: LocaleCategory
    }

}

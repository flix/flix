mod Test.Import.Alias {

    import java.io.{File => JFile}
    import java.lang.{Object => JObject}
    import java.lang.{Process => JProcess}
    import java.lang.{Runtime => JRuntime}
    import java.lang.{StringBuffer => JStringBuffer}
    import java.lang.{StringBuilder => JStringBuilder}
    import java.lang.{Thread => JThread}
    import java.nio.charset.{Charset => JCharset}
    import java.util.{Locale => JLocale}
    import java.util.concurrent.atomic.{AtomicInteger => JAtomicInteger}

    @CompileTest
    def importAlias01(x: JFile): JFile = (x: JFile)

    @CompileTest
    def importAlias02(x: JObject): JObject = (x: JObject)

    @CompileTest
    def importAlias03(x: JProcess): JProcess = (x: JProcess)

    @CompileTest
    def importAlias04(x: JRuntime): JRuntime = (x: JRuntime)

    @CompileTest
    def importAlias05(x: JStringBuffer): JStringBuffer = (x: JStringBuffer)

    @CompileTest
    def importAlias06(x: JStringBuilder): JStringBuilder = (x: JStringBuilder)

    @CompileTest
    def importAlias07(x: JThread): JThread = (x: JThread)

    @CompileTest
    def importAlias08(x: JCharset): JCharset = (x: JCharset)

    @CompileTest
    def importAlias09(x: JLocale): JLocale = (x: JLocale)

    @CompileTest
    def importAlias10(x: JAtomicInteger): JAtomicInteger = (x: JAtomicInteger)

    pub enum E {
        case C01(JFile)
        case C02(JObject)
        case C03(JProcess)
        case C04(JRuntime)
        case C05(JStringBuffer)
        case C06(JStringBuilder)
        case C07(JThread)
        case C08(JCharset)
        case C09(JLocale)
        case C10(JAtomicInteger)
    }

    type alias A01 = JFile
    type alias A02 = JObject
    type alias A03 = JProcess
    type alias A04 = JRuntime
    type alias A05 = JStringBuffer
    type alias A06 = JStringBuilder
    type alias A07 = JThread
    type alias A08 = JCharset
    type alias A09 = JLocale
    type alias A10 = JAtomicInteger

    pub struct S[r] {
        f01: JFile,
        f02: JObject,
        f03: JProcess,
        f04: JRuntime,
        f05: JStringBuffer,
        f06: JStringBuilder,
        f07: JThread,
        f08: JCharset,
        f09: JLocale,
        f10: JAtomicInteger
    }

    pub trait T[a] {
        pub def f01(x: a): JFile
        pub def f02(x: a): JObject
        pub def f03(x: a): JProcess
        pub def f04(x: a): JRuntime
        pub def f05(x: a): JStringBuffer
        pub def f06(x: a): JStringBuilder
        pub def f07(x: a): JThread
        pub def f08(x: a): JCharset
        pub def f09(x: a): JLocale
        pub def f10(x: a): JAtomicInteger
    }

    instance T[Unit] {
        pub def f01(_x: Unit): JFile = ???
        pub def f02(_x: Unit): JObject = ???
        pub def f03(_x: Unit): JProcess = ???
        pub def f04(_x: Unit): JRuntime = ???
        pub def f05(_x: Unit): JStringBuffer = ???
        pub def f06(_x: Unit): JStringBuilder = ???
        pub def f07(_x: Unit): JThread = ???
        pub def f08(_x: Unit): JCharset = ???
        pub def f09(_x: Unit): JLocale = ???
        pub def f10(_x: Unit): JAtomicInteger = ???
    }

    pub eff F {
        def f01(x: JFile): JFile
        def f02(x: JObject): JObject
        def f03(x: JProcess): JProcess
        def f04(x: JRuntime): JRuntime
        def f05(x: JStringBuffer): JStringBuffer
        def f06(x: JStringBuilder): JStringBuilder
        def f07(x: JThread): JThread
        def f08(x: JCharset): JCharset
        def f09(x: JLocale): JLocale
        def f10(x: JAtomicInteger): JAtomicInteger
    }

}

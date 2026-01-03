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
    def importAlias01(x: JFile): JFile = x

    @CompileTest
    def importAlias02(x: JObject): JObject = x

    @CompileTest
    def importAlias03(x: JProcess): JProcess = x

    @CompileTest
    def importAlias04(x: JRuntime): JRuntime = x

    @CompileTest
    def importAlias05(x: JStringBuffer): JStringBuffer = x

    @CompileTest
    def importAlias06(x: JStringBuilder): JStringBuilder = x

    @CompileTest
    def importAlias07(x: JThread): JThread = x

    @CompileTest
    def importAlias08(x: JCharset): JCharset = x

    @CompileTest
    def importAlias09(x: JLocale): JLocale = x

    @CompileTest
    def importAlias10(x: JAtomicInteger): JAtomicInteger = x

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

}

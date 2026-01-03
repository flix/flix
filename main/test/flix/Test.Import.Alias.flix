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

}

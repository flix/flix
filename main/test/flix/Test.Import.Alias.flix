mod Test.Import.Alias {

    import java.io.{File => JFile}
    import java.lang.{Object => JObject}
    import java.lang.{Process => JProcess}
    import java.lang.{Runtime => JRuntime}
    import java.lang.{StringBuffer => JStringBuffer}
    import java.lang.{StringBuilder => JStringBuilder}
    import java.lang.{Thread => JThread}
    import java.util.{Date => JDate}
    import java.util.{Locale => JLocale}
    import java.util.{Random => JRandom}

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
    def importAlias08(x: JDate): JDate = x

    @CompileTest
    def importAlias09(x: JLocale): JLocale = x

    @CompileTest
    def importAlias10(x: JRandom): JRandom = x

}

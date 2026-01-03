mod Test.Import.Interface {

    import java.io.Closeable
    import java.io.Flushable
    import java.io.Serializable
    import java.lang.Appendable
    import java.lang.AutoCloseable
    import java.lang.CharSequence
    import java.lang.Cloneable
    import java.lang.Readable
    import java.lang.Runnable
    import java.util.EventListener

    @CompileTest
    def importInterface01(x: Closeable): Closeable = x

    @CompileTest
    def importInterface02(x: Flushable): Flushable = x

    @CompileTest
    def importInterface03(x: Serializable): Serializable = x

    @CompileTest
    def importInterface04(x: Appendable): Appendable = x

    @CompileTest
    def importInterface05(x: AutoCloseable): AutoCloseable = x

    @CompileTest
    def importInterface06(x: CharSequence): CharSequence = x

    @CompileTest
    def importInterface07(x: Cloneable): Cloneable = x

    @CompileTest
    def importInterface08(x: Readable): Readable = x

    @CompileTest
    def importInterface09(x: Runnable): Runnable = x

    @CompileTest
    def importInterface10(x: EventListener): EventListener = x

}

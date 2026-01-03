mod Test.Import.Interface {

    import java.io.Closeable
    import java.io.Flushable
    import java.lang.Appendable
    import java.lang.AutoCloseable
    import java.lang.CharSequence
    import java.lang.Cloneable
    import java.lang.Readable
    import java.lang.Runnable
    import java.util.concurrent.Future
    import java.util.concurrent.locks.Lock

    @CompileTest
    def importInterface01(x: Closeable): Closeable = x

    @CompileTest
    def importInterface02(x: Flushable): Flushable = x

    @CompileTest
    def importInterface03(x: Appendable): Appendable = x

    @CompileTest
    def importInterface04(x: AutoCloseable): AutoCloseable = x

    @CompileTest
    def importInterface05(x: CharSequence): CharSequence = x

    @CompileTest
    def importInterface06(x: Cloneable): Cloneable = x

    @CompileTest
    def importInterface07(x: Readable): Readable = x

    @CompileTest
    def importInterface08(x: Runnable): Runnable = x

    @CompileTest
    def importInterface09(x: Future): Future = x

    @CompileTest
    def importInterface10(x: Lock): Lock = x

    pub enum E {
        case C01(Closeable)
        case C02(Flushable)
        case C03(Appendable)
        case C04(AutoCloseable)
        case C05(CharSequence)
        case C06(Cloneable)
        case C07(Readable)
        case C08(Runnable)
        case C09(Future)
        case C10(Lock)
    }

}

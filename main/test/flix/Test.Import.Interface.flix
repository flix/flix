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

    type alias A01 = Closeable
    type alias A02 = Flushable
    type alias A03 = Appendable
    type alias A04 = AutoCloseable
    type alias A05 = CharSequence
    type alias A06 = Cloneable
    type alias A07 = Readable
    type alias A08 = Runnable
    type alias A09 = Future
    type alias A10 = Lock

    pub struct S[r] {
        f01: Closeable,
        f02: Flushable,
        f03: Appendable,
        f04: AutoCloseable,
        f05: CharSequence,
        f06: Cloneable,
        f07: Readable,
        f08: Runnable,
        f09: Future,
        f10: Lock
    }

    pub trait T[a] {
        pub def f01(x: a): Closeable
        pub def f02(x: a): Flushable
        pub def f03(x: a): Appendable
        pub def f04(x: a): AutoCloseable
        pub def f05(x: a): CharSequence
        pub def f06(x: a): Cloneable
        pub def f07(x: a): Readable
        pub def f08(x: a): Runnable
        pub def f09(x: a): Future
        pub def f10(x: a): Lock
    }

    instance T[Unit] {
        pub def f01(_x: Unit): Closeable = ???
        pub def f02(_x: Unit): Flushable = ???
        pub def f03(_x: Unit): Appendable = ???
        pub def f04(_x: Unit): AutoCloseable = ???
        pub def f05(_x: Unit): CharSequence = ???
        pub def f06(_x: Unit): Cloneable = ???
        pub def f07(_x: Unit): Readable = ???
        pub def f08(_x: Unit): Runnable = ???
        pub def f09(_x: Unit): Future = ???
        pub def f10(_x: Unit): Lock = ???
    }

    pub eff F {
        def f01(x: Closeable): Closeable
        def f02(x: Flushable): Flushable
        def f03(x: Appendable): Appendable
        def f04(x: AutoCloseable): AutoCloseable
        def f05(x: CharSequence): CharSequence
        def f06(x: Cloneable): Cloneable
        def f07(x: Readable): Readable
        def f08(x: Runnable): Runnable
        def f09(x: Future): Future
        def f10(x: Lock): Lock
    }

}

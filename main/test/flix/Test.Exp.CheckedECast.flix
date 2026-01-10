 mod Test.Exp.CheckedEffectCast {

    @CompileTest
    def testCheckedEffectCast01(): Unit \ IO = checked_ecast(())

    @CompileTest
    def testCheckedEffectCast02(): Bool \ IO = checked_ecast(true)

    @CompileTest
    def testCheckedEffectCast03(): Bool \ IO = checked_ecast(not false)

    @CompileTest
    def testCheckedEffectCast04(): Char \ IO = checked_ecast('a')

    @CompileTest
    def testCheckedEffectCast05(): Char \ IO = checked_ecast('b')

    @CompileTest
    def testCheckedEffectCast06(): Char \ IO = checked_ecast('Z')

    @CompileTest
    def testCheckedEffectCast07(): Char \ IO = checked_ecast('0')

    @CompileTest
    def testCheckedEffectCast08(): Float32 \ IO = checked_ecast(0.0f32)

    @CompileTest
    def testCheckedEffectCast09(): Float32 \ IO = checked_ecast(123.4f32)

    @CompileTest
    def testCheckedEffectCast10(): Float64 \ IO = checked_ecast(0.0f64)

    @CompileTest
    def testCheckedEffectCast11(): Float64 \ IO = checked_ecast(123.4f64)

    @CompileTest
    def testCheckedEffectCast12(): Int8 \ IO = checked_ecast(0i8)

    @CompileTest
    def testCheckedEffectCast13(): Int8 \ IO = checked_ecast(64i8)

    @CompileTest
    def testCheckedEffectCast14(): Int16 \ IO = checked_ecast(0i16)

    @CompileTest
    def testCheckedEffectCast15(): Int16 \ IO = checked_ecast(64i16)

    @CompileTest
    def testCheckedEffectCast16(): Int32 \ IO = checked_ecast(0i32)

    @CompileTest
    def testCheckedEffectCast17(): Int32 \ IO = checked_ecast(64i32)

    @CompileTest
    def testCheckedEffectCast18(): Int64 \ IO = checked_ecast(0i64)

    @CompileTest
    def testCheckedEffectCast19(): Int64 \ IO = checked_ecast(64i64)

    @CompileTest
    def testCheckedEffectCast20(): BigInt \ IO = checked_ecast(0ii)

    @CompileTest
    def testCheckedEffectCast21(): BigInt \ IO = checked_ecast(64ii)

    @CompileTest
    def testCheckedEffectCast22(): String \ IO = checked_ecast("ABC")

    @CompileTest
    def testCheckedEffectCast23(): Unit \ IO = region rc {
        discard checked_ecast(%%ARRAY_NEW%%(rc, 'A', 8));
        ()
    }

    @CompileTest
    def testCheckedEffectCast24(): Unit \ IO = region rc {
        discard checked_ecast(%%ARRAY_NEW%%(rc, Some(42), 8));
        ()
    }

    @CompileTest
    def testCheckedEffectCast25(): (Char, Int32, Bool) \ IO = checked_ecast(('a', 42, false))

    @CompileTest
    def testCheckedEffectCast26(): A[String] \ IO = checked_ecast(A.A("ABC"))

    @CompileTest
    def testCheckedEffectCast27(): (a -> a \ IO) = x -> checked_ecast(x)

    @CompileTest
    def testCheckedEffectCast28(): (a -> a) \ IO = checked_ecast(x -> x)

    enum A[a](a)

    pub eff E

    def _testCheckedEffectCast29(): (a -> a \ E) = x -> checked_ecast(x)

}

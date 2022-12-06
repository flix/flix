namespace Test/Exp/Cast {

    @test
    def testCast01(): Unit = unsafe_cast () as Unit

    @test
    def testCast02(): Bool = unsafe_cast true as Bool

    @test
    def testCast03(): Bool = unsafe_cast not false as Bool

    @test
    def testCast04(): Char = unsafe_cast 'a' as Char

    @test
    def testCast05(): Float32 = unsafe_cast 0.0f32 as Float32

    @test
    def testCast06(): Float64 = unsafe_cast 0.0f64 as Float64

    @test
    def testCast07(): Int8 = unsafe_cast 0i8 as Int8

    @test
    def testCast08(): Int16 = unsafe_cast 0i16 as Int16

    @test
    def testCast09(): Int32 = unsafe_cast 0i32 as Int32

    @test
    def testCast10(): Int64 = unsafe_cast 0i64 as Int64

    @test
    def testCast11(): BigInt = unsafe_cast 0ii as BigInt

    @test
    def testCast12(): BigDecimal = unsafe_cast 0.0ff as BigDecimal

    @test
    def testCast13(): String = unsafe_cast "hello world" as String

    @test
    def testCast14(): (Int32, Int32, Int32) = unsafe_cast (1, 2, 3) as (Int32, Int32, Int32)

    @test
    def testCast15(): Option[Int32] = unsafe_cast Some(42) as Option[Int32]

    @test
    def testCastWithEff01(): Unit \ IO = unsafe_cast () as Unit \ IO

    @test
    def testCastWithEff02(): Bool \ IO = unsafe_cast true as Bool \ IO

    @test
    def testCastWithEff03(): Bool \ IO = unsafe_cast not false as Bool \ IO

    @test
    def testCastWithEff04(): Char \ IO = unsafe_cast 'a' as Char \ IO

    @test
    def testCastWithEff05(): Float32 \ IO = unsafe_cast 0.0f32 as Float32 \ IO

    @test
    def testCastWithEff06(): Float64 \ IO = unsafe_cast 0.0f64 as Float64 \ IO

    @test
    def testCastWithEff07(): Int8 \ IO = unsafe_cast 0i8 as Int8 \ IO

    @test
    def testCastWithEff08(): Int16 \ IO = unsafe_cast 0i16 as Int16 \ IO

    @test
    def testCastWithEff09(): Int32 \ IO = unsafe_cast 0i32 as Int32 \ IO

    @test
    def testCastWithEff10(): Int64 \ IO = unsafe_cast 0i64 as Int64 \ IO

    @test
    def testCastWithEff11(): BigInt \ IO = unsafe_cast 0ii as BigInt \ IO

    @test
    def testCastWithEff12(): BigDecimal \ IO = unsafe_cast 0.0ff as BigDecimal \ IO

    @test
    def testCastWithEff13(): String \ IO = unsafe_cast "hello world" as String \ IO

    @test
    def testCastWithEff14(): (Int32, Int32, Int32) \ IO = unsafe_cast (1, 2, 3) as (Int32, Int32, Int32) \ IO

    @test
    def testCastWithEff15(): Option[Int32] \ IO = unsafe_cast Some(42) as Option[Int32] \ IO

    @test
    def testCastWithWildAndEff01(): Unit \ IO = unsafe_cast () as _ \ IO

    @test
    def testCastWithWildAndEff02(): Bool \ IO = unsafe_cast true as _ \ IO

    @test
    def testCastWithWildAndEff03(): Bool \ IO = unsafe_cast not false as _ \ IO

    @test
    def testCastWithWildAndEff04(): Char \ IO = unsafe_cast 'a' as _ \ IO

    @test
    def testCastWithWildAndEff05(): Float32 \ IO = unsafe_cast 0.0f32 as _ \ IO

    @test
    def testCastWithWildAndEff06(): Float64 \ IO = unsafe_cast 0.0f64 as _ \ IO

    @test
    def testCastWithWildAndEff07(): Int8 \ IO = unsafe_cast 0i8 as _ \ IO

    @test
    def testCastWithWildAndEff08(): Int16 \ IO = unsafe_cast 0i16 as _ \ IO

    @test
    def testCastWithWildAndEff09(): Int32 \ IO = unsafe_cast 0i32 as _ \ IO

    @test
    def testCastWithWildAndEff10(): Int64 \ IO = unsafe_cast 0i64 as _ \ IO

    @test
    def testCastWithWildAndEff11(): BigInt \ IO = unsafe_cast 0ii as _ \ IO

    @test
    def testCastWithWildAndEff12(): BigDecimal \ IO = unsafe_cast 0.0ff as _ \ IO

    @test
    def testCastWithWildAndEff13(): String \ IO = unsafe_cast "hello world" as _ \ IO

    @test
    def testCastWithWildAndEff14(): (Int32, Int32, Int32) \ IO = unsafe_cast (1, 2, 3) as _ \ IO

    @test
    def testCastWithWildAndEff15(): Option[Int32] \ IO = unsafe_cast Some(42) as _ \ IO

}

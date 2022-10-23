namespace Test/Exp/Cast {

    @test
    def testCast01(): Unit = () as Unit

    @test
    def testCast02(): Bool = true as Bool

    @test
    def testCast03(): Bool = not false as Bool

    @test
    def testCast04(): Char = 'a' as Char

    @test
    def testCast05(): Float32 = 0.0f32 as Float32

    @test
    def testCast06(): Float64 = 0.0f64 as Float64

    @test
    def testCast07(): Int8 = 0i8 as Int8

    @test
    def testCast08(): Int16 = 0i16 as Int16

    @test
    def testCast09(): Int32 = 0i32 as Int32

    @test
    def testCast10(): Int64 = 0i64 as Int64

    @test
    def testCast11(): BigInt = 0ii as BigInt

    @test
    def testCast12(): BigDecimal = 0.0ff as BigDecimal

    @test
    def testCast13(): String = "hello world" as String

    @test
    def testCast14(): (Int32, Int32, Int32) = (1, 2, 3) as (Int32, Int32, Int32)

    @test
    def testCast15(): Option[Int32] = Some(42) as Option[Int32]

    @test
    def testCastWithEff01(): Unit \ IO = () as Unit \ IO

    @test
    def testCastWithEff02(): Bool \ IO = true as Bool \ IO

    @test
    def testCastWithEff03(): Bool \ IO = not false as Bool \ IO

    @test
    def testCastWithEff04(): Char \ IO = 'a' as Char \ IO

    @test
    def testCastWithEff05(): Float32 \ IO = 0.0f32 as Float32 \ IO

    @test
    def testCastWithEff06(): Float64 \ IO = 0.0f64 as Float64 \ IO

    @test
    def testCastWithEff07(): Int8 \ IO = 0i8 as Int8 \ IO

    @test
    def testCastWithEff08(): Int16 \ IO = 0i16 as Int16 \ IO

    @test
    def testCastWithEff09(): Int32 \ IO = 0i32 as Int32 \ IO

    @test
    def testCastWithEff10(): Int64 \ IO = 0i64 as Int64 \ IO

    @test
    def testCastWithEff11(): BigInt \ IO = 0ii as BigInt \ IO

    @test
    def testCastWithEff12(): BigDecimal \ IO = 0.0ff as BigDecimal \ IO

    @test
    def testCastWithEff13(): String \ IO = "hello world" as String \ IO

    @test
    def testCastWithEff14(): (Int32, Int32, Int32) \ IO = (1, 2, 3) as (Int32, Int32, Int32) \ IO

    @test
    def testCastWithEff15(): Option[Int32] \ IO = Some(42) as Option[Int32] \ IO

    @test
    def testCastWithWildAndEff01(): Unit \ IO = () as \ IO

    @test
    def testCastWithWildAndEff02(): Bool \ IO = true as \ IO

    @test
    def testCastWithWildAndEff03(): Bool \ IO = not false as \ IO

    @test
    def testCastWithWildAndEff04(): Char \ IO = 'a' as \ IO

    @test
    def testCastWithWildAndEff05(): Float32 \ IO = 0.0f32 as \ IO

    @test
    def testCastWithWildAndEff06(): Float64 \ IO = 0.0f64 as \ IO

    @test
    def testCastWithWildAndEff07(): Int8 \ IO = 0i8 as \ IO

    @test
    def testCastWithWildAndEff08(): Int16 \ IO = 0i16 as \ IO

    @test
    def testCastWithWildAndEff09(): Int32 \ IO = 0i32 as \ IO

    @test
    def testCastWithWildAndEff10(): Int64 \ IO = 0i64 as \ IO

    @test
    def testCastWithWildAndEff11(): BigInt \ IO = 0ii as \ IO

    @test
    def testCastWithWildAndEff12(): BigDecimal \ IO = 0.0ff as \ IO

    @test
    def testCastWithWildAndEff13(): String \ IO = "hello world" as \ IO

    @test
    def testCastWithWildAndEff14(): (Int32, Int32, Int32) \ IO = (1, 2, 3) as \ IO

    @test
    def testCastWithWildAndEff15(): Option[Int32] \ IO = Some(42) as \ IO

}

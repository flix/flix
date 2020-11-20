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
    def testCast12(): String = "hello world" as String

    @test
    def testCast13(): (Int, Int, Int) = (1, 2, 3) as (Int, Int, Int)

    @test
    def testCast14(): Option[Int] = Some(42) as Option[Int]

    @test
    def testCastWithEff01(): Unit & Impure = () as Unit & Impure

    @test
    def testCastWithEff02(): Bool & Impure = true as Bool & Impure

    @test
    def testCastWithEff03(): Bool & Impure = not false as Bool & Impure

    @test
    def testCastWithEff04(): Char & Impure = 'a' as Char & Impure

    @test
    def testCastWithEff05(): Float32 & Impure = 0.0f32 as Float32 & Impure

    @test
    def testCastWithEff06(): Float64 & Impure = 0.0f64 as Float64 & Impure

    @test
    def testCastWithEff07(): Int8 & Impure = 0i8 as Int8 & Impure

    @test
    def testCastWithEff08(): Int16 & Impure = 0i16 as Int16 & Impure

    @test
    def testCastWithEff09(): Int32 & Impure = 0i32 as Int32 & Impure

    @test
    def testCastWithEff10(): Int64 & Impure = 0i64 as Int64 & Impure

    @test
    def testCastWithEff11(): BigInt & Impure = 0ii as BigInt & Impure

    @test
    def testCastWithEff12(): String & Impure = "hello world" as String & Impure

    @test
    def testCastWithEff13(): (Int, Int, Int) & Impure = (1, 2, 3) as (Int, Int, Int) & Impure

    @test
    def testCastWithEff14(): Option[Int] & Impure = Some(42) as Option[Int] & Impure

    @test
    def testCastWithWildAndEff01(): Unit & Impure = () as & Impure

    @test
    def testCastWithWildAndEff02(): Bool & Impure = true as & Impure

    @test
    def testCastWithWildAndEff03(): Bool & Impure = not false as & Impure

    @test
    def testCastWithWildAndEff04(): Char & Impure = 'a' as & Impure

    @test
    def testCastWithWildAndEff05(): Float32 & Impure = 0.0f32 as & Impure

    @test
    def testCastWithWildAndEff06(): Float64 & Impure = 0.0f64 as & Impure

    @test
    def testCastWithWildAndEff07(): Int8 & Impure = 0i8 as & Impure

    @test
    def testCastWithWildAndEff08(): Int16 & Impure = 0i16 as & Impure

    @test
    def testCastWithWildAndEff09(): Int32 & Impure = 0i32 as & Impure

    @test
    def testCastWithWildAndEff10(): Int64 & Impure = 0i64 as & Impure

    @test
    def testCastWithWildAndEff11(): BigInt & Impure = 0ii as & Impure

    @test
    def testCastWithWildAndEff12(): String & Impure = "hello world" as & Impure

    @test
    def testCastWithWildAndEff13(): (Int, Int, Int) & Impure = (1, 2, 3) as & Impure

    @test
    def testCastWithWildAndEff14(): Option[Int] & Impure = Some(42) as & Impure

}

namespace Test/Exp/Default {

    @test
    def testDefaultUnit(): Bool = (default: Unit) == ()

    @test
    def testDefaultBool(): Bool = (default: Bool) == false

    @test
    def testDefaultChar(): Bool = (default: Char) == '0'

    @test
    def testDefaultFloat32(): Bool = (default: Float32) == 0.0f32

    @test
    def testDefaultFloat64(): Bool = (default: Float64) == 0.0f64

    @test
    def testDefaultInt8(): Bool = (default: Int8) == 0i8

    @test
    def testDefaultInt16(): Bool = (default: Int16) == 0i16

    @test
    def testDefaultInt32(): Bool = (default: Int32) == 0i32

    @test
    def testDefaultInt64(): Bool = (default: Int64) == 0i64

    @test
    def testDefaultBigInt(): Bool = (default: BigInt) == 0ii

}

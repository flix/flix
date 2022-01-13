namespace Test/Exp/Default {

    @test
    def testDefaultUnit(): Bool = ($DEFAULT$: Unit) == ()

    @test
    def testDefaultBool(): Bool = ($DEFAULT$: Bool) == false

    @test
    def testDefaultChar(): Bool = ($DEFAULT$: Char) == '0'

    @test
    def testDefaultFloat32(): Bool = ($DEFAULT$: Float32) == 0.0f32

    @test
    def testDefaultFloat64(): Bool = ($DEFAULT$: Float64) == 0.0f64

    @test
    def testDefaultInt8(): Bool = ($DEFAULT$: Int8) == 0i8

    @test
    def testDefaultInt16(): Bool = ($DEFAULT$: Int16) == 0i16

    @test
    def testDefaultInt32(): Bool = ($DEFAULT$: Int32) == 0i32

    @test
    def testDefaultInt64(): Bool = ($DEFAULT$: Int64) == 0i64

    @test
    def testDefaultBigInt(): Bool = ($DEFAULT$: BigInt) == 0ii

}

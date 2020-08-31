namespace Test/Exp/Default {

    @test
    def testDefaultUnit(): Bool = (default: Unit) == ()

    @test
    def testDefaultBool(): Bool = (default: Bool) == false

    @test
    def testDefaultFloat32(): Bool = (default: Float32) == 0.0f32

    @test
    def testDefaultFloat64(): Bool = (default: Float64) == 0.0f64

    @test
    def testDefaultInt8(): Bool = (default: Int8) == 0i8


}

namespace Test/Exp/$DEFAULT$ {

    @test
    def test$DEFAULT$Unit(): Bool = ($DEFAULT$: Unit) == ()

    @test
    def test$DEFAULT$Bool(): Bool = ($DEFAULT$: Bool) == false

    @test
    def test$DEFAULT$Char(): Bool = ($DEFAULT$: Char) == '0'

    @test
    def test$DEFAULT$Float32(): Bool = ($DEFAULT$: Float32) == 0.0f32

    @test
    def test$DEFAULT$Float64(): Bool = ($DEFAULT$: Float64) == 0.0f64

    @test
    def test$DEFAULT$Int8(): Bool = ($DEFAULT$: Int8) == 0i8

    @test
    def test$DEFAULT$Int16(): Bool = ($DEFAULT$: Int16) == 0i16

    @test
    def test$DEFAULT$Int32(): Bool = ($DEFAULT$: Int32) == 0i32

    @test
    def test$DEFAULT$Int64(): Bool = ($DEFAULT$: Int64) == 0i64

    @test
    def test$DEFAULT$BigInt(): Bool = ($DEFAULT$: BigInt) == 0ii

}

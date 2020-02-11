namespace Test/Exp/Spawn {

    @test
    def testSpawn01(): Unit @ Impure = spawn ()

    @test
    def testSpawn02(): Unit @ Impure = spawn true

    @test
    def testSpawn03(): Unit @ Impure = spawn 'a'

    @test
    def testSpawn04(): Unit @ Impure = spawn 1.0f32 + 2.0f32

    @test
    def testSpawn05(): Unit @ Impure = spawn 1.0f64 + 2.0f64

    @test
    def testSpawn06(): Unit @ Impure = spawn 1i8 + 2i8

    @test
    def testSpawn07(): Unit @ Impure = spawn 1i16 + 2i16

    @test
    def testSpawn08(): Unit @ Impure = spawn 1i32 + 2i32

    @test
    def testSpawn09(): Unit @ Impure = spawn 1i64 + 2i64


}

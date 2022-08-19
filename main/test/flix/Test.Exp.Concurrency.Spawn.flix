namespace Test/Exp/Concurrency/Spawn {

    @test
    def testSpawn01(): Unit \ IO = spawn ()

    @test
    def testSpawn02(): Unit \ IO = spawn true

    @test
    def testSpawn03(): Unit \ IO = spawn 'a'

    @test
    def testSpawn04(): Unit \ IO = spawn 1.0f32 + 2.0f32

    @test
    def testSpawn05(): Unit \ IO = spawn 1.0f64 + 2.0f64

    @test
    def testSpawn06(): Unit \ IO = spawn 1i8 + 2i8

    @test
    def testSpawn07(): Unit \ IO = spawn 1i16 + 2i16

    @test
    def testSpawn08(): Unit \ IO = spawn 1i32 + 2i32

    @test
    def testSpawn09(): Unit \ IO = spawn 1i64 + 2i64

    @test
    def testSpawn10(): Unit \ IO = spawn 1ii + 2ii

    @test
    def testSpawn11(): Unit \ IO = spawn "Hello World!"

    @test
    def testSpawn12(): Unit \ IO = spawn (123, 456)

    @test
    def testSpawn13(): Unit \ IO = spawn None

    @test
    def testSpawn14(): Unit \ IO = spawn Some(42)

    @test
    def testSpawn15(): Unit \ IO = spawn Ok(42)

    @test
    def testSpawn16(): Unit \ IO = spawn Err(42)

    @test
    def testSpawn17(): Unit \ IO = spawn spawn 123

    @test
    def testSpawn18(): Unit \ IO = spawn spawn spawn 123

}

namespace Test/Exp/Concurrency/Spawn {

    @test
    def testSpawn01(): Unit & Impure = spawn ()

    @test
    def testSpawn02(): Unit & Impure = spawn true

    @test
    def testSpawn03(): Unit & Impure = spawn 'a'

    @test
    def testSpawn04(): Unit & Impure = spawn 1.0f32 + 2.0f32

    @test
    def testSpawn05(): Unit & Impure = spawn 1.0f64 + 2.0f64

    @test
    def testSpawn06(): Unit & Impure = spawn 1i8 + 2i8

    @test
    def testSpawn07(): Unit & Impure = spawn 1i16 + 2i16

    @test
    def testSpawn08(): Unit & Impure = spawn 1i32 + 2i32

    @test
    def testSpawn09(): Unit & Impure = spawn 1i64 + 2i64

    @test
    def testSpawn10(): Unit & Impure = spawn 1ii + 2ii

    @test
    def testSpawn11(): Unit & Impure = spawn "Hello World!"

    @test
    def testSpawn12(): Unit & Impure = spawn (123, 456)

    @test
    def testSpawn13(): Unit & Impure = spawn None

    @test
    def testSpawn14(): Unit & Impure = spawn Some(42)

    @test
    def testSpawn15(): Unit & Impure = spawn Ok(42)

    @test
    def testSpawn16(): Unit & Impure = spawn Err(42)

    @test
    def testSpawn17(): Unit & Impure = spawn spawn 123

    @test
    def testSpawn18(): Unit & Impure = spawn spawn spawn 123

}

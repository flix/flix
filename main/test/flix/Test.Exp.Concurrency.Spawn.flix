namespace Test/Exp/Concurrency/Spawn {

    @test
    def testSpawn01(): Unit \ IO = spawn () @ Static

    @test
    def testSpawn02(): Unit \ IO = spawn true @ Static

    @test
    def testSpawn03(): Unit \ IO = spawn 'a' @ Static

    @test
    def testSpawn04(): Unit \ IO = spawn 1.0f32 + 2.0f32 @ Static

    @test
    def testSpawn05(): Unit \ IO = spawn 1.0f64 + 2.0f64 @ Static

    @test
    def testSpawn06(): Unit \ IO = spawn 1i8 + 2i8 @ Static

    @test
    def testSpawn07(): Unit \ IO = spawn 1i16 + 2i16 @ Static

    @test
    def testSpawn08(): Unit \ IO = spawn 1i32 + 2i32 @ Static

    @test
    def testSpawn09(): Unit \ IO = spawn 1i64 + 2i64 @ Static

    @test
    def testSpawn10(): Unit \ IO = spawn 1ii + 2ii @ Static

    @test
    def testSpawn11(): Unit \ IO = spawn "Hello World!" @ Static

    @test
    def testSpawn12(): Unit \ IO = spawn (123, 456) @ Static

    @test
    def testSpawn13(): Unit \ IO = spawn None @ Static

    @test
    def testSpawn14(): Unit \ IO = spawn Some(42) @ Static

    @test
    def testSpawn15(): Unit \ IO = spawn Ok(42) @ Static

    @test
    def testSpawn16(): Unit \ IO = spawn Err(42) @ Static

    @test
    def testSpawn17(): Unit \ IO = spawn spawn 123 @ Static @ Static

    @test
    def testSpawn18(): Unit \ IO = spawn spawn spawn 123 @ Static @ Static @ Static

//    @test
//    def testSpawnAndRegionOf01(): Unit \ IO = region r {
//        let a = [1, 2, 3] @ r;
//        spawn () @ Scoped.regionOf(a)
//    }
}

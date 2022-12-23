namespace Test/Exp/Concurrency/Spawn {

    @test
    def testSpawn01(): Unit \ IO = region r { spawn () @ r }

    @test
    def testSpawn02(): Unit \ IO = region r { spawn true @ r }

    @test
    def testSpawn03(): Unit \ IO = region r { spawn 'a' @ r }

    @test
    def testSpawn04(): Unit \ IO = region r { spawn 1.0f32 + 2.0f32 @ r }

    @test
    def testSpawn05(): Unit \ IO = region r { spawn 1.0f64 + 2.0f64 @ r }

    @test
    def testSpawn06(): Unit \ IO = region r { spawn 1i8 + 2i8 @ r }

    @test
    def testSpawn07(): Unit \ IO = region r { spawn 1i16 + 2i16 @ r }

    @test
    def testSpawn08(): Unit \ IO = region r { spawn 1i32 + 2i32 @ r }

    @test
    def testSpawn09(): Unit \ IO = region r { spawn 1i64 + 2i64 @ r }

    @test
    def testSpawn10(): Unit \ IO = region r { spawn 1ii + 2ii @ r }

    @test
    def testSpawn11(): Unit \ IO = region r { spawn "Hello World!" @ r }

    @test
    def testSpawn12(): Unit \ IO = region r { spawn (123, 456) @ r }

    @test
    def testSpawn13(): Unit \ IO = region r { spawn None @ r }

    @test
    def testSpawn14(): Unit \ IO = region r { spawn Some(42) @ r }

    @test
    def testSpawn15(): Unit \ IO = region r { spawn Ok(42) @ r }

    @test
    def testSpawn16(): Unit \ IO = region r { spawn Err(42) @ r }

    @test
    def testSpawn17(): Unit \ IO = region r { spawn spawn 123 @ r @ r }

    @test
    def testSpawn18(): Unit \ IO = region r { spawn spawn spawn 123 @ r @ r @ r }

//    @test
//    def testSpawnAndRegionOf01(): Unit \ IO = region r {
//        let a = [1, 2, 3] @ r;
//        spawn () @ Scoped.regionOf(a)
//    }
}

mod Test.Exp.Concurrency.Spawn {

    import java.lang.NullPointerException

    @test
    def testSpawn01(): Unit = region rc { spawn () @ rc }

    @test
    def testSpawn02(): Unit = region rc { spawn true @ rc }

    @test
    def testSpawn03(): Unit = region rc { spawn 'a' @ rc }

    @test
    def testSpawn04(): Unit = region rc { spawn 1.0f32 + 2.0f32 @ rc }

    @test
    def testSpawn05(): Unit = region rc { spawn 1.0f64 + 2.0f64 @ rc }

    @test
    def testSpawn06(): Unit = region rc { spawn 1i8 + 2i8 @ rc }

    @test
    def testSpawn07(): Unit = region rc { spawn 1i16 + 2i16 @ rc }

    @test
    def testSpawn08(): Unit = region rc { spawn 1i32 + 2i32 @ rc }

    @test
    def testSpawn09(): Unit = region rc { spawn 1i64 + 2i64 @ rc }

    @test
    def testSpawn10(): Unit = region rc { spawn 1ii + 2ii @ rc }

    @test
    def testSpawn11(): Unit = region rc { spawn "Hello World!" @ rc }

    @test
    def testSpawn12(): Unit = region rc { spawn (123, 456) @ rc }

    @test
    def testSpawn13(): Unit = region rc { spawn None @ rc }

    @test
    def testSpawn14(): Unit = region rc { spawn Some(42) @ rc }

    @test
    def testSpawn15(): Unit = region rc { spawn Ok(42) @ rc }

    @test
    def testSpawn16(): Unit = region rc { spawn Err(42) @ rc }

}

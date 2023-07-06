mod Test.Exp.Concurrency.Spawn {

    @test
    def testSpawn01(): Unit \ IO = region rc { spawn () @ rc }

    @test
    def testSpawn02(): Unit \ IO = region rc { spawn true @ rc }

    @test
    def testSpawn03(): Unit \ IO = region rc { spawn 'a' @ rc }

    @test
    def testSpawn04(): Unit \ IO = region rc { spawn 1.0f32 + 2.0f32 @ rc }

    @test
    def testSpawn05(): Unit \ IO = region rc { spawn 1.0f64 + 2.0f64 @ rc }

    @test
    def testSpawn06(): Unit \ IO = region rc { spawn 1i8 + 2i8 @ rc }

    @test
    def testSpawn07(): Unit \ IO = region rc { spawn 1i16 + 2i16 @ rc }

    @test
    def testSpawn08(): Unit \ IO = region rc { spawn 1i32 + 2i32 @ rc }

    @test
    def testSpawn09(): Unit \ IO = region rc { spawn 1i64 + 2i64 @ rc }

    @test
    def testSpawn10(): Unit \ IO = region rc { spawn 1ii + 2ii @ rc }

    @test
    def testSpawn11(): Unit \ IO = region rc { spawn "Hello World!" @ rc }

    @test
    def testSpawn12(): Unit \ IO = region rc { spawn (123, 456) @ rc }

    @test
    def testSpawn13(): Unit \ IO = region rc { spawn None @ rc }

    @test
    def testSpawn14(): Unit \ IO = region rc { spawn Some(42) @ rc }

    @test
    def testSpawn15(): Unit \ IO = region rc { spawn Ok(42) @ rc }

    @test
    def testSpawn16(): Unit \ IO = region rc { spawn Err(42) @ rc }

    @test
    def testSpawn17(): Unit \ IO = region rc { spawn spawn 123 @ rc @ rc }

    @test
    def testSpawn18(): Unit \ IO = region rc { spawn spawn spawn 123 @ rc @ rc @ rc }

    @test
    def testExceptionsInChildThread01(): Bool \ IO =
        try {
            region rc {
                spawn {
                    spawn { String.concat(checked_cast(null), "foo") } @ rc
                } @ rc;
                Thread.sleep(Time.Duration.fromSeconds(1));
                false
            }
        } catch {
            case _: ##java.lang.NullPointerException => true
        }


    @test
    def testExceptionsInChildThread02(): Bool \ IO = region rc {
        spawn {
            spawn {
                try {
                    String.concat(checked_cast(null), "foo")
                } catch {
                    case _: ##java.lang.NullPointerException => ""
                }
            } @ rc
        } @ rc;
        Thread.sleep(Time.Duration.fromSeconds(1));
        true
    }
}

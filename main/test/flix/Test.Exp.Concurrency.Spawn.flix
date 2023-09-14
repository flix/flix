mod Test.Exp.Concurrency.Spawn {

    @Test
    def testSpawn01(): Unit \ IO = region rc { spawn () @ rc }

    @Test
    def testSpawn02(): Unit \ IO = region rc { spawn true @ rc }

    @Test
    def testSpawn03(): Unit \ IO = region rc { spawn 'a' @ rc }

    @Test
    def testSpawn04(): Unit \ IO = region rc { spawn 1.0f32 + 2.0f32 @ rc }

    @Test
    def testSpawn05(): Unit \ IO = region rc { spawn 1.0f64 + 2.0f64 @ rc }

    @Test
    def testSpawn06(): Unit \ IO = region rc { spawn 1i8 + 2i8 @ rc }

    @Test
    def testSpawn07(): Unit \ IO = region rc { spawn 1i16 + 2i16 @ rc }

    @Test
    def testSpawn08(): Unit \ IO = region rc { spawn 1i32 + 2i32 @ rc }

    @Test
    def testSpawn09(): Unit \ IO = region rc { spawn 1i64 + 2i64 @ rc }

    @Test
    def testSpawn10(): Unit \ IO = region rc { spawn 1ii + 2ii @ rc }

    @Test
    def testSpawn11(): Unit \ IO = region rc { spawn "Hello World!" @ rc }

    @Test
    def testSpawn12(): Unit \ IO = region rc { spawn (123, 456) @ rc }

    @Test
    def testSpawn13(): Unit \ IO = region rc { spawn None @ rc }

    @Test
    def testSpawn14(): Unit \ IO = region rc { spawn Some(42) @ rc }

    @Test
    def testSpawn15(): Unit \ IO = region rc { spawn Ok(42) @ rc }

    @Test
    def testSpawn16(): Unit \ IO = region rc { spawn Err(42) @ rc }

    @Test
    def testSpawn17(): Unit \ IO = region rc { spawn spawn 123 @ rc @ rc }

    @Test
    def testSpawn18(): Unit \ IO = region rc { spawn spawn spawn 123 @ rc @ rc @ rc }

    @Test
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


    @Test
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

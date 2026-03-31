mod Test.Exp.Concurrency.Spawn {

    import java.lang.NullPointerException
    import java.lang.{Thread => JThread}

    use Assert.assertTrue

    @Test
    def testSpawn01(): Unit = region rc { spawn () @ rc }

    @Test
    def testSpawn02(): Unit = region rc { spawn true @ rc }

    @Test
    def testSpawn03(): Unit = region rc { spawn 'a' @ rc }

    @Test
    def testSpawn04(): Unit = region rc { spawn 1.0f32 + 2.0f32 @ rc }

    @Test
    def testSpawn05(): Unit = region rc { spawn 1.0f64 + 2.0f64 @ rc }

    @Test
    def testSpawn06(): Unit = region rc { spawn 1i8 + 2i8 @ rc }

    @Test
    def testSpawn07(): Unit = region rc { spawn 1i16 + 2i16 @ rc }

    @Test
    def testSpawn08(): Unit = region rc { spawn 1i32 + 2i32 @ rc }

    @Test
    def testSpawn09(): Unit = region rc { spawn 1i64 + 2i64 @ rc }

    @Test
    def testSpawn10(): Unit = region rc { spawn 1ii + 2ii @ rc }

    @Test
    def testSpawn11(): Unit = region rc { spawn "Hello World!" @ rc }

    @Test
    def testSpawn12(): Unit = region rc { spawn (123, 456) @ rc }

    @Test
    def testSpawn13(): Unit = region rc { spawn None @ rc }

    @Test
    def testSpawn14(): Unit = region rc { spawn Some(42) @ rc }

    @Test
    def testSpawn15(): Unit = region rc { spawn Ok(42) @ rc }

    @Test
    def testSpawn16(): Unit = region rc { spawn Err(42) @ rc }

    @Test
    def testSpawn17(): Unit = region rc { spawn spawn 123 @ rc @ rc }

    @Test
    def testSpawn18(): Unit = region rc { spawn spawn spawn 123 @ rc @ rc @ rc }

    @Test
    def testExceptionsInChildThread01(): Unit \ (Assert + IO) =
        let r = try {
            region rc {
                spawn {
                    spawn { println(String.concat(checked_cast(null), "foo")) } @ rc
                } @ rc;
                JThread.sleep(1_000i64);
                false
            }
        } catch {
            case _: NullPointerException => true
        };
        assertTrue(r)


    @Test
    def testExceptionsInChildThread02(): Unit \ (Assert + IO) = region rc {
        spawn {
            spawn {
                try {
                    println(String.concat(checked_cast(null), "foo"))
                } catch {
                    case _: NullPointerException => ()
                }
            } @ rc
        } @ rc;
        JThread.sleep(1_000i64);
        assertTrue(true)
    }

}

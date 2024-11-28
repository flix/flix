mod Test.Exp.Concurrency.Spawn {

    import java.lang.NullPointerException

    @test
    def testSpawn01(): Unit = spawn ()

    @test
    def testSpawn02(): Unit = spawn true

    @test
    def testSpawn03(): Unit = spawn 'a'

    @test
    def testSpawn04(): Unit = spawn 1.0f32 + 2.0f32

    @test
    def testSpawn05(): Unit = spawn 1.0f64 + 2.0f64

    @test
    def testSpawn06(): Unit = spawn 1i8 + 2i8

    @test
    def testSpawn07(): Unit = spawn 1i16 + 2i16

    @test
    def testSpawn08(): Unit = spawn 1i32 + 2i32

    @test
    def testSpawn09(): Unit = spawn 1i64 + 2i64

    @test
    def testSpawn10(): Unit = spawn 1ii + 2ii

    @test
    def testSpawn11(): Unit = spawn "Hello World!"

    @test
    def testSpawn12(): Unit = spawn (123, 456)

    @test
    def testSpawn13(): Unit = spawn None

    @test
    def testSpawn14(): Unit = spawn Some(42)

    @test
    def testSpawn15(): Unit = spawn Ok(42)

    @test
    def testSpawn16(): Unit = spawn Err(42)

    @test
    def testSpawn17(): Unit = spawn spawn 123

    @test
    def testSpawn18(): Unit = spawn spawn spawn 123

//    @test
//    def testExceptionsInChildThread01(): Bool \ IO =
//        try {
//            region rc {
//                spawn {
//                    spawn { String.concat(checked_cast(null), "foo") }
//                };
//                Thread.sleep(Time.Duration.fromSeconds(1));
//                false
//            }
//        } catch {
//            case _: NullPointerException => true
//        }


//    @test
//    def testExceptionsInChildThread02(): Bool \ IO = region rc {
//        spawn {
//            spawn {
//                try {
//                    String.concat(checked_cast(null), "foo")
//                } catch {
//                    case _: NullPointerException => ""
//                }
//            }
//        };
//        Thread.sleep(Time.Duration.fromSeconds(1));
//        true
//    }
}

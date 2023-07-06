mod Test.Exp.Ref.Ref {

    @test
    def testRef01(): Unit = region rc { discard ref () @ rc }

    @test
    def testRef02(): Unit = region rc { discard ref true @ rc }

    @test
    def testRef03(): Unit = region rc { discard ref false @ rc }

    @test
    def testRef04(): Unit = region rc { discard ref 'a' @ rc }

    @test
    def testRef05(): Unit = region rc { discard ref 0.0f32 @ rc }

    @test
    def testRef06(): Unit = region rc { discard ref 0.0f64 @ rc }

    @test
    def testRef07(): Unit = region rc { discard ref 0i8 @ rc }

    @test
    def testRef08(): Unit = region rc { discard ref 0i16 @ rc }

    @test
    def testRef09(): Unit = region rc { discard ref 0i32 @ rc }

    @test
    def testRef10(): Unit = region rc { discard ref 0i64 @ rc }

    @test
    def testRef11(): Unit = region rc { discard ref 0ii @ rc }

    @test
    def testRef12(): Unit = region rc { discard ref 0.0ff @ rc }

    @test
    def testRef13(): Unit = region rc { discard ref "Hello World!" @ rc }

    @test
    def testRef14(): Unit = region rc { discard ref (1, 2, 3) @ rc }

    @test
    def testRef15(): Unit = region rc { discard ref None : Option[Int32] @ rc }

    @test
    def testRef16(): Unit = region rc { discard ref Some(42) @ rc }

    @test
    def testRef17(): Unit = region rc { discard ref Ok(42): Result[String, Int32] @ rc }

    @test
    def testRef18(): Unit = region rc { discard ref Err("Goodbye World!"): Result[String, Int32] @ rc }

    @test
    def testRefRegion01(): Unit = // Pure
        region rc {
            let _u = ref () @ rc;
            ()
        }

    @test
    def testRefRegion02(): Unit = // Pure
        region rc {
            let _u = ref true @ rc;
            ()
        }

    @test
    def testRefRegion03(): Unit = // Pure
        region rc {
            let _u = ref 'a' @ rc;
            ()
        }

}

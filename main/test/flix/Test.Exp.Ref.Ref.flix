mod Test.Exp.Ref.Ref {

    @Test
    def testRef01(): Unit = region rc { discard ref () @ rc }

    @Test
    def testRef02(): Unit = region rc { discard ref true @ rc }

    @Test
    def testRef03(): Unit = region rc { discard ref false @ rc }

    @Test
    def testRef04(): Unit = region rc { discard ref 'a' @ rc }

    @Test
    def testRef05(): Unit = region rc { discard ref 0.0f32 @ rc }

    @Test
    def testRef06(): Unit = region rc { discard ref 0.0f64 @ rc }

    @Test
    def testRef07(): Unit = region rc { discard ref 0i8 @ rc }

    @Test
    def testRef08(): Unit = region rc { discard ref 0i16 @ rc }

    @Test
    def testRef09(): Unit = region rc { discard ref 0i32 @ rc }

    @Test
    def testRef10(): Unit = region rc { discard ref 0i64 @ rc }

    @Test
    def testRef11(): Unit = region rc { discard ref 0ii @ rc }

    @Test
    def testRef12(): Unit = region rc { discard ref 0.0ff @ rc }

    @Test
    def testRef13(): Unit = region rc { discard ref "Hello World!" @ rc }

    @Test
    def testRef14(): Unit = region rc { discard ref (1, 2, 3) @ rc }

    @Test
    def testRef15(): Unit = region rc { discard ref None : Option[Int32] @ rc }

    @Test
    def testRef16(): Unit = region rc { discard ref Some(42) @ rc }

    @Test
    def testRef17(): Unit = region rc { discard ref Ok(42): Result[String, Int32] @ rc }

    @Test
    def testRef18(): Unit = region rc { discard ref Err("Goodbye World!"): Result[String, Int32] @ rc }

    @Test
    def testRefRegion01(): Unit = // Pure
        region rc {
            let _u = ref () @ rc;
            ()
        }

    @Test
    def testRefRegion02(): Unit = // Pure
        region rc {
            let _u = ref true @ rc;
            ()
        }

    @Test
    def testRefRegion03(): Unit = // Pure
        region rc {
            let _u = ref 'a' @ rc;
            ()
        }

}

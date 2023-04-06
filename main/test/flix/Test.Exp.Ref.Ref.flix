mod Test.Exp.Ref.Ref {

    @test
    def testRef01(): Unit = region r { discard ref () @ r }

    @test
    def testRef02(): Unit = region r { discard ref true @ r }

    @test
    def testRef03(): Unit = region r { discard ref false @ r }

    @test
    def testRef04(): Unit = region r { discard ref 'a' @ r }

    @test
    def testRef05(): Unit = region r { discard ref 0.0f32 @ r }

    @test
    def testRef06(): Unit = region r { discard ref 0.0f64 @ r }

    @test
    def testRef07(): Unit = region r { discard ref 0i8 @ r }

    @test
    def testRef08(): Unit = region r { discard ref 0i16 @ r }

    @test
    def testRef09(): Unit = region r { discard ref 0i32 @ r }

    @test
    def testRef10(): Unit = region r { discard ref 0i64 @ r }

    @test
    def testRef11(): Unit = region r { discard ref 0ii @ r }

    @test
    def testRef12(): Unit = region r { discard ref 0.0ff @ r }

    @test
    def testRef13(): Unit = region r { discard ref "Hello World!" @ r }

    @test
    def testRef14(): Unit = region r { discard ref (1, 2, 3) @ r }

    @test
    def testRef15(): Unit = region r { discard ref None : Option[Int32] @ r }

    @test
    def testRef16(): Unit = region r { discard ref Some(42) @ r }

    @test
    def testRef17(): Unit = region r { discard ref Ok(42): Result[String, Int32] @ r }

    @test
    def testRef18(): Unit = region r { discard ref Err("Goodbye World!"): Result[String, Int32] @ r }

    @test
    def testRefRegion01(): Unit = // Pure
        region r {
            let _u = ref () @ r;
            ()
        }

    @test
    def testRefRegion02(): Unit = // Pure
        region r {
            let _u = ref true @ r;
            ()
        }

    @test
    def testRefRegion03(): Unit = // Pure
        region r {
            let _u = ref 'a' @ r;
            ()
        }

}

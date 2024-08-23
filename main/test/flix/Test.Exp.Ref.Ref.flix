mod Test.Exp.Ref.Ref {

    @test
    def testRef01(): Unit = region rc { discard Ref.fresh(rc, ()) }

    @test
    def testRef02(): Unit = region rc { discard Ref.fresh(rc, true) }

    @test
    def testRef03(): Unit = region rc { discard Ref.fresh(rc, false) }

    @test
    def testRef04(): Unit = region rc { discard Ref.fresh(rc, 'a') }

    @test
    def testRef05(): Unit = region rc { discard Ref.fresh(rc, 0.0f32) }

    @test
    def testRef06(): Unit = region rc { discard Ref.fresh(rc, 0.0f64) }

    @test
    def testRef07(): Unit = region rc { discard Ref.fresh(rc, 0i8) }

    @test
    def testRef08(): Unit = region rc { discard Ref.fresh(rc, 0i16) }

    @test
    def testRef09(): Unit = region rc { discard Ref.fresh(rc, 0i32) }

    @test
    def testRef10(): Unit = region rc { discard Ref.fresh(rc, 0i64) }

    @test
    def testRef11(): Unit = region rc { discard Ref.fresh(rc, 0ii) }

    @test
    def testRef12(): Unit = region rc { discard Ref.fresh(rc, 0.0ff) }

    @test
    def testRef13(): Unit = region rc { discard Ref.fresh(rc, "Hello World!") }

    @test
    def testRef14(): Unit = region rc { discard Ref.fresh(rc, (1, 2, 3)) }

    @test
    def testRef15(): Unit = region rc { discard Ref.fresh(rc, (None : Option[Int32])) }

    @test
    def testRef16(): Unit = region rc { discard Ref.fresh(rc, Some(42)) }

    @test
    def testRef17(): Unit = region rc { discard Ref.fresh(rc, (Ok(42): Result[String, Int32])) }

    @test
    def testRef18(): Unit = region rc { discard Ref.fresh(rc, (Err("Goodbye World!"): Result[String, Int32])) }

    @test
    def testRefRegion01(): Unit = // Pure
        region rc {
            let _u = Ref.fresh(rc, ());
            ()
        }

    @test
    def testRefRegion02(): Unit = // Pure
        region rc {
            let _u = Ref.fresh(rc, true);
            ()
        }

    @test
    def testRefRegion03(): Unit = // Pure
        region rc {
            let _u = Ref.fresh(rc, 'a');
            ()
        }

}

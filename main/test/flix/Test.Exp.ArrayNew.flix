mod Test.Exp.ArrayNew {

    @test
    def testArrayNew01(): Unit = region r {
        discard $ARRAY_NEW$(r, 10, 0);
        ()
    }

    @test
    def testArrayNew02(): Unit = region r {
        discard $ARRAY_NEW$(r, 1, 1);
        ()
    }

    @test
    def testArrayNew03(): Unit = region r {
        discard $ARRAY_NEW$(r, 0, 2);
        ()
    }

    @test
    def testArrayNew04(): Unit = region r {
        discard $ARRAY_NEW$(r, 0, 113);
        ()
    }

    @test
    def testArrayNew05(): Unit = region r {
        discard $ARRAY_NEW$(r, 0, 500);
        ()
    }

    @test
    def testArrayNew06(): Unit = region r {
        discard $ARRAY_NEW$(r, (), 5);
        ()
    }

    @test
    def testArrayNew07(): Unit = region r {
        discard $ARRAY_NEW$(r, true, 5);
        ()
    }

    @test
    def testArrayNew08(): Unit = region r {
        discard $ARRAY_NEW$(r, false, 5);
        ()
    }

    @test
    def testArrayNew09(): Unit = region r {
        discard $ARRAY_NEW$(r, 'a', 5);
        ()
    }

    @test
    def testArrayNew10(): Unit = region r {
        discard $ARRAY_NEW$(r, 10i8, 5);
        ()
    }

    @test
    def testArrayNew11(): Unit = region r {
        discard $ARRAY_NEW$(r, 10i16, 5);
        ()
    }

    @test
    def testArrayNew12(): Unit = region r {
        discard $ARRAY_NEW$(r, 10, 5);
        ()
    }

    @test
    def testArrayNew13(): Unit = region r {
        discard $ARRAY_NEW$(r, 10i64, 5);
        ()
    }

    @test
    def testArrayNew14(): Unit = region r {
        discard $ARRAY_NEW$(r, 10ii, 5);
        ()
    }

    @test
    def testArrayNew15(): Unit = region r {
        discard $ARRAY_NEW$(r, 3.0f32, 5);
        ()
    }

    @test
    def testArrayNew16(): Unit = region r {
        discard $ARRAY_NEW$(r, 3.0f64, 5);
        ()
    }

    @test
    def testArrayNew17(): Unit = region r {
        discard $ARRAY_NEW$(r, "foo", 5);
        ()
    }

    @test
    def testArrayNew18(): Unit = region r {
        discard $ARRAY_NEW$(r, $ARRAY_NEW$(r, 10, 10), 5);
        ()
    }

    @test
    def testArrayNew19(): Unit = region r {
        discard $ARRAY_NEW$(r, (1, 2), 5);
        ()
    }

    @test
    def testArrayNew20(): Unit = region r {
        discard $ARRAY_NEW$(r, 1 :: Nil, 5);
        ()
    }

    @test
    def testArrayNew21(): Unit = region r {
        discard $ARRAY_NEW$(r, None, 1);
        ()
    }
}

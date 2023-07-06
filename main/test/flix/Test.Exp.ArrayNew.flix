mod Test.Exp.ArrayNew {

    @test
    def testArrayNew01(): Unit = region rc {
        discard $ARRAY_NEW$(rc, 10, 0);
        ()
    }

    @test
    def testArrayNew02(): Unit = region rc {
        discard $ARRAY_NEW$(rc, 1, 1);
        ()
    }

    @test
    def testArrayNew03(): Unit = region rc {
        discard $ARRAY_NEW$(rc, 0, 2);
        ()
    }

    @test
    def testArrayNew04(): Unit = region rc {
        discard $ARRAY_NEW$(rc, 0, 113);
        ()
    }

    @test
    def testArrayNew05(): Unit = region rc {
        discard $ARRAY_NEW$(rc, 0, 500);
        ()
    }

    @test
    def testArrayNew06(): Unit = region rc {
        discard $ARRAY_NEW$(rc, (), 5);
        ()
    }

    @test
    def testArrayNew07(): Unit = region rc {
        discard $ARRAY_NEW$(rc, true, 5);
        ()
    }

    @test
    def testArrayNew08(): Unit = region rc {
        discard $ARRAY_NEW$(rc, false, 5);
        ()
    }

    @test
    def testArrayNew09(): Unit = region rc {
        discard $ARRAY_NEW$(rc, 'a', 5);
        ()
    }

    @test
    def testArrayNew10(): Unit = region rc {
        discard $ARRAY_NEW$(rc, 10i8, 5);
        ()
    }

    @test
    def testArrayNew11(): Unit = region rc {
        discard $ARRAY_NEW$(rc, 10i16, 5);
        ()
    }

    @test
    def testArrayNew12(): Unit = region rc {
        discard $ARRAY_NEW$(rc, 10, 5);
        ()
    }

    @test
    def testArrayNew13(): Unit = region rc {
        discard $ARRAY_NEW$(rc, 10i64, 5);
        ()
    }

    @test
    def testArrayNew14(): Unit = region rc {
        discard $ARRAY_NEW$(rc, 10ii, 5);
        ()
    }

    @test
    def testArrayNew15(): Unit = region rc {
        discard $ARRAY_NEW$(rc, 3.0f32, 5);
        ()
    }

    @test
    def testArrayNew16(): Unit = region rc {
        discard $ARRAY_NEW$(rc, 3.0f64, 5);
        ()
    }

    @test
    def testArrayNew17(): Unit = region rc {
        discard $ARRAY_NEW$(rc, "foo", 5);
        ()
    }

    @test
    def testArrayNew18(): Unit = region rc {
        discard $ARRAY_NEW$(rc, $ARRAY_NEW$(rc, 10, 10), 5);
        ()
    }

    @test
    def testArrayNew19(): Unit = region rc {
        discard $ARRAY_NEW$(rc, (1, 2), 5);
        ()
    }

    @test
    def testArrayNew20(): Unit = region rc {
        discard $ARRAY_NEW$(rc, 1 :: Nil, 5);
        ()
    }

    @test
    def testArrayNew21(): Unit = region rc {
        discard $ARRAY_NEW$(rc, None, 1);
        ()
    }
}

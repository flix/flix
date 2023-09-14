mod Test.Exp.ArrayNew {

    @Test
    def testArrayNew01(): Unit = region rc {
        discard $ARRAY_NEW$(rc, 10, 0);
        ()
    }

    @Test
    def testArrayNew02(): Unit = region rc {
        discard $ARRAY_NEW$(rc, 1, 1);
        ()
    }

    @Test
    def testArrayNew03(): Unit = region rc {
        discard $ARRAY_NEW$(rc, 0, 2);
        ()
    }

    @Test
    def testArrayNew04(): Unit = region rc {
        discard $ARRAY_NEW$(rc, 0, 113);
        ()
    }

    @Test
    def testArrayNew05(): Unit = region rc {
        discard $ARRAY_NEW$(rc, 0, 500);
        ()
    }

    @Test
    def testArrayNew06(): Unit = region rc {
        discard $ARRAY_NEW$(rc, (), 5);
        ()
    }

    @Test
    def testArrayNew07(): Unit = region rc {
        discard $ARRAY_NEW$(rc, true, 5);
        ()
    }

    @Test
    def testArrayNew08(): Unit = region rc {
        discard $ARRAY_NEW$(rc, false, 5);
        ()
    }

    @Test
    def testArrayNew09(): Unit = region rc {
        discard $ARRAY_NEW$(rc, 'a', 5);
        ()
    }

    @Test
    def testArrayNew10(): Unit = region rc {
        discard $ARRAY_NEW$(rc, 10i8, 5);
        ()
    }

    @Test
    def testArrayNew11(): Unit = region rc {
        discard $ARRAY_NEW$(rc, 10i16, 5);
        ()
    }

    @Test
    def testArrayNew12(): Unit = region rc {
        discard $ARRAY_NEW$(rc, 10, 5);
        ()
    }

    @Test
    def testArrayNew13(): Unit = region rc {
        discard $ARRAY_NEW$(rc, 10i64, 5);
        ()
    }

    @Test
    def testArrayNew14(): Unit = region rc {
        discard $ARRAY_NEW$(rc, 10ii, 5);
        ()
    }

    @Test
    def testArrayNew15(): Unit = region rc {
        discard $ARRAY_NEW$(rc, 3.0f32, 5);
        ()
    }

    @Test
    def testArrayNew16(): Unit = region rc {
        discard $ARRAY_NEW$(rc, 3.0f64, 5);
        ()
    }

    @Test
    def testArrayNew17(): Unit = region rc {
        discard $ARRAY_NEW$(rc, "foo", 5);
        ()
    }

    @Test
    def testArrayNew18(): Unit = region rc {
        discard $ARRAY_NEW$(rc, $ARRAY_NEW$(rc, 10, 10), 5);
        ()
    }

    @Test
    def testArrayNew19(): Unit = region rc {
        discard $ARRAY_NEW$(rc, (1, 2), 5);
        ()
    }

    @Test
    def testArrayNew20(): Unit = region rc {
        discard $ARRAY_NEW$(rc, 1 :: Nil, 5);
        ()
    }

    @Test
    def testArrayNew21(): Unit = region rc {
        discard $ARRAY_NEW$(rc, None, 1);
        ()
    }
}

mod Test.Exp.ArrayNew {

    use Assert.assertEq

    @Test
    def testArrayNew01(): Unit \ Assert = region rc {
        let x = %%ARRAY_NEW%%(rc, 0, 0);
        assertEq(expected = 0, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayNew02(): Unit \ Assert = region rc {
        let x = %%ARRAY_NEW%%(rc, 1, 1);
        assertEq(expected = 1, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayNew03(): Unit \ Assert = region rc {
        let x = %%ARRAY_NEW%%(rc, 0, 7);
        assertEq(expected = 7, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayNew04(): Unit \ Assert = region rc {
        let x = %%ARRAY_NEW%%(rc, 0, 8);
        assertEq(expected = 8, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayNew05(): Unit \ Assert = region rc {
        let x = %%ARRAY_NEW%%(rc, 0, 15);
        assertEq(expected = 15, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayNew06(): Unit \ Assert = region rc {
        let x = %%ARRAY_NEW%%(rc, (), 16);
        assertEq(expected = 16, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayNew07(): Unit \ Assert = region rc {
        let x = %%ARRAY_NEW%%(rc, true, 31);
        assertEq(expected = 31, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayNew08(): Unit \ Assert = region rc {
        let x = %%ARRAY_NEW%%(rc, false, 32);
        assertEq(expected = 32, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayNew09(): Unit \ Assert = region rc {
        let x = %%ARRAY_NEW%%(rc, 'a', 63);
        assertEq(expected = 63, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayNew10(): Unit \ Assert = region rc {
        let x = %%ARRAY_NEW%%(rc, 10i8, 64);
        assertEq(expected = 64, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayNew11(): Unit \ Assert = region rc {
        let x = %%ARRAY_NEW%%(rc, 10i16, 127);
        assertEq(expected = 127, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayNew12(): Unit \ Assert = region rc {
        let x = %%ARRAY_NEW%%(rc, 10, 128);
        assertEq(expected = 128, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayNew13(): Unit \ Assert = region rc {
        let x = %%ARRAY_NEW%%(rc, 10i64, 255);
        assertEq(expected = 255, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayNew14(): Unit \ Assert = region rc {
        let x = %%ARRAY_NEW%%(rc, 10ii, 256);
        assertEq(expected = 256, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayNew15(): Unit \ Assert = region rc {
        let x = %%ARRAY_NEW%%(rc, 3.0f32, 257);
        assertEq(expected = 257, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayNew16(): Unit \ Assert = region rc {
        let x = %%ARRAY_NEW%%(rc, 3.0f64, 511);
        assertEq(expected = 511, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayNew17(): Unit \ Assert = region rc {
        let x = %%ARRAY_NEW%%(rc, "foo", 512);
        assertEq(expected = 512, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayNew18(): Unit \ Assert = region rc {
        let x = %%ARRAY_NEW%%(rc, %%ARRAY_NEW%%(rc, 10, 10), 3);
        assertEq(expected = 3, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayNew19(): Unit \ Assert = region rc {
        let x = %%ARRAY_NEW%%(rc, (1, 2), 100);
        assertEq(expected = 100, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayNew20(): Unit \ Assert = region rc {
        let x = %%ARRAY_NEW%%(rc, 1 :: Nil, 383);
        assertEq(expected = 383, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayNew21(): Unit \ Assert = region rc {
        let x = %%ARRAY_NEW%%(rc, None, 42);
        assertEq(expected = 42, %%ARRAY_LENGTH%%(x))
    }
}

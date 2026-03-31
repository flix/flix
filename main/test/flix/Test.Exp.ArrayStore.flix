mod Test.Exp.ArrayStore {

    use Assert.assertEq

    @Test
    def testArrayStore01(): Unit \ Assert = region rc {
        let x = Array#{1} @ rc;
        %%ARRAY_STORE%%(x, 0, 42);
        assertEq(expected = 42, %%ARRAY_LOAD%%(x, 0))
    }

    @Test
    def testArrayStore02(): Unit \ Assert = region rc {
        let x = Array#{1, 2, 3} @ rc;
        %%ARRAY_STORE%%(x, 2, 42);
        assertEq(expected = 42, %%ARRAY_LOAD%%(x, 2))
    }

    @Test
    def testArrayStore03(): Unit \ Assert = region rc {
        let x = Array#{1, 2, 3, 4} @ rc;
        %%ARRAY_STORE%%(x, 2, 42);
        assertEq(expected = 42, %%ARRAY_LOAD%%(x, 2))
    }

    @Test
    def testArrayStore04(): Unit \ Assert = region rc {
        let x = Array#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20} @ rc;
        %%ARRAY_STORE%%(x, 10, 42);
        assertEq(expected = 42, %%ARRAY_LOAD%%(x, 10))
    }

    @Test
    def testArrayStore05(): Unit \ Assert = region rc {
        let x = Array#{1, 2} @ rc;
        %%ARRAY_STORE%%(x, 1, 42);
        assertEq(expected = 42, %%ARRAY_LOAD%%(x, 1))
    }

    @Test
    def testArrayStore06(): Unit \ Assert = region rc {
        let x = Array#{true} @ rc;
        %%ARRAY_STORE%%(x, 0, false);
        assertEq(expected = false, %%ARRAY_LOAD%%(x, 0))
    }

    @Test
    def testArrayStore07(): Unit \ Assert = region rc {
        let x = Array#{false} @ rc;
        %%ARRAY_STORE%%(x, 0, true);
        assertEq(expected = true, %%ARRAY_LOAD%%(x, 0))
    }

    @Test
    def testArrayStore08(): Unit \ Assert = region rc {
        let x = Array#{true, false} @ rc;
        %%ARRAY_STORE%%(x, 1, true);
        assertEq(expected = true, %%ARRAY_LOAD%%(x, 1))
    }

    @Test
    def testArrayStore09(): Unit \ Assert = region rc {
        let x = Array#{false, true, false} @ rc;
        %%ARRAY_STORE%%(x, 2, false);
        assertEq(expected = false, %%ARRAY_LOAD%%(x, 2))
    }

    @Test
    def testArrayStore10(): Unit \ Assert = region rc {
        let x = Array#{false, false, false, false, true} @ rc;
        %%ARRAY_STORE%%(x, 3, true);
        assertEq(expected = true, %%ARRAY_LOAD%%(x, 3))
    }

    @Test
    def testArrayStore11(): Unit \ Assert = region rc {
        let x = Array#{'c'} @ rc;
        %%ARRAY_STORE%%(x, 0, 'e');
        assertEq(expected = 'e', %%ARRAY_LOAD%%(x, 0))
    }

    @Test
    def testArrayStore12(): Unit \ Assert = region rc {
        let x = Array#{'a', 'b'} @ rc;
        %%ARRAY_STORE%%(x, 0, 'e');
        assertEq(expected = 'e', %%ARRAY_LOAD%%(x, 0))
    }

    @Test
    def testArrayStore13(): Unit \ Assert = region rc {
        let x = Array#{'a', 'b', 'c'} @ rc;
        %%ARRAY_STORE%%(x, 1, 'e');
        assertEq(expected = 'e', %%ARRAY_LOAD%%(x, 1))
    }

    @Test
    def testArrayStore14(): Unit \ Assert = region rc {
        let x = Array#{'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n'} @ rc;
        %%ARRAY_STORE%%(x, 9, 'e');
        assertEq(expected = 'e', %%ARRAY_LOAD%%(x, 9))
    }

    @Test
    def testArrayStore15(): Unit \ Assert = region rc {
        let x = Array#{1i8} @ rc;
        %%ARRAY_STORE%%(x, 0, 42i8);
        assertEq(expected = 42i8, %%ARRAY_LOAD%%(x, 0))
    }

    @Test
    def testArrayStore16(): Unit \ Assert = region rc {
        let x = Array#{1i8, 2i8, 3i8} @ rc;
        %%ARRAY_STORE%%(x, 1, 42i8);
        assertEq(expected = 42i8, %%ARRAY_LOAD%%(x, 1))
    }

    @Test
    def testArrayStore17(): Unit \ Assert = region rc {
        let x = Array#{1i8, 2i8, 3i8, 4i8} @ rc;
        %%ARRAY_STORE%%(x, 2, 42i8);
        assertEq(expected = 42i8, %%ARRAY_LOAD%%(x, 2))
    }

    @Test
    def testArrayStore18(): Unit \ Assert = region rc {
        let x = Array#{1i16} @ rc;
        %%ARRAY_STORE%%(x, 0, 42i16);
        assertEq(expected = 42i16, %%ARRAY_LOAD%%(x, 0))
    }

    @Test
    def testArrayStore19(): Unit \ Assert = region rc {
        let x = Array#{1i16, 2i16, 3i16} @ rc;
        %%ARRAY_STORE%%(x, 1, 42i16);
        assertEq(expected = 42i16, %%ARRAY_LOAD%%(x, 1))
    }

    @Test
    def testArrayStore20(): Unit \ Assert = region rc {
        let x = Array#{1i16, 2i16, 3i16, 4i16} @ rc;
        %%ARRAY_STORE%%(x, 2, 42i16);
        assertEq(expected = 42i16, %%ARRAY_LOAD%%(x, 2))
    }

    @Test
    def testArrayStore21(): Unit \ Assert = region rc {
        let x = Array#{1i64} @ rc;
        %%ARRAY_STORE%%(x, 0, 42i64);
        assertEq(expected = 42i64, %%ARRAY_LOAD%%(x, 0))
    }

    @Test
    def testArrayStore22(): Unit \ Assert = region rc {
        let x = Array#{1i64, 2i64, 3i64} @ rc;
        %%ARRAY_STORE%%(x, 1, 42i64);
        assertEq(expected = 42i64, %%ARRAY_LOAD%%(x, 1))
    }

    @Test
    def testArrayStore23(): Unit \ Assert = region rc {
        let x = Array#{1i64, 2i64, 3i64, 4i64} @ rc;
        %%ARRAY_STORE%%(x, 2, 42i64);
        assertEq(expected = 42i64, %%ARRAY_LOAD%%(x, 2))
    }

    @Test
    def testArrayStore24(): Unit \ Assert = region rc {
        let x = Array#{1.0f32} @ rc;
        %%ARRAY_STORE%%(x, 0, 42.0f32);
        assertEq(expected = 42.0f32, %%ARRAY_LOAD%%(x, 0))
    }

    @Test
    def testArrayStore25(): Unit \ Assert = region rc {
        let x = Array#{1.0f32, 2.0f32, 3.0f32} @ rc;
        %%ARRAY_STORE%%(x, 1, 42.0f32);
        assertEq(expected = 42.0f32, %%ARRAY_LOAD%%(x, 1))
    }

    @Test
    def testArrayStore26(): Unit \ Assert = region rc {
        let x = Array#{1.0f32, 2.0f32, 3.0f32, 4.0f32} @ rc;
        %%ARRAY_STORE%%(x, 2, 42.0f32);
        assertEq(expected = 42.0f32, %%ARRAY_LOAD%%(x, 2))
    }

    @Test
    def testArrayStore27(): Unit \ Assert = region rc {
        let x = Array#{1.0f64} @ rc;
        %%ARRAY_STORE%%(x, 0, 42.0f64);
        assertEq(expected = 42.0f64, %%ARRAY_LOAD%%(x, 0))
    }

    @Test
    def testArrayStore28(): Unit \ Assert = region rc {
        let x = Array#{1.0f64, 2.0f64, 3.0f64} @ rc;
        %%ARRAY_STORE%%(x, 1, 42.0f64);
        assertEq(expected = 42.0f64, %%ARRAY_LOAD%%(x, 1))
    }

    @Test
    def testArrayStore29(): Unit \ Assert = region rc {
        let x = Array#{1.0f64, 2.0f64, 3.0f64, 4.0f64} @ rc;
        %%ARRAY_STORE%%(x, 3, 42.0f64);
        assertEq(expected = 42.0f64, %%ARRAY_LOAD%%(x, 3))
    }

    @Test
    def testArrayStore30(): Unit \ Assert = region rc {
        let x = Array#{"Hello"} @ rc;
        %%ARRAY_STORE%%(x, 0, "Test");
        assertEq(expected = "Test", %%ARRAY_LOAD%%(x, 0))
    }

    @Test
    def testArrayStore31(): Unit \ Assert = region rc {
        let x = Array#{"Hello", "World"} @ rc;
        %%ARRAY_STORE%%(x, 1, "Test");
        assertEq(expected = "Test", %%ARRAY_LOAD%%(x, 1))
    }

    @Test
    def testArrayStore32(): Unit \ Assert = region rc {
        let x = Array#{"World", "Hello", "World"} @ rc;
        %%ARRAY_STORE%%(x, 2, "Test");
        assertEq(expected = "Test", %%ARRAY_LOAD%%(x, 2))
    }

    @Test
    def testArrayStore33(): Unit \ Assert = region rc {
        let x = Array#{Array#{1, 2, 3} @ rc} @ rc;
        %%ARRAY_STORE%%(%%ARRAY_LOAD%%(x, 0), 1, 42);
        assertEq(expected = 42, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 0), 1))
    }

    @Test
    def testArrayStore34(): Unit \ Assert = region rc {
        let x = Array#{Array#{1, 2} @ rc, Array#{1, 2} @ rc} @ rc;
        %%ARRAY_STORE%%(%%ARRAY_LOAD%%(x, 1), 0, 42);
        assertEq(expected = 42, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 1), 0))
    }

    @Test
    def testArrayStore35(): Unit \ Assert = region rc {
        let x = Array#{Array#{1} @ rc, Array#{1, 2, 3, 4, 5, 6, 7, 8} @ rc} @ rc;
        %%ARRAY_STORE%%(%%ARRAY_LOAD%%(x, 1), 5, 42);
        assertEq(expected = 42, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 1), 5))
    }

    @Test
    def testArrayStore36(): Unit \ Assert = region rc {
        let x = Array#{Array#{1, 2, 3, 4, 5} @ rc, Array#{2} @ rc} @ rc;
        %%ARRAY_STORE%%(%%ARRAY_LOAD%%(x, 0), 3, 42);
        assertEq(expected = 42, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 0), 3))
    }

    @Test
    def testArrayStore37(): Unit \ Assert = region rc {
        let x = Array#{Array#{1, 2, 3} @ rc, Array#{1, 2, 3} @ rc, Array#{1, 2, 3} @ rc} @ rc;
        %%ARRAY_STORE%%(%%ARRAY_LOAD%%(x, 2), 2, 42);
        assertEq(expected = 42, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 2), 2))
    }

    @Test
    def testArrayStore38(): Unit \ Assert = region rc {
        let x = Array#{Array#{(7 , 10)} @ rc, Array#{(7 , 10)} @ rc} @ rc;
        %%ARRAY_STORE%%(x, 1, Array#{(19 , 10)} @ rc);
        assertEq(expected = (19, 10), %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 1), 0))
    }

    @Test
    def testArrayStore39(): Unit \ Assert = region rc {
        let x = Array#{Array#{7 :: 10 :: Nil} @ rc, Array#{7 :: Nil} @ rc} @ rc;
        %%ARRAY_STORE%%(x, 1, Array#{19 :: Nil} @ rc);
        assertEq(expected = 19 :: Nil, %%ARRAY_LOAD%%(%%ARRAY_LOAD%%(x, 1), 0))
    }

    @Test
    def testArrayStore40(): Unit \ Assert = region rc {
        let x = Array#{None, None, None, Some(4)} @ rc;
        %%ARRAY_STORE%%(x, 1, Some(6));
        assertEq(expected = Some(6), %%ARRAY_LOAD%%(x, 1))
    }

}

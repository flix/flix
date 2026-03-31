mod Test.Exp.ArrayLength {

    use Assert.assertEq

    @Test
    def testArrayLength01(): Unit \ Assert = region rc {
        let x = Array#{} @ rc;
        assertEq(expected = 0, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength02(): Unit \ Assert = region rc {
        let x = Array#{1} @ rc;
        assertEq(expected = 1, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength03(): Unit \ Assert = region rc {
        let x = Array#{1, 2, 3} @ rc;
        assertEq(expected = 3, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength04(): Unit \ Assert = region rc {
        let x = Array#{1, 2, 3, 4} @ rc;
        assertEq(expected = 4, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength05(): Unit \ Assert = region rc {
        let x = Array#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20} @ rc;
        assertEq(expected = 20, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength06(): Unit \ Assert = region rc {
        let x = Array#{1, 2} @ rc;
        assertEq(expected = 2, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength07(): Unit \ Assert = region rc {
        let x = Array#{true} @ rc;
        assertEq(expected = 1, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength08(): Unit \ Assert = region rc {
        let x = Array#{false} @ rc;
        assertEq(expected = 1, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength09(): Unit \ Assert = region rc {
        let x = Array#{true, false} @ rc;
        assertEq(expected = 2, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength10(): Unit \ Assert = region rc {
        let x = Array#{false, true, false} @ rc;
        assertEq(expected = 3, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength11(): Unit \ Assert = region rc {
        let x = Array#{false, false, false, false, true} @ rc;
        assertEq(expected = 5, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength12(): Unit \ Assert = region rc {
        let x = Array#{'c'} @ rc;
        assertEq(expected = 1, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength13(): Unit \ Assert = region rc {
        let x = Array#{'a', 'b'} @ rc;
        assertEq(expected = 2, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength14(): Unit \ Assert = region rc {
        let x = Array#{'a', 'b', 'c'} @ rc;
        assertEq(expected = 3, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength15(): Unit \ Assert = region rc {
        let x = Array#{'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n'} @ rc;
        assertEq(expected = 14, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength16(): Unit \ Assert = region rc {
        let x = Array#{1i8} @ rc;
        assertEq(expected = 1, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength17(): Unit \ Assert = region rc {
        let x = Array#{1i8, 2i8, 3i8} @ rc;
        assertEq(expected = 3, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength18(): Unit \ Assert = region rc {
        let x = Array#{1i8, 2i8, 3i8, 4i8} @ rc;
        assertEq(expected = 4, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength19(): Unit \ Assert = region rc {
        let x = Array#{1i16} @ rc;
        assertEq(expected = 1, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength20(): Unit \ Assert = region rc {
        let x = Array#{1i16, 2i16, 3i16} @ rc;
        assertEq(expected = 3, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength21(): Unit \ Assert = region rc {
        let x = Array#{1i16, 2i16, 3i16, 4i16} @ rc;
        assertEq(expected = 4, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength22(): Unit \ Assert = region rc {
        let x = Array#{1i64} @ rc;
        assertEq(expected = 1, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength23(): Unit \ Assert = region rc {
        let x = Array#{1i64, 2i64, 3i64} @ rc;
        assertEq(expected = 3, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength24(): Unit \ Assert = region rc {
        let x = Array#{1i64, 2i64, 3i64, 4i64} @ rc;
        assertEq(expected = 4, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength25(): Unit \ Assert = region rc {
        let x = Array#{1.0f32} @ rc;
        assertEq(expected = 1, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength26(): Unit \ Assert = region rc {
        let x = Array#{1.0f32, 2.0f32, 3.0f32} @ rc;
        assertEq(expected = 3, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength27(): Unit \ Assert = region rc {
        let x = Array#{1.0f32, 2.0f32, 3.0f32, 4.0f32} @ rc;
        assertEq(expected = 4, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength28(): Unit \ Assert = region rc {
        let x = Array#{1.0f64} @ rc;
        assertEq(expected = 1, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength29(): Unit \ Assert = region rc {
        let x = Array#{1.0f64, 2.0f64, 3.0f64} @ rc;
        assertEq(expected = 3, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength30(): Unit \ Assert = region rc {
        let x = Array#{1.0f64, 2.0f64, 3.0f64, 4.0f64} @ rc;
        assertEq(expected = 4, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength31(): Unit \ Assert = region rc {
        let x = Array#{"Hello"} @ rc;
        assertEq(expected = 1, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength32(): Unit \ Assert = region rc {
        let x = Array#{"Hello", "World"} @ rc;
        assertEq(expected = 2, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength33(): Unit \ Assert = region rc {
        let x = Array#{"World", "Hello", "World"} @ rc;
        assertEq(expected = 3, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength34(): Unit \ Assert = region rc {
        let x = Array#{Array#{1, 2, 3} @ rc} @ rc;
        assertEq(expected = 1, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength35(): Unit \ Assert = region rc {
        let x = Array#{Array#{1, 2} @ rc, Array#{1, 2} @ rc} @ rc;
        assertEq(expected = 2, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength36(): Unit \ Assert = region rc {
        let x = Array#{Array#{1} @ rc, Array#{1, 2, 3, 4, 5, 6, 7, 8} @ rc} @ rc;
        assertEq(expected = 2, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength37(): Unit \ Assert = region rc {
        let x = Array#{Array#{1, 2, 3, 4, 5} @ rc, Array#{2} @ rc} @ rc;
        assertEq(expected = 2, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength38(): Unit \ Assert = region rc {
        let x = Array#{Array#{1, 2, 3} @ rc, Array#{1, 2, 3} @ rc, Array#{1, 2, 3} @ rc} @ rc;
        assertEq(expected = 3, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength39(): Unit \ Assert = region rc {
        let x = Array#{Array#{None, Some(1), None} @ rc, Array#{Some(2), Some(45), Some(9), None} @ rc} @ rc;
        assertEq(expected = 2, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength40(): Unit \ Assert = region rc {
        let x = Array#{(1, 2), (3, 4)} @ rc;
        assertEq(expected = 2, %%ARRAY_LENGTH%%(x))
    }

    @Test
    def testArrayLength41(): Unit \ Assert = region rc {
        let x = Array#{Array#{1 :: Nil} @ rc, Array#{3 :: 4 :: 5 :: Nil} @ rc} @ rc;
        assertEq(expected = 2, %%ARRAY_LENGTH%%(x))
    }

}

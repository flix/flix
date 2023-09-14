mod Test.Exp.ArrayLength {

    @Test
    def testArrayLength01(): Int32 = region rc {
        let x = Array#{} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength02(): Int32 = region rc {
        let x = Array#{1} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength03(): Int32 = region rc {
        let x = Array#{1, 2, 3} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength04(): Int32 = region rc {
        let x = Array#{1, 2, 3, 4} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength05(): Int32 = region rc {
        let x = Array#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength06(): Int32 = region rc {
        let x = Array#{1, 2} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength07(): Int32 = region rc {
        let x = Array#{true} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength08(): Int32 = region rc {
        let x = Array#{false} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength09(): Int32 = region rc {
        let x = Array#{true, false} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength10(): Int32 = region rc {
        let x = Array#{false, true, false} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength11(): Int32 = region rc {
        let x = Array#{false, false, false, false, true} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength12(): Int32 = region rc {
        let x = Array#{'c'} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength13(): Int32 = region rc {
        let x = Array#{'a', 'b'} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength14(): Int32 = region rc {
        let x = Array#{'a', 'b', 'c'} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength15(): Int32 = region rc {
        let x = Array#{'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n'} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength16(): Int32 = region rc {
        let x = Array#{1i8} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength17(): Int32 = region rc {
        let x = Array#{1i8, 2i8, 3i8} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength18(): Int32 = region rc {
        let x = Array#{1i8, 2i8, 3i8, 4i8} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength19(): Int32 = region rc {
        let x = Array#{1i16} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength20(): Int32 = region rc {
        let x = Array#{1i16, 2i16, 3i16} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength21(): Int32 = region rc {
        let x = Array#{1i16, 2i16, 3i16, 4i16} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength22(): Int32 = region rc {
        let x = Array#{1i64} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength23(): Int32 = region rc {
        let x = Array#{1i64, 2i64, 3i64} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength24(): Int32 = region rc {
        let x = Array#{1i64, 2i64, 3i64, 4i64} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength25(): Int32 = region rc {
        let x = Array#{1.0f32} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength26(): Int32 = region rc {
        let x = Array#{1.0f32, 2.0f32, 3.0f32} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength27(): Int32 = region rc {
        let x = Array#{1.0f32, 2.0f32, 3.0f32, 4.0f32} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength28(): Int32 = region rc {
        let x = Array#{1.0f64} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength29(): Int32 = region rc {
        let x = Array#{1.0f64, 2.0f64, 3.0f64} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength30(): Int32 = region rc {
        let x = Array#{1.0f64, 2.0f64, 3.0f64, 4.0f64} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength31(): Int32 = region rc {
        let x = Array#{"Hello"} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength32(): Int32 = region rc {
        let x = Array#{"Hello", "World"} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength33(): Int32 = region rc {
        let x = Array#{"World", "Hello", "World"} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength34(): Int32 = region rc {
        let x = Array#{Array#{1, 2, 3} @ rc} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength35(): Int32 = region rc {
        let x = Array#{Array#{1, 2} @ rc, Array#{1, 2} @ rc} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength36(): Int32 = region rc {
        let x = Array#{Array#{1} @ rc, Array#{1, 2, 3, 4, 5, 6, 7, 8} @ rc} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength37(): Int32 = region rc {
        let x = Array#{Array#{1, 2, 3, 4, 5} @ rc, Array#{2} @ rc} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength38(): Int32 = region rc {
        let x = Array#{Array#{1, 2, 3} @ rc, Array#{1, 2, 3} @ rc, Array#{1, 2, 3} @ rc} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength39(): Int32 = region rc {
        let x = Array#{Array#{None, Some(1), None} @ rc, Array#{Some(2), Some(45), Some(9), None} @ rc} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength40(): Int32 = region rc {
        let x = Array#{(1, 2), (3, 4)} @ rc;
        $ARRAY_LENGTH$(x)
    }

    @Test
    def testArrayLength41(): Int32 = region rc {
        let x = Array#{Array#{1 :: Nil} @ rc, Array#{3 :: 4 :: 5 :: Nil} @ rc} @ rc;
        $ARRAY_LENGTH$(x)
    }
}

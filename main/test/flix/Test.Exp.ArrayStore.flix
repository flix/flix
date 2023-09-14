mod Test.Exp.ArrayStore {

    @Test
    def testArrayStore01(): Unit = region rc {
        let x = Array#{1} @ rc;
        $ARRAY_STORE$(x, 0, 42)
    }

    @Test
    def testArrayStore02(): Unit = region rc {
        let x = Array#{1, 2, 3} @ rc;
        $ARRAY_STORE$(x, 2, 42)
    }

    @Test
    def testArrayStore03(): Unit = region rc {
        let x = Array#{1, 2, 3, 4} @ rc;
        $ARRAY_STORE$(x, 2, 42)
    }

    @Test
    def testArrayStore04(): Unit = region rc {
        let x = Array#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20} @ rc;
        $ARRAY_STORE$(x, 10, 42)
    }

    @Test
    def testArrayStore05(): Unit = region rc {
        let x = Array#{1, 2} @ rc;
        $ARRAY_STORE$(x, 1, 42)
    }

    @Test
    def testArrayStore06(): Unit = region rc {
        let x = Array#{true} @ rc;
        $ARRAY_STORE$(x, 0, false)
    }

    @Test
    def testArrayStore07(): Unit = region rc {
        let x = Array#{false} @ rc;
        $ARRAY_STORE$(x, 0, true)
    }

    @Test
    def testArrayStore08(): Unit = region rc {
        let x = Array#{true, false} @ rc;
        $ARRAY_STORE$(x, 1, true)
    }

    @Test
    def testArrayStore9(): Unit = region rc {
        let x = Array#{false, true, false} @ rc;
        $ARRAY_STORE$(x, 2, false)
    }

    @Test
    def testArrayStore10(): Unit = region rc {
        let x = Array#{false, false, false, false, true} @ rc;
        $ARRAY_STORE$(x, 3, true)
    }

    @Test
    def testArrayStore11(): Unit = region rc {
        let x = Array#{'c'} @ rc;
        $ARRAY_STORE$(x, 0, 'e')
    }

    @Test
    def testArrayStore12(): Unit = region rc {
        let x = Array#{'a', 'b'} @ rc;
        $ARRAY_STORE$(x, 0, 'e')
    }

    @Test
    def testArrayStore13(): Unit = region rc {
        let x = Array#{'a', 'b', 'c'} @ rc;
        $ARRAY_STORE$(x, 1, 'e')
    }

    @Test
    def testArrayStore14(): Unit = region rc {
        let x = Array#{'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n'} @ rc;
        $ARRAY_STORE$(x, 9, 'e')
    }

    @Test
    def testArrayStore15(): Unit = region rc {
        let x = Array#{1i8} @ rc;
        $ARRAY_STORE$(x, 0, 42i8)
    }

    @Test
    def testArrayStore16(): Unit = region rc {
        let x = Array#{1i8, 2i8, 3i8} @ rc;
        $ARRAY_STORE$(x, 1, 42i8)
    }

    @Test
    def testArrayStore17(): Unit = region rc {
        let x = Array#{1i8, 2i8, 3i8, 4i8} @ rc;
        $ARRAY_STORE$(x, 2, 42i8)
    }

    @Test
    def testArrayStore18(): Unit = region rc {
        let x = Array#{1i16} @ rc;
        $ARRAY_STORE$(x, 0, 42i16)
    }

    @Test
    def testArrayStore19(): Unit = region rc {
        let x = Array#{1i16, 2i16, 3i16} @ rc;
        $ARRAY_STORE$(x, 1, 42i16)
    }

    @Test
    def testArrayStore20(): Unit = region rc {
        let x = Array#{1i16, 2i16, 3i16, 4i16} @ rc;
        $ARRAY_STORE$(x, 2, 42i16)
    }

    @Test
    def testArrayStore21(): Unit = region rc {
        let x = Array#{1i64} @ rc;
        $ARRAY_STORE$(x, 0, 42i64)
    }

    @Test
    def testArrayStore22(): Unit = region rc {
        let x = Array#{1i64, 2i64, 3i64} @ rc;
        $ARRAY_STORE$(x, 1, 42i64)
    }

    @Test
    def testArrayStore23(): Unit = region rc {
        let x = Array#{1i64, 2i64, 3i64, 4i64} @ rc;
        $ARRAY_STORE$(x, 2, 42i64)
    }

    @Test
    def testArrayStore24(): Unit = region rc {
        let x = Array#{1.0f32} @ rc;
        $ARRAY_STORE$(x, 0, 42.0f32)
    }

    @Test
    def testArrayStore25(): Unit = region rc {
        let x = Array#{1.0f32, 2.0f32, 3.0f32} @ rc;
        $ARRAY_STORE$(x, 1, 42.0f32)
    }

    @Test
    def testArrayStore26(): Unit = region rc {
        let x = Array#{1.0f32, 2.0f32, 3.0f32, 4.0f32} @ rc;
        $ARRAY_STORE$(x, 2, 42.0f32)
    }

    @Test
    def testArrayStore27(): Unit = region rc {
        let x = Array#{1.0f64} @ rc;
        $ARRAY_STORE$(x, 0, 42.0f64)
    }

    @Test
    def testArrayStore28(): Unit = region rc {
        let x = Array#{1.0f64, 2.0f64, 3.0f64} @ rc;
        $ARRAY_STORE$(x, 1, 42.0f64)
    }

    @Test
    def testArrayStore29(): Unit = region rc {
        let x = Array#{1.0f64, 2.0f64, 3.0f64, 4.0f64} @ rc;
        $ARRAY_STORE$(x, 3, 42.0f64)
    }

    @Test
    def testArrayStore30(): Unit = region rc {
        let x = Array#{"Hello"} @ rc;
        $ARRAY_STORE$(x, 0, "Test")
    }

    @Test
    def testArrayStore31(): Unit = region rc {
        let x = Array#{"Hello", "World"} @ rc;
        $ARRAY_STORE$(x, 1, "Test")
    }

    @Test
    def testArrayStore32(): Unit = region rc {
        let x = Array#{"World", "Hello", "World"} @ rc;
        $ARRAY_STORE$(x, 2, "Test")
    }

    @Test
    def testArrayStore33(): Unit = region rc {
        let x = Array#{Array#{1, 2, 3} @ rc} @ rc;
        $ARRAY_STORE$($ARRAY_LOAD$(x, 0), 1, 42)
    }

    @Test
    def testArrayStore34(): Unit = region rc {
        let x = Array#{Array#{1, 2} @ rc, Array#{1, 2} @ rc} @ rc;
        $ARRAY_STORE$($ARRAY_LOAD$(x, 1), 0, 42)
    }

    @Test
    def testArrayStore35(): Unit = region rc {
        let x = Array#{Array#{1} @ rc, Array#{1, 2, 3, 4, 5, 6, 7, 8} @ rc} @ rc;
        $ARRAY_STORE$($ARRAY_LOAD$(x, 1), 5, 42)
    }

    @Test
    def testArrayStore36(): Unit = region rc {
        let x = Array#{Array#{1, 2, 3, 4, 5} @ rc, Array#{2} @ rc} @ rc;
        $ARRAY_STORE$($ARRAY_LOAD$(x, 0), 3, 42)
    }

    @Test
    def testArrayStore37(): Unit = region rc {
        let x = Array#{Array#{1, 2, 3} @ rc, Array#{1, 2, 3} @ rc, Array#{1, 2, 3} @ rc} @ rc;
        $ARRAY_STORE$($ARRAY_LOAD$(x, 2), 2, 42)
    }

    @Test
    def testArrayStore38(): Unit = region rc {
        let x = Array#{Array#{(7 , 10)} @ rc, Array#{(7 , 10)} @ rc} @ rc;
        $ARRAY_STORE$(x, 1, Array#{(19 , 10)} @ rc)
    }

    @Test
    def testArrayStore39(): Unit = region rc {
        let x = Array#{Array#{7 :: 10 :: Nil} @ rc, Array#{7 :: Nil} @ rc} @ rc;
        $ARRAY_STORE$(x, 1, Array#{19 :: Nil} @ rc)
    }

    @Test
    def testArrayStore40(): Unit = region rc {
        let x = Array#{None, None, None, Some(4)} @ rc;
        $ARRAY_STORE$(x, 1, Some(6))
    }

}

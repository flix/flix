mod Test.Exp.ArrayLit {

    @Test
    def testArrayLit01(): Unit = region rc {
        discard Array#{} @ rc;
        ()
    }

    @Test
    def testArrayLit02(): Unit = region rc {
        discard Array#{1} @ rc;
        ()
    }

    @Test
    def testArrayLit04(): Unit = region rc {
        discard Array#{1, 2, 3, 4} @ rc;
        ()
    }

    @Test
    def testArrayLit05(): Unit = region rc {
        discard Array#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20} @ rc;
        ()
    }

    @Test
    def testArrayLit06(): Unit = region rc {
        discard Array#{1, 2} @ rc;
        ()
    }

    @Test
    def testArrayLit07(): Unit = region rc {
        discard Array#{true} @ rc;
        ()
    }

    @Test
    def testArrayLit08(): Unit = region rc {
        discard Array#{false} @ rc;
        ()
    }

    @Test
    def testArrayLit09(): Unit = region rc {
        discard Array#{true, false} @ rc;
        ()
    }

    @Test
    def testArrayLit10(): Unit = region rc {
        discard Array#{false, true, false} @ rc;
        ()
    }

    @Test
    def testArrayLit11(): Unit = region rc {
        discard Array#{false, false, false, false, true} @ rc;
        ()
    }

    @Test
    def testArrayLit12(): Unit = region rc {
        discard Array#{'c'} @ rc;
        ()
    }

    @Test
    def testArrayLit13(): Unit = region rc {
        discard Array#{'a', 'b'} @ rc;
        ()
    }

    @Test
    def testArrayLit14(): Unit = region rc {
        discard Array#{'a', 'b', 'c'} @ rc;
        ()
    }

    @Test
    def testArrayLit15(): Unit = region rc {
        discard Array#{'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n'} @ rc;
        ()
    }

    @Test
    def testArrayLit16(): Unit = region rc {
        discard Array#{1i8} @ rc;
        ()
    }

    @Test
    def testArrayLit17(): Unit = region rc {
        discard Array#{1i8, 2i8, 3i8} @ rc;
        ()
    }

    @Test
    def testArrayLit18(): Unit = region rc {
        discard Array#{1i8, 2i8, 3i8, 4i8} @ rc;
        ()
    }

    @Test
    def testArrayLit19(): Unit = region rc {
        discard Array#{1i16} @ rc;
        ()
    }

    @Test
    def testArrayLit20(): Unit = region rc {
        discard Array#{1i16, 2i16, 3i16} @ rc;
        ()
    }

    @Test
    def testArrayLit21(): Unit = region rc {
        discard Array#{1i16, 2i16, 3i16, 4i16} @ rc;
        ()
    }

    @Test
    def testArrayLit22(): Unit = region rc {
        discard Array#{1i64} @ rc;
        ()
    }

    @Test
    def testArrayLit23(): Unit = region rc {
        discard Array#{1i64, 2i64, 3i64} @ rc;
        ()
    }

    @Test
    def testArrayLit24(): Unit = region rc {
        discard Array#{1i64, 2i64, 3i64, 4i64} @ rc;
        ()
    }

    @Test
    def testArrayLit25(): Unit = region rc {
        discard Array#{1.0f32} @ rc;
        ()
    }

    @Test
    def testArrayLit26(): Unit = region rc {
        discard Array#{1.0f32, 2.0f32, 3.0f32} @ rc;
        ()
    }

    @Test
    def testArrayLit27(): Unit = region rc {
        discard Array#{1.0f32, 2.0f32, 3.0f32, 4.0f32} @ rc;
        ()
    }

    @Test
    def testArrayLit28(): Unit = region rc {
        discard Array#{1.0f64} @ rc;
        ()
    }

    @Test
    def testArrayLit29(): Unit = region rc {
        discard Array#{1.0f64, 2.0f64, 3.0f64} @ rc;
        ()
    }

    @Test
    def testArrayLit30(): Unit = region rc {
        discard Array#{1.0f64, 2.0f64, 3.0f64, 4.0f64} @ rc;
        ()
    }

    @Test
    def testArrayLit31(): Unit = region rc {
        discard Array#{"Hello"} @ rc;
        ()
    }

    @Test
    def testArrayLit32(): Unit = region rc {
        discard Array#{"Hello", "World"} @ rc;
        ()
    }

    @Test
    def testArrayLit33(): Unit = region rc {
        discard Array#{"World", "Hello", "World"} @ rc;
        ()
    }

    @Test
    def testArrayLit34(): Unit = region rc {
        discard Array#{Array#{1,2,3} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit35(): Unit = region rc {
        discard Array#{Array#{1, 2} @ rc, Array#{1, 2} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit36(): Unit = region rc {
        discard Array#{Array#{1} @ rc, Array#{1, 2, 3, 4, 5, 6, 7, 8} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit37(): Unit = region rc {
        discard Array#{Array#{1, 2, 3, 4, 5} @ rc, Array#{2} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit38(): Unit = region rc {
        discard Array#{Array#{1, 2, 3} @ rc, Array#{1, 2, 3} @ rc, Array#{1, 2, 3} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit39(): Unit = region rc {
        discard Array#{Array#{true, false} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit40(): Unit = region rc {
        discard Array#{Array#{true, false} @ rc, Array#{true, false} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit41(): Unit = region rc {
        discard Array#{Array#{true} @ rc, Array#{true, true, false, false} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit42(): Unit = region rc {
        discard Array#{Array#{true, false, true, false, true, false} @ rc, Array#{true} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit43(): Unit = region rc {
        discard Array#{Array#{true, true, true} @ rc, Array#{false, false, false} @ rc, Array#{false, true, false} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit44(): Unit = region rc {
        discard Array#{Array#{'a', 'b', 'c'} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit45(): Unit = region rc {
        discard Array#{Array#{'a', 'b'} @ rc, Array#{'a', 'b'} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit46(): Unit = region rc {
        discard Array#{Array#{'a'} @ rc, Array#{'a', 'b', 'c', 'd', 'e'} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit47(): Unit = region rc {
        discard Array#{Array#{'a', 'b', 'c', 'd', 'e'} @ rc, Array#{'a'} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit48(): Unit = region rc {
        discard Array#{Array#{'a', 'b', 'c'} @ rc, Array#{'a', 'b', 'c'} @ rc, Array#{'a', 'b', 'c'} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit49(): Unit = region rc {
        discard Array#{Array#{1i8, 2i8, 3i8} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit50(): Unit = region rc {
        discard Array#{Array#{1i8, 2i8} @ rc, Array#{1i8, 2i8} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit51(): Unit = region rc {
        discard Array#{Array#{1i8} @ rc, Array#{1i8, 2i8, 3i8, 4i8, 5i8, 6i8} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit52(): Unit = region rc {
        discard Array#{Array#{1i8, 2i8, 3i8, 4i8, 5i8, 6i8} @ rc, Array#{1i8} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit53(): Unit = region rc {
        discard Array#{Array#{1i8, 2i8, 3i8} @ rc, Array#{1i8, 2i8, 3i8} @ rc, Array#{1i8, 2i8, 3i8} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit54(): Unit = region rc {
        discard Array#{Array#{1i16, 2i16, 3i16} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit55(): Unit = region rc {
        discard Array#{Array#{1i16, 2i16} @ rc, Array#{1i16, 2i16} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit56(): Unit = region rc {
        discard Array#{Array#{1i16} @ rc, Array#{1i16, 2i16, 3i16, 4i16, 5i16, 6i16} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit57(): Unit = region rc {
        discard Array#{Array#{1i16, 2i16, 3i16, 4i16, 5i16, 6i16} @ rc, Array#{1i16} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit58(): Unit = region rc {
        discard Array#{Array#{1i16, 2i16, 3i16} @ rc, Array#{1i16, 2i16, 3i16} @ rc, Array#{1i16, 2i16, 3i16} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit59(): Unit = region rc {
        discard Array#{Array#{1i64, 2i64, 3i64} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit60(): Unit = region rc {
        discard Array#{Array#{1i64, 2i64} @ rc, Array#{1i64, 2i64} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit61(): Unit = region rc {
        discard Array#{Array#{1i64} @ rc, Array#{1i64, 2i64, 3i64, 4i64, 5i64, 6i64} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit62(): Unit = region rc {
        discard Array#{Array#{1i64, 2i64, 3i64, 4i64, 5i64, 6i64} @ rc, Array#{1i64} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit63(): Unit = region rc {
        discard Array#{Array#{1i64, 2i64, 3i64} @ rc, Array#{1i64, 2i64, 3i64} @ rc, Array#{1i64, 2i64, 3i64} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit64(): Unit = region rc {
        discard Array#{Array#{1.0f32, 2.0f32, 3.0f32} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit65(): Unit = region rc {
        discard Array#{Array#{1.0f32, 2.0f32} @ rc, Array#{1.0f32, 2.0f32} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit66(): Unit = region rc {
        discard Array#{Array#{1.0f32} @ rc, Array#{1.0f32, 2.0f32, 3.0f32, 4.0f32, 5.0f32, 6.0f32} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit67(): Unit = region rc {
        discard Array#{Array#{1.0f32, 2.0f32, 3.0f32, 4.0f32, 5.0f32, 6.0f32} @ rc, Array#{1.0f32} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit68(): Unit = region rc {
        discard Array#{Array#{1.0f64, 2.0f64, 3.0f64} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit69(): Unit = region rc {
        discard Array#{Array#{1.0f64, 2.0f64} @ rc, Array#{3.0f64, 4.0f64} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit70(): Unit = region rc {
        discard Array#{Array#{1.0f64} @ rc, Array#{1.0f64, 2.0f64, 3.0f64, 4.0f64, 5.0f64, 6.0f64} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit71(): Unit = region rc {
        discard Array#{Array#{1.0f64, 2.0f64, 3.0f64, 4.0f64, 5.0f64, 6.0f64} @ rc, Array#{1.0f64} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit72(): Unit = region rc {
        discard Array#{Array#{1.0f64, 2.0f64, 3.0f64} @ rc, Array#{1.0f64, 2.0f64, 3.0f64} @ rc, Array#{1.0f64, 2.0f64, 3.0f64} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit73(): Unit = region rc {
        discard Array#{Array#{"Hello"} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit74(): Unit = region rc {
        discard Array#{Array#{"Hello"} @ rc, Array#{"World"} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit75(): Unit = region rc {
        discard Array#{Array#{"Hello"} @ rc, Array#{"Hello", "World", "World", "Hello"} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit76(): Unit = region rc {
        discard Array#{Array#{"Hello", "World", "World", "Hello"} @ rc, Array#{"Hello"} @ rc} @ rc;
        ()
    }

    @Test
    def testArrayLit77(): Unit = region rc {
        discard Array#{(12, 42)} @ rc;
        ()
    }

    @Test
    def testArrayLit78(): Unit = region rc {
        discard Array#{(12, 42, 34), (12, 42, 34)} @ rc;
        ()
    }

    @Test
    def testArrayLit79(): Unit = region rc {
        discard Array#{(12, 42, 12), (12, 23, 12), (12,12,12)} @ rc;
        ()
    }

    @Test
    def testArrayLit80(): Unit = region rc {
        discard Array#{(12, 42, 12, 12), (12, 23, 12, 12), (12,12,12, 31)} @ rc;
        ()
    }

    @Test
    def testArrayLit81(): Unit = region rc {
        discard Array#{(1,1), (1,1), (2,2), (3,3), (1,1), (2,1), (5,5)} @ rc;
        ()
    }

    @Test
    def testArrayLit82(): Unit = region rc {
        discard Array#{(true, true)} @ rc;
        ()
    }

    @Test
    def testArrayLit83(): Unit = region rc {
        discard Array#{(true, true, true), (true, true, true)} @ rc;
        ()
    }

    @Test
    def testArrayLit84(): Unit = region rc {
        discard Array#{(true, true, true), (true, false, false), (true, true, false)} @ rc;
        ()
    }

    @Test
    def testArrayLit85(): Unit = region rc {
        discard Array#{(Nil)} @ rc;
        ()
    }

    @Test
    def testArrayLit86(): Unit = region rc {
        discard Array#{(1 :: Nil)} @ rc;
        ()
    }

    @Test
    def testArrayLit88(): Unit = region rc {
        discard Array#{Some("Hello World")} @ rc;
        ()
    }

    @Test
    def testArrayLit89(): Unit = region rc {
        discard Array#{Some(1), Some(2), None} @ rc;
        ()
    }
}

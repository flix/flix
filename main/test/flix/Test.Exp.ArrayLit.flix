mod Test.Exp.ArrayLit {

    @test
    def testArrayLit01(): Unit = region rc {
        discard Array#{} @ rc;
        ()
    }

    @test
    def testArrayLit02(): Unit = region rc {
        discard Array#{1} @ rc;
        ()
    }

    @test
    def testArrayLit04(): Unit = region rc {
        discard Array#{1, 2, 3, 4} @ rc;
        ()
    }

    @test
    def testArrayLit05(): Unit = region rc {
        discard Array#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20} @ rc;
        ()
    }

    @test
    def testArrayLit06(): Unit = region rc {
        discard Array#{1, 2} @ rc;
        ()
    }

    @test
    def testArrayLit07(): Unit = region rc {
        discard Array#{true} @ rc;
        ()
    }

    @test
    def testArrayLit08(): Unit = region rc {
        discard Array#{false} @ rc;
        ()
    }

    @test
    def testArrayLit09(): Unit = region rc {
        discard Array#{true, false} @ rc;
        ()
    }

    @test
    def testArrayLit10(): Unit = region rc {
        discard Array#{false, true, false} @ rc;
        ()
    }

    @test
    def testArrayLit11(): Unit = region rc {
        discard Array#{false, false, false, false, true} @ rc;
        ()
    }

    @test
    def testArrayLit12(): Unit = region rc {
        discard Array#{'c'} @ rc;
        ()
    }

    @test
    def testArrayLit13(): Unit = region rc {
        discard Array#{'a', 'b'} @ rc;
        ()
    }

    @test
    def testArrayLit14(): Unit = region rc {
        discard Array#{'a', 'b', 'c'} @ rc;
        ()
    }

    @test
    def testArrayLit15(): Unit = region rc {
        discard Array#{'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n'} @ rc;
        ()
    }

    @test
    def testArrayLit16(): Unit = region rc {
        discard Array#{1i8} @ rc;
        ()
    }

    @test
    def testArrayLit17(): Unit = region rc {
        discard Array#{1i8, 2i8, 3i8} @ rc;
        ()
    }

    @test
    def testArrayLit18(): Unit = region rc {
        discard Array#{1i8, 2i8, 3i8, 4i8} @ rc;
        ()
    }

    @test
    def testArrayLit19(): Unit = region rc {
        discard Array#{1i16} @ rc;
        ()
    }

    @test
    def testArrayLit20(): Unit = region rc {
        discard Array#{1i16, 2i16, 3i16} @ rc;
        ()
    }

    @test
    def testArrayLit21(): Unit = region rc {
        discard Array#{1i16, 2i16, 3i16, 4i16} @ rc;
        ()
    }

    @test
    def testArrayLit22(): Unit = region rc {
        discard Array#{1i64} @ rc;
        ()
    }

    @test
    def testArrayLit23(): Unit = region rc {
        discard Array#{1i64, 2i64, 3i64} @ rc;
        ()
    }

    @test
    def testArrayLit24(): Unit = region rc {
        discard Array#{1i64, 2i64, 3i64, 4i64} @ rc;
        ()
    }

    @test
    def testArrayLit25(): Unit = region rc {
        discard Array#{1.0f32} @ rc;
        ()
    }

    @test
    def testArrayLit26(): Unit = region rc {
        discard Array#{1.0f32, 2.0f32, 3.0f32} @ rc;
        ()
    }

    @test
    def testArrayLit27(): Unit = region rc {
        discard Array#{1.0f32, 2.0f32, 3.0f32, 4.0f32} @ rc;
        ()
    }

    @test
    def testArrayLit28(): Unit = region rc {
        discard Array#{1.0f64} @ rc;
        ()
    }

    @test
    def testArrayLit29(): Unit = region rc {
        discard Array#{1.0f64, 2.0f64, 3.0f64} @ rc;
        ()
    }

    @test
    def testArrayLit30(): Unit = region rc {
        discard Array#{1.0f64, 2.0f64, 3.0f64, 4.0f64} @ rc;
        ()
    }

    @test
    def testArrayLit31(): Unit = region rc {
        discard Array#{"Hello"} @ rc;
        ()
    }

    @test
    def testArrayLit32(): Unit = region rc {
        discard Array#{"Hello", "World"} @ rc;
        ()
    }

    @test
    def testArrayLit33(): Unit = region rc {
        discard Array#{"World", "Hello", "World"} @ rc;
        ()
    }

    @test
    def testArrayLit34(): Unit = region rc {
        discard Array#{Array#{1,2,3} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit35(): Unit = region rc {
        discard Array#{Array#{1, 2} @ rc, Array#{1, 2} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit36(): Unit = region rc {
        discard Array#{Array#{1} @ rc, Array#{1, 2, 3, 4, 5, 6, 7, 8} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit37(): Unit = region rc {
        discard Array#{Array#{1, 2, 3, 4, 5} @ rc, Array#{2} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit38(): Unit = region rc {
        discard Array#{Array#{1, 2, 3} @ rc, Array#{1, 2, 3} @ rc, Array#{1, 2, 3} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit39(): Unit = region rc {
        discard Array#{Array#{true, false} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit40(): Unit = region rc {
        discard Array#{Array#{true, false} @ rc, Array#{true, false} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit41(): Unit = region rc {
        discard Array#{Array#{true} @ rc, Array#{true, true, false, false} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit42(): Unit = region rc {
        discard Array#{Array#{true, false, true, false, true, false} @ rc, Array#{true} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit43(): Unit = region rc {
        discard Array#{Array#{true, true, true} @ rc, Array#{false, false, false} @ rc, Array#{false, true, false} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit44(): Unit = region rc {
        discard Array#{Array#{'a', 'b', 'c'} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit45(): Unit = region rc {
        discard Array#{Array#{'a', 'b'} @ rc, Array#{'a', 'b'} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit46(): Unit = region rc {
        discard Array#{Array#{'a'} @ rc, Array#{'a', 'b', 'c', 'd', 'e'} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit47(): Unit = region rc {
        discard Array#{Array#{'a', 'b', 'c', 'd', 'e'} @ rc, Array#{'a'} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit48(): Unit = region rc {
        discard Array#{Array#{'a', 'b', 'c'} @ rc, Array#{'a', 'b', 'c'} @ rc, Array#{'a', 'b', 'c'} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit49(): Unit = region rc {
        discard Array#{Array#{1i8, 2i8, 3i8} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit50(): Unit = region rc {
        discard Array#{Array#{1i8, 2i8} @ rc, Array#{1i8, 2i8} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit51(): Unit = region rc {
        discard Array#{Array#{1i8} @ rc, Array#{1i8, 2i8, 3i8, 4i8, 5i8, 6i8} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit52(): Unit = region rc {
        discard Array#{Array#{1i8, 2i8, 3i8, 4i8, 5i8, 6i8} @ rc, Array#{1i8} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit53(): Unit = region rc {
        discard Array#{Array#{1i8, 2i8, 3i8} @ rc, Array#{1i8, 2i8, 3i8} @ rc, Array#{1i8, 2i8, 3i8} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit54(): Unit = region rc {
        discard Array#{Array#{1i16, 2i16, 3i16} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit55(): Unit = region rc {
        discard Array#{Array#{1i16, 2i16} @ rc, Array#{1i16, 2i16} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit56(): Unit = region rc {
        discard Array#{Array#{1i16} @ rc, Array#{1i16, 2i16, 3i16, 4i16, 5i16, 6i16} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit57(): Unit = region rc {
        discard Array#{Array#{1i16, 2i16, 3i16, 4i16, 5i16, 6i16} @ rc, Array#{1i16} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit58(): Unit = region rc {
        discard Array#{Array#{1i16, 2i16, 3i16} @ rc, Array#{1i16, 2i16, 3i16} @ rc, Array#{1i16, 2i16, 3i16} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit59(): Unit = region rc {
        discard Array#{Array#{1i64, 2i64, 3i64} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit60(): Unit = region rc {
        discard Array#{Array#{1i64, 2i64} @ rc, Array#{1i64, 2i64} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit61(): Unit = region rc {
        discard Array#{Array#{1i64} @ rc, Array#{1i64, 2i64, 3i64, 4i64, 5i64, 6i64} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit62(): Unit = region rc {
        discard Array#{Array#{1i64, 2i64, 3i64, 4i64, 5i64, 6i64} @ rc, Array#{1i64} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit63(): Unit = region rc {
        discard Array#{Array#{1i64, 2i64, 3i64} @ rc, Array#{1i64, 2i64, 3i64} @ rc, Array#{1i64, 2i64, 3i64} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit64(): Unit = region rc {
        discard Array#{Array#{1.0f32, 2.0f32, 3.0f32} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit65(): Unit = region rc {
        discard Array#{Array#{1.0f32, 2.0f32} @ rc, Array#{1.0f32, 2.0f32} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit66(): Unit = region rc {
        discard Array#{Array#{1.0f32} @ rc, Array#{1.0f32, 2.0f32, 3.0f32, 4.0f32, 5.0f32, 6.0f32} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit67(): Unit = region rc {
        discard Array#{Array#{1.0f32, 2.0f32, 3.0f32, 4.0f32, 5.0f32, 6.0f32} @ rc, Array#{1.0f32} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit68(): Unit = region rc {
        discard Array#{Array#{1.0f64, 2.0f64, 3.0f64} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit69(): Unit = region rc {
        discard Array#{Array#{1.0f64, 2.0f64} @ rc, Array#{3.0f64, 4.0f64} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit70(): Unit = region rc {
        discard Array#{Array#{1.0f64} @ rc, Array#{1.0f64, 2.0f64, 3.0f64, 4.0f64, 5.0f64, 6.0f64} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit71(): Unit = region rc {
        discard Array#{Array#{1.0f64, 2.0f64, 3.0f64, 4.0f64, 5.0f64, 6.0f64} @ rc, Array#{1.0f64} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit72(): Unit = region rc {
        discard Array#{Array#{1.0f64, 2.0f64, 3.0f64} @ rc, Array#{1.0f64, 2.0f64, 3.0f64} @ rc, Array#{1.0f64, 2.0f64, 3.0f64} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit73(): Unit = region rc {
        discard Array#{Array#{"Hello"} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit74(): Unit = region rc {
        discard Array#{Array#{"Hello"} @ rc, Array#{"World"} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit75(): Unit = region rc {
        discard Array#{Array#{"Hello"} @ rc, Array#{"Hello", "World", "World", "Hello"} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit76(): Unit = region rc {
        discard Array#{Array#{"Hello", "World", "World", "Hello"} @ rc, Array#{"Hello"} @ rc} @ rc;
        ()
    }

    @test
    def testArrayLit77(): Unit = region rc {
        discard Array#{(12, 42)} @ rc;
        ()
    }

    @test
    def testArrayLit78(): Unit = region rc {
        discard Array#{(12, 42, 34), (12, 42, 34)} @ rc;
        ()
    }

    @test
    def testArrayLit79(): Unit = region rc {
        discard Array#{(12, 42, 12), (12, 23, 12), (12,12,12)} @ rc;
        ()
    }

    @test
    def testArrayLit80(): Unit = region rc {
        discard Array#{(12, 42, 12, 12), (12, 23, 12, 12), (12,12,12, 31)} @ rc;
        ()
    }

    @test
    def testArrayLit81(): Unit = region rc {
        discard Array#{(1,1), (1,1), (2,2), (3,3), (1,1), (2,1), (5,5)} @ rc;
        ()
    }

    @test
    def testArrayLit82(): Unit = region rc {
        discard Array#{(true, true)} @ rc;
        ()
    }

    @test
    def testArrayLit83(): Unit = region rc {
        discard Array#{(true, true, true), (true, true, true)} @ rc;
        ()
    }

    @test
    def testArrayLit84(): Unit = region rc {
        discard Array#{(true, true, true), (true, false, false), (true, true, false)} @ rc;
        ()
    }

    @test
    def testArrayLit85(): Unit = region rc {
        discard Array#{(Nil)} @ rc;
        ()
    }

    @test
    def testArrayLit86(): Unit = region rc {
        discard Array#{(1 :: Nil)} @ rc;
        ()
    }

    @test
    def testArrayLit88(): Unit = region rc {
        discard Array#{Some("Hello World")} @ rc;
        ()
    }

    @test
    def testArrayLit89(): Unit = region rc {
        discard Array#{Some(1), Some(2), None} @ rc;
        ()
    }
}

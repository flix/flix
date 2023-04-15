mod Test.Exp.ArrayLit {

    @test
    def testArrayLit01(): Unit = region r {
        discard Array#{} @ r;
        ()
    }

    @test
    def testArrayLit02(): Unit = region r {
        discard Array#{1} @ r;
        ()
    }

    @test
    def testArrayLit04(): Unit = region r {
        discard Array#{1, 2, 3, 4} @ r;
        ()
    }

    @test
    def testArrayLit05(): Unit = region r {
        discard Array#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20} @ r;
        ()
    }

    @test
    def testArrayLit06(): Unit = region r {
        discard Array#{1, 2} @ r;
        ()
    }

    @test
    def testArrayLit07(): Unit = region r {
        discard Array#{true} @ r;
        ()
    }

    @test
    def testArrayLit08(): Unit = region r {
        discard Array#{false} @ r;
        ()
    }

    @test
    def testArrayLit09(): Unit = region r {
        discard Array#{true, false} @ r;
        ()
    }

    @test
    def testArrayLit10(): Unit = region r {
        discard Array#{false, true, false} @ r;
        ()
    }

    @test
    def testArrayLit11(): Unit = region r {
        discard Array#{false, false, false, false, true} @ r;
        ()
    }

    @test
    def testArrayLit12(): Unit = region r {
        discard Array#{'c'} @ r;
        ()
    }

    @test
    def testArrayLit13(): Unit = region r {
        discard Array#{'a', 'b'} @ r;
        ()
    }

    @test
    def testArrayLit14(): Unit = region r {
        discard Array#{'a', 'b', 'c'} @ r;
        ()
    }

    @test
    def testArrayLit15(): Unit = region r {
        discard Array#{'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n'} @ r;
        ()
    }

    @test
    def testArrayLit16(): Unit = region r {
        discard Array#{1i8} @ r;
        ()
    }

    @test
    def testArrayLit17(): Unit = region r {
        discard Array#{1i8, 2i8, 3i8} @ r;
        ()
    }

    @test
    def testArrayLit18(): Unit = region r {
        discard Array#{1i8, 2i8, 3i8, 4i8} @ r;
        ()
    }

    @test
    def testArrayLit19(): Unit = region r {
        discard Array#{1i16} @ r;
        ()
    }

    @test
    def testArrayLit20(): Unit = region r {
        discard Array#{1i16, 2i16, 3i16} @ r;
        ()
    }

    @test
    def testArrayLit21(): Unit = region r {
        discard Array#{1i16, 2i16, 3i16, 4i16} @ r;
        ()
    }

    @test
    def testArrayLit22(): Unit = region r {
        discard Array#{1i64} @ r;
        ()
    }

    @test
    def testArrayLit23(): Unit = region r {
        discard Array#{1i64, 2i64, 3i64} @ r;
        ()
    }

    @test
    def testArrayLit24(): Unit = region r {
        discard Array#{1i64, 2i64, 3i64, 4i64} @ r;
        ()
    }

    @test
    def testArrayLit25(): Unit = region r {
        discard Array#{1.0f32} @ r;
        ()
    }

    @test
    def testArrayLit26(): Unit = region r {
        discard Array#{1.0f32, 2.0f32, 3.0f32} @ r;
        ()
    }

    @test
    def testArrayLit27(): Unit = region r {
        discard Array#{1.0f32, 2.0f32, 3.0f32, 4.0f32} @ r;
        ()
    }

    @test
    def testArrayLit28(): Unit = region r {
        discard Array#{1.0f64} @ r;
        ()
    }

    @test
    def testArrayLit29(): Unit = region r {
        discard Array#{1.0f64, 2.0f64, 3.0f64} @ r;
        ()
    }

    @test
    def testArrayLit30(): Unit = region r {
        discard Array#{1.0f64, 2.0f64, 3.0f64, 4.0f64} @ r;
        ()
    }

    @test
    def testArrayLit31(): Unit = region r {
        discard Array#{"Hello"} @ r;
        ()
    }

    @test
    def testArrayLit32(): Unit = region r {
        discard Array#{"Hello", "World"} @ r;
        ()
    }

    @test
    def testArrayLit33(): Unit = region r {
        discard Array#{"World", "Hello", "World"} @ r;
        ()
    }

    @test
    def testArrayLit34(): Unit = region r {
        discard Array#{Array#{1,2,3} @ r} @ r;
        ()
    }

    @test
    def testArrayLit35(): Unit = region r {
        discard Array#{Array#{1, 2} @ r, Array#{1, 2} @ r} @ r;
        ()
    }

    @test
    def testArrayLit36(): Unit = region r {
        discard Array#{Array#{1} @ r, Array#{1, 2, 3, 4, 5, 6, 7, 8} @ r} @ r;
        ()
    }

    @test
    def testArrayLit37(): Unit = region r {
        discard Array#{Array#{1, 2, 3, 4, 5} @ r, Array#{2} @ r} @ r;
        ()
    }

    @test
    def testArrayLit38(): Unit = region r {
        discard Array#{Array#{1, 2, 3} @ r, Array#{1, 2, 3} @ r, Array#{1, 2, 3} @ r} @ r;
        ()
    }

    @test
    def testArrayLit39(): Unit = region r {
        discard Array#{Array#{true, false} @ r} @ r;
        ()
    }

    @test
    def testArrayLit40(): Unit = region r {
        discard Array#{Array#{true, false} @ r, Array#{true, false} @ r} @ r;
        ()
    }

    @test
    def testArrayLit41(): Unit = region r {
        discard Array#{Array#{true} @ r, Array#{true, true, false, false} @ r} @ r;
        ()
    }

    @test
    def testArrayLit42(): Unit = region r {
        discard Array#{Array#{true, false, true, false, true, false} @ r, Array#{true} @ r} @ r;
        ()
    }

    @test
    def testArrayLit43(): Unit = region r {
        discard Array#{Array#{true, true, true} @ r, Array#{false, false, false} @ r, Array#{false, true, false} @ r} @ r;
        ()
    }

    @test
    def testArrayLit44(): Unit = region r {
        discard Array#{Array#{'a', 'b', 'c'} @ r} @ r;
        ()
    }

    @test
    def testArrayLit45(): Unit = region r {
        discard Array#{Array#{'a', 'b'} @ r, Array#{'a', 'b'} @ r} @ r;
        ()
    }

    @test
    def testArrayLit46(): Unit = region r {
        discard Array#{Array#{'a'} @ r, Array#{'a', 'b', 'c', 'd', 'e'} @ r} @ r;
        ()
    }

    @test
    def testArrayLit47(): Unit = region r {
        discard Array#{Array#{'a', 'b', 'c', 'd', 'e'} @ r, Array#{'a'} @ r} @ r;
        ()
    }

    @test
    def testArrayLit48(): Unit = region r {
        discard Array#{Array#{'a', 'b', 'c'} @ r, Array#{'a', 'b', 'c'} @ r, Array#{'a', 'b', 'c'} @ r} @ r;
        ()
    }

    @test
    def testArrayLit49(): Unit = region r {
        discard Array#{Array#{1i8, 2i8, 3i8} @ r} @ r;
        ()
    }

    @test
    def testArrayLit50(): Unit = region r {
        discard Array#{Array#{1i8, 2i8} @ r, Array#{1i8, 2i8} @ r} @ r;
        ()
    }

    @test
    def testArrayLit51(): Unit = region r {
        discard Array#{Array#{1i8} @ r, Array#{1i8, 2i8, 3i8, 4i8, 5i8, 6i8} @ r} @ r;
        ()
    }

    @test
    def testArrayLit52(): Unit = region r {
        discard Array#{Array#{1i8, 2i8, 3i8, 4i8, 5i8, 6i8} @ r, Array#{1i8} @ r} @ r;
        ()
    }

    @test
    def testArrayLit53(): Unit = region r {
        discard Array#{Array#{1i8, 2i8, 3i8} @ r, Array#{1i8, 2i8, 3i8} @ r, Array#{1i8, 2i8, 3i8} @ r} @ r;
        ()
    }

    @test
    def testArrayLit54(): Unit = region r {
        discard Array#{Array#{1i16, 2i16, 3i16} @ r} @ r;
        ()
    }

    @test
    def testArrayLit55(): Unit = region r {
        discard Array#{Array#{1i16, 2i16} @ r, Array#{1i16, 2i16} @ r} @ r;
        ()
    }

    @test
    def testArrayLit56(): Unit = region r {
        discard Array#{Array#{1i16} @ r, Array#{1i16, 2i16, 3i16, 4i16, 5i16, 6i16} @ r} @ r;
        ()
    }

    @test
    def testArrayLit57(): Unit = region r {
        discard Array#{Array#{1i16, 2i16, 3i16, 4i16, 5i16, 6i16} @ r, Array#{1i16} @ r} @ r;
        ()
    }

    @test
    def testArrayLit58(): Unit = region r {
        discard Array#{Array#{1i16, 2i16, 3i16} @ r, Array#{1i16, 2i16, 3i16} @ r, Array#{1i16, 2i16, 3i16} @ r} @ r;
        ()
    }

    @test
    def testArrayLit59(): Unit = region r {
        discard Array#{Array#{1i64, 2i64, 3i64} @ r} @ r;
        ()
    }

    @test
    def testArrayLit60(): Unit = region r {
        discard Array#{Array#{1i64, 2i64} @ r, Array#{1i64, 2i64} @ r} @ r;
        ()
    }

    @test
    def testArrayLit61(): Unit = region r {
        discard Array#{Array#{1i64} @ r, Array#{1i64, 2i64, 3i64, 4i64, 5i64, 6i64} @ r} @ r;
        ()
    }

    @test
    def testArrayLit62(): Unit = region r {
        discard Array#{Array#{1i64, 2i64, 3i64, 4i64, 5i64, 6i64} @ r, Array#{1i64} @ r} @ r;
        ()
    }

    @test
    def testArrayLit63(): Unit = region r {
        discard Array#{Array#{1i64, 2i64, 3i64} @ r, Array#{1i64, 2i64, 3i64} @ r, Array#{1i64, 2i64, 3i64} @ r} @ r;
        ()
    }

    @test
    def testArrayLit64(): Unit = region r {
        discard Array#{Array#{1.0f32, 2.0f32, 3.0f32} @ r} @ r;
        ()
    }

    @test
    def testArrayLit65(): Unit = region r {
        discard Array#{Array#{1.0f32, 2.0f32} @ r, Array#{1.0f32, 2.0f32} @ r} @ r;
        ()
    }

    @test
    def testArrayLit66(): Unit = region r {
        discard Array#{Array#{1.0f32} @ r, Array#{1.0f32, 2.0f32, 3.0f32, 4.0f32, 5.0f32, 6.0f32} @ r} @ r;
        ()
    }

    @test
    def testArrayLit67(): Unit = region r {
        discard Array#{Array#{1.0f32, 2.0f32, 3.0f32, 4.0f32, 5.0f32, 6.0f32} @ r, Array#{1.0f32} @ r} @ r;
        ()
    }

    @test
    def testArrayLit68(): Unit = region r {
        discard Array#{Array#{1.0f64, 2.0f64, 3.0f64} @ r} @ r;
        ()
    }

    @test
    def testArrayLit69(): Unit = region r {
        discard Array#{Array#{1.0f64, 2.0f64} @ r, Array#{3.0f64, 4.0f64} @ r} @ r;
        ()
    }

    @test
    def testArrayLit70(): Unit = region r {
        discard Array#{Array#{1.0f64} @ r, Array#{1.0f64, 2.0f64, 3.0f64, 4.0f64, 5.0f64, 6.0f64} @ r} @ r;
        ()
    }

    @test
    def testArrayLit71(): Unit = region r {
        discard Array#{Array#{1.0f64, 2.0f64, 3.0f64, 4.0f64, 5.0f64, 6.0f64} @ r, Array#{1.0f64} @ r} @ r;
        ()
    }

    @test
    def testArrayLit72(): Unit = region r {
        discard Array#{Array#{1.0f64, 2.0f64, 3.0f64} @ r, Array#{1.0f64, 2.0f64, 3.0f64} @ r, Array#{1.0f64, 2.0f64, 3.0f64} @ r} @ r;
        ()
    }

    @test
    def testArrayLit73(): Unit = region r {
        discard Array#{Array#{"Hello"} @ r} @ r;
        ()
    }

    @test
    def testArrayLit74(): Unit = region r {
        discard Array#{Array#{"Hello"} @ r, Array#{"World"} @ r} @ r;
        ()
    }

    @test
    def testArrayLit75(): Unit = region r {
        discard Array#{Array#{"Hello"} @ r, Array#{"Hello", "World", "World", "Hello"} @ r} @ r;
        ()
    }

    @test
    def testArrayLit76(): Unit = region r {
        discard Array#{Array#{"Hello", "World", "World", "Hello"} @ r, Array#{"Hello"} @ r} @ r;
        ()
    }

    @test
    def testArrayLit77(): Unit = region r {
        discard Array#{(12, 42)} @ r;
        ()
    }

    @test
    def testArrayLit78(): Unit = region r {
        discard Array#{(12, 42, 34), (12, 42, 34)} @ r;
        ()
    }

    @test
    def testArrayLit79(): Unit = region r {
        discard Array#{(12, 42, 12), (12, 23, 12), (12,12,12)} @ r;
        ()
    }

    @test
    def testArrayLit80(): Unit = region r {
        discard Array#{(12, 42, 12, 12), (12, 23, 12, 12), (12,12,12, 31)} @ r;
        ()
    }

    @test
    def testArrayLit81(): Unit = region r {
        discard Array#{(1,1), (1,1), (2,2), (3,3), (1,1), (2,1), (5,5)} @ r;
        ()
    }

    @test
    def testArrayLit82(): Unit = region r {
        discard Array#{(true, true)} @ r;
        ()
    }

    @test
    def testArrayLit83(): Unit = region r {
        discard Array#{(true, true, true), (true, true, true)} @ r;
        ()
    }

    @test
    def testArrayLit84(): Unit = region r {
        discard Array#{(true, true, true), (true, false, false), (true, true, false)} @ r;
        ()
    }

    @test
    def testArrayLit85(): Unit = region r {
        discard Array#{(Nil)} @ r;
        ()
    }

    @test
    def testArrayLit86(): Unit = region r {
        discard Array#{(1 :: Nil)} @ r;
        ()
    }

    @test
    def testArrayLit88(): Unit = region r {
        discard Array#{Some("Hello World")} @ r;
        ()
    }

    @test
    def testArrayLit89(): Unit = region r {
        discard Array#{Some(1), Some(2), None} @ r;
        ()
    }
}

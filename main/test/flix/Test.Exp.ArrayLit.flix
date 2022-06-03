namespace Test/Exp/ArrayLit {

    @test
    def testArrayLit01(): Unit = region r {
        discard [];
        ()
    }

    @test
    def testArrayLit02(): Unit = region r {
        discard [1];
        ()
    }

    @test
    def testArrayLit04(): Unit = region r {
        discard [1, 2, 3, 4];
        ()
    }

    @test
    def testArrayLit05(): Unit = region r {
        discard [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20];
        ()
    }

    @test
    def testArrayLit06(): Unit = region r {
        discard [1, 2];
        ()
    }

    @test
    def testArrayLit07(): Unit = region r {
        discard [true];
        ()
    }

    @test
    def testArrayLit08(): Unit = region r {
        discard [false];
        ()
    }

    @test
    def testArrayLit09(): Unit = region r {
        discard [true, false];
        ()
    }

    @test
    def testArrayLit10(): Unit = region r {
        discard [false, true, false];
        ()
    }

    @test
    def testArrayLit11(): Unit = region r {
        discard [false, false, false, false, true];
        ()
    }

    @test
    def testArrayLit12(): Unit = region r {
        discard ['c'];
        ()
    }

    @test
    def testArrayLit13(): Unit = region r {
        discard ['a', 'b'];
        ()
    }

    @test
    def testArrayLit14(): Unit = region r {
        discard ['a', 'b', 'c'];
        ()
    }

    @test
    def testArrayLit15(): Unit = region r {
        discard ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n'];
        ()
    }

    @test
    def testArrayLit16(): Unit = region r {
        discard [1i8];
        ()
    }

    @test
    def testArrayLit17(): Unit = region r {
        discard [1i8, 2i8, 3i8];
        ()
    }

    @test
    def testArrayLit18(): Unit = region r {
        discard [1i8, 2i8, 3i8, 4i8];
        ()
    }

    @test
    def testArrayLit19(): Unit = region r {
        discard [1i16];
        ()
    }

    @test
    def testArrayLit20(): Unit = region r {
        let _ = [1i16, 2i16, 3i16];
        ()
    }

    @test
    def testArrayLit21(): Unit = region r {
        let _ = [1i16, 2i16, 3i16, 4i16];
        ()
    }

    @test
    def testArrayLit22(): Unit = region r {
        let _ = [1i64];
        ()
    }

    @test
    def testArrayLit23(): Unit = region r {
        let _ = [1i64, 2i64, 3i64];
        ()
    }

    @test
    def testArrayLit24(): Unit = region r {
        let _ = [1i64, 2i64, 3i64, 4i64];
        ()
    }

    @test
    def testArrayLit25(): Unit = region r {
        let _ = [1.0f32];
        ()
    }

    @test
    def testArrayLit26(): Unit = region r {
        let _ = [1.0f32, 2.0f32, 3.0f32];
        ()
    }

    @test
    def testArrayLit27(): Unit = region r {
        let _ = [1.0f32, 2.0f32, 3.0f32, 4.0f32];
        ()
    }

    @test
    def testArrayLit28(): Unit = region r {
        let _ = [1.0f64];
        ()
    }

    @test
    def testArrayLit29(): Unit = region r {
        let _ = [1.0f64, 2.0f64, 3.0f64];
        ()
    }

    @test
    def testArrayLit30(): Unit = region r {
        let _ = [1.0f64, 2.0f64, 3.0f64, 4.0f64];
        ()
    }

    @test
    def testArrayLit31(): Unit = region r {
        let _ = ["Hello"];
        ()
    }

    @test
    def testArrayLit32(): Unit = region r {
        let _ = ["Hello", "World"];
        ()
    }

    @test
    def testArrayLit33(): Unit = region r {
        let _ = ["World", "Hello", "World"];
        ()
    }

    @test
    def testArrayLit34(): Unit = region r {
        let _ = [[1,2,3]];
        ()
    }

    @test
    def testArrayLit35(): Unit = region r {
        let _ = [[1, 2], [1, 2]];
        ()
    }

    @test
    def testArrayLit36(): Unit = region r {
        let _ = [[1], [1, 2, 3, 4, 5, 6, 7, 8]];
        ()
    }

    @test
    def testArrayLit37(): Unit = region r {
        let _ = [[1, 2, 3, 4, 5], [2]];
        ()
    }

    @test
    def testArrayLit38(): Unit = region r {
        let _ = [[1, 2, 3], [1, 2, 3], [1, 2, 3]];
        ()
    }

    @test
    def testArrayLit39(): Unit = region r {
        let _ = [[true, false]];
        ()
    }

    @test
    def testArrayLit40(): Unit = region r {
        let _ = [[true, false], [true, false]];
        ()
    }

    @test
    def testArrayLit41(): Unit = region r {
        let _ = [[true], [true, true, false, false]];
        ()
    }

    @test
    def testArrayLit42(): Unit = region r {
        let _ = [[true, false, true, false, true, false], [true]];
        ()
    }

    @test
    def testArrayLit43(): Unit = region r {
        let _ = [[true, true, true], [false, false, false], [false, true, false]];
        ()
    }

    @test
    def testArrayLit44(): Unit = region r {
        let _ = [['a', 'b', 'c']];
        ()
    }

    @test
    def testArrayLit45(): Unit = region r {
        let _ = [['a', 'b'], ['a', 'b']];
        ()
    }

    @test
    def testArrayLit46(): Unit = region r {
        let _ = [['a'], ['a', 'b', 'c', 'd', 'e']];
        ()
    }

    @test
    def testArrayLit47(): Unit = region r {
        let _ = [['a', 'b', 'c', 'd', 'e'], ['a']];
        ()
    }

    @test
    def testArrayLit48(): Unit = region r {
        let _ = [['a', 'b', 'c'], ['a', 'b', 'c'], ['a', 'b', 'c']];
        ()
    }

    @test
    def testArrayLit49(): Unit = region r {
        let _ = [[1i8, 2i8, 3i8]];
        ()
    }

    @test
    def testArrayLit50(): Unit = region r {
        let _ = [[1i8, 2i8], [1i8, 2i8]];
        ()
    }

    @test
    def testArrayLit51(): Unit = region r {
        let _ = [[1i8], [1i8, 2i8, 3i8, 4i8, 5i8, 6i8]];
        ()
    }

    @test
    def testArrayLit52(): Unit = region r {
        let _ = [[1i8, 2i8, 3i8, 4i8, 5i8, 6i8], [1i8]];
        ()
    }

    @test
    def testArrayLit53(): Unit = region r {
        let _ = [[1i8, 2i8, 3i8], [1i8, 2i8, 3i8], [1i8, 2i8, 3i8]];
        ()
    }

    @test
    def testArrayLit54(): Unit = region r {
        let _ = [[1i16, 2i16, 3i16]];
        ()
    }

    @test
    def testArrayLit55(): Unit = region r {
        let _ = [[1i16, 2i16], [1i16, 2i16]];
        ()
    }

    @test
    def testArrayLit56(): Unit = region r {
        let _ = [[1i16], [1i16, 2i16, 3i16, 4i16, 5i16, 6i16]];
        ()
    }

    @test
    def testArrayLit57(): Unit = region r {
        let _ = [[1i16, 2i16, 3i16, 4i16, 5i16, 6i16], [1i16]];
        ()
    }

    @test
    def testArrayLit58(): Unit = region r {
        let _ = [[1i16, 2i16, 3i16], [1i16, 2i16, 3i16], [1i16, 2i16, 3i16]];
        ()
    }

    @test
    def testArrayLit59(): Unit = region r {
        let _ = [[1i64, 2i64, 3i64]];
        ()
    }

    @test
    def testArrayLit60(): Unit = region r {
        let _ = [[1i64, 2i64], [1i64, 2i64]];
        ()
    }

    @test
    def testArrayLit61(): Unit = region r {
        let _ = [[1i64], [1i64, 2i64, 3i64, 4i64, 5i64, 6i64]];
        ()
    }

    @test
    def testArrayLit62(): Unit = region r {
        let _ = [[1i64, 2i64, 3i64, 4i64, 5i64, 6i64], [1i64]];
        ()
    }

    @test
    def testArrayLit63(): Unit = region r {
        let _ = [[1i64, 2i64, 3i64], [1i64, 2i64, 3i64], [1i64, 2i64, 3i64]];
        ()
    }

    @test
    def testArrayLit64(): Unit = region r {
        let _ = [[1.0f32, 2.0f32, 3.0f32]];
        ()
    }

    @test
    def testArrayLit65(): Unit = region r {
        let _ = [[1.0f32, 2.0f32], [1.0f32, 2.0f32]];
        ()
    }

    @test
    def testArrayLit66(): Unit = region r {
        let _ = [[1.0f32], [1.0f32, 2.0f32, 3.0f32, 4.0f32, 5.0f32, 6.0f32]];
        ()
    }

    @test
    def testArrayLit67(): Unit = region r {
        let _ = [[1.0f32, 2.0f32, 3.0f32, 4.0f32, 5.0f32, 6.0f32], [1.0f32]];
        ()
    }

    @test
    def testArrayLit68(): Unit = region r {
        let _ = [[1.0f64, 2.0f64, 3.0f64]];
        ()
    }

    @test
    def testArrayLit69(): Unit = region r {
        let _ = [[1.0f64, 2.0f64], [3.0f64, 4.0f64]];
        ()
    }

    @test
    def testArrayLit70(): Unit = region r {
        let _ = [[1.0f64], [1.0f64, 2.0f64, 3.0f64, 4.0f64, 5.0f64, 6.0f64]];
        ()
    }

    @test
    def testArrayLit71(): Unit = region r {
        let _ = [[1.0f64, 2.0f64, 3.0f64, 4.0f64, 5.0f64, 6.0f64], [1.0f64]];
        ()
    }

    @test
    def testArrayLit72(): Unit = region r {
        let _ = [[1.0f64, 2.0f64, 3.0f64], [1.0f64, 2.0f64, 3.0f64], [1.0f64, 2.0f64, 3.0f64]];
        ()
    }

    @test
    def testArrayLit73(): Unit = region r {
        let _ = [["Hello"]];
        ()
    }

    @test
    def testArrayLit74(): Unit = region r {
        let _ = [["Hello"], ["World"]];
        ()
    }

    @test
    def testArrayLit75(): Unit = region r {
        let _ = [["Hello"], ["Hello", "World", "World", "Hello"]];
        ()
    }

    @test
    def testArrayLit76(): Unit = region r {
        let _ = [["Hello", "World", "World", "Hello"], ["Hello"]];
        ()
    }

    @test
    def testArrayLit77(): Unit = region r {
        let _ = [(12, 42)];
        ()
    }

    @test
    def testArrayLit78(): Unit = region r {
        let _ = [(12, 42, 34), (12, 42, 34)];
        ()
    }

    @test
    def testArrayLit79(): Unit = region r {
        let _ = [(12, 42, 12), (12, 23, 12), (12,12,12)];
        ()
    }

    @test
    def testArrayLit80(): Unit = region r {
        let _ = [(12, 42, 12, 12), (12, 23, 12, 12), (12,12,12, 31)];
        ()
    }

    @test
    def testArrayLit81(): Unit = region r {
        let _ = [(1,1), (1,1), (2,2), (3,3), (1,1), (2,1), (5,5)];
        ()
    }

    @test
    def testArrayLit82(): Unit = region r {
        let _ = [(true, true)];
        ()
    }

    @test
    def testArrayLit83(): Unit = region r {
        let _ = [(true, true, true), (true, true, true)];
        ()
    }

    @test
    def testArrayLit84(): Unit = region r {
        let _ = [(true, true, true), (true, false, false), (true, true, false)];
        ()
    }

    @test
    def testArrayLit85(): Unit = region r {
        let _ = [(Nil)];
        ()
    }

    @test
    def testArrayLit86(): Unit = region r {
        let _ = [(1 :: Nil)];
        ()
    }

    @test
    def testArrayLit88(): Unit = region r {
        let _ = [Some("Hello World")];
        ()
    }

    @test
    def testArrayLit89(): Unit = region r {
        let _ = [Some(1), Some(2), None];
        ()
    }
}

mod Test.Exp.ArrayLoad {

    @test
    def testArrayLoad01(): Int32 = region r {
        let x = Array#{1} @ r;
        $ARRAY_LOAD$(x, 0)
    }

    @test
    def testArrayLoad02(): Int32 = region r {
        let x = Array#{1, 2, 3} @ r;
        $ARRAY_LOAD$(x, 2)
    }

    @test
    def testArrayLoad03(): Int32 = region r {
        let x = Array#{1, 2, 3, 4} @ r;
        $ARRAY_LOAD$(x, 2)
    }

    @test
    def testArrayLoad04(): Int32 = region r {
        let x = Array#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20} @ r;
        $ARRAY_LOAD$(x, 10)
    }

    @test
    def testArrayLoad05(): Int32 = region r {
        let x = Array#{1, 2} @ r;
        $ARRAY_LOAD$(x, 1)
    }

    @test
    def testArrayLoad06(): Bool = region r {
        let x = Array#{true} @ r;
        $ARRAY_LOAD$(x, 0)
    }

    @test
    def testArrayLoad07(): Bool = region r {
        let x = Array#{false} @ r;
        not $ARRAY_LOAD$(x, 0)
    }

    @test
    def testArrayLoad08(): Bool = region r {
        let x = Array#{true, false} @ r;
        not $ARRAY_LOAD$(x, 1)
    }

    @test
    def testArrayLoad9(): Bool = region r {
        let x = Array#{false, true, false} @ r;
        not $ARRAY_LOAD$(x, 2)
    }

    @test
    def testArrayLoad10(): Bool = region r {
        let x = Array#{false, false, false, false, true} @ r;
        not $ARRAY_LOAD$(x, 3)
    }

    @test
    def testArrayLoad11(): Char = region r {
        let x = Array#{'c'} @ r;
        $ARRAY_LOAD$(x, 0)
    }

    @test
    def testArrayLoad12(): Char = region r {
        let x = Array#{'a', 'b'} @ r;
        $ARRAY_LOAD$(x, 0)
    }

    @test
    def testArrayLoad13(): Char = region r {
        let x = Array#{'a', 'b', 'c'} @ r;
        $ARRAY_LOAD$(x, 1)
    }

    @test
    def testArrayLoad14(): Char = region r {
        let x = Array#{'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n'} @ r;
        $ARRAY_LOAD$(x, 9)
    }

    @test
    def testArrayLoad15(): Int8 = region r {
        let x = Array#{1i8} @ r;
        $ARRAY_LOAD$(x, 0)
    }

    @test
    def testArrayLoad16(): Int8 = region r {
        let x = Array#{1i8, 2i8, 3i8} @ r;
        $ARRAY_LOAD$(x, 1)
    }

    @test
    def testArrayLoad17(): Int8 = region r {
        let x = Array#{1i8, 2i8, 3i8, 4i8} @ r;
        $ARRAY_LOAD$(x, 2)
    }

    @test
    def testArrayLoad18(): Int16 = region r {
        let x = Array#{1i16} @ r;
        $ARRAY_LOAD$(x, 0)
    }

    @test
    def testArrayLoad19(): Int16 = region r {
        let x = Array#{1i16, 2i16, 3i16} @ r;
        $ARRAY_LOAD$(x, 1)
    }

    @test
    def testArrayLoad20(): Int16 = region r {
        let x = Array#{1i16, 2i16, 3i16, 4i16} @ r;
        $ARRAY_LOAD$(x, 2)
    }

    @test
    def testArrayLoad21(): Int64 = region r {
        let x = Array#{1i64} @ r;
        $ARRAY_LOAD$(x, 0)
    }

    @test
    def testArrayLoad22(): Int64 = region r {
        let x = Array#{1i64, 2i64, 3i64} @ r;
        $ARRAY_LOAD$(x, 1)
    }

    @test
    def testArrayLoad23(): Int64 = region r {
        let x = Array#{1i64, 2i64, 3i64, 4i64} @ r;
        $ARRAY_LOAD$(x, 2)
    }

    @test
    def testArrayLoad24(): Float32 = region r {
        let x = Array#{1.0f32} @ r;
        $ARRAY_LOAD$(x, 0)
    }

    @test
    def testArrayLoad25(): Float32 = region r {
        let x = Array#{1.0f32, 2.0f32, 3.0f32} @ r;
        $ARRAY_LOAD$(x, 1)
    }

    @test
    def testArrayLoad26(): Float32 = region r {
        let x = Array#{1.0f32, 2.0f32, 3.0f32, 4.0f32} @ r;
        $ARRAY_LOAD$(x, 2)
    }

    @test
    def testArrayLoad27(): Float64 = region r {
        let x = Array#{1.0f64} @ r;
        $ARRAY_LOAD$(x, 0)
    }

    @test
    def testArrayLoad28(): Float64 = region r {
        let x = Array#{1.0f64, 2.0f64, 3.0f64} @ r;
        $ARRAY_LOAD$(x, 1)
    }

    @test
    def testArrayLoad29(): Float64 = region r {
        let x = Array#{1.0f64, 2.0f64, 3.0f64, 4.0f64} @ r;
        $ARRAY_LOAD$(x, 3)
    }

    @test
    def testArrayLoad30(): String = region r {
        let x = Array#{"Hello"} @ r;
        $ARRAY_LOAD$(x, 0)
    }

    @test
    def testArrayLoad31(): String = region r {
        let x = Array#{"Hello", "World"} @ r;
        $ARRAY_LOAD$(x, 1)
    }

    @test
    def testArrayLoad32(): String = region r {
        let x = Array#{"World", "Hello", "World"} @ r;
        $ARRAY_LOAD$(x, 2)
    }

    @test
    def testArrayLoad33(): Int32 = region r {
        let x = Array#{Array#{1, 2, 3} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 0), 1)
    }

    @test
    def testArrayLoad34(): Int32 = region r {
        let x = Array#{Array#{1, 2} @ r, Array#{1, 2} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 0)
    }

    @test
    def testArrayLoad35(): Int32 = region r {
        let x = Array#{Array#{1} @ r, Array#{1, 2, 3, 4, 5, 6, 7, 8} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 5)
    }

    @test
    def testArrayLoad36(): Int32 = region r {
        let x = Array#{Array#{1, 2, 3, 4, 5} @ r, Array#{2} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 0), 3)
    }

    @test
    def testArrayLoad37(): Int32 = region r {
        let x = Array#{Array#{1, 2, 3} @ r, Array#{1, 2, 3} @ r, Array#{1, 2, 3} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 2), 2)
    }

    @test
    def testArrayLoad38(): Bool = region r {
        let x = Array#{Array#{true, false, true} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 0), 2)
    }

    @test
    def testArrayLoad39(): Bool = region r {
        let x = Array#{Array#{true, true} @ r, Array#{true, false} @ r} @ r;
        not $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 1)
    }

    @test
    def testArrayLoad40(): Bool = region r {
        let x = Array#{Array#{true} @ r, Array#{true, false, true, false} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 2)
    }

    @test
    def testArrayLoad41(): Bool = region r {
        let x = Array#{Array#{true, false, true} @ r, Array#{true, false, true} @ r, Array#{true, false, true} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 2), 2)
    }

    @test
    def testArrayLoad42(): Char = region r {
        let x = Array#{Array#{'a', 'b', 'c'} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 0), 1)
    }

    @test
    def testArrayLoad43(): Char = region r {
        let x = Array#{Array#{'a', 'b'} @ r, Array#{'a', 'b'} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 1)
    }

    @test
    def testArrayLoad44(): Char = region r {
        let x = Array#{Array#{'a'} @ r, Array#{'a', 'b', 'c', 'd', 'e'} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 1)
    }

    @test
    def testArrayLoad45(): Char = region r {
        let x = Array#{Array#{'a', 'b', 'c', 'd', 'e'} @ r, Array#{'a'} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 0), 3)
    }

    @test
    def testArrayLoad46(): Char = region r {
        let x = Array#{Array#{'a', 'b', 'c'} @ r, Array#{'a', 'b', 'c'} @ r, Array#{'a', 'b', 'c'} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 2), 1)
    }

    @test
    def testArrayLoad47(): Int8 = region r {
        let x = Array#{Array#{1i8, 2i8, 3i8} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 0), 1)
    }

    @test
    def testArrayLoad48(): Int8 = region r {
        let x = Array#{Array#{1i8, 2i8} @ r, Array#{1i8, 2i8} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 0)
    }

    @test
    def testArrayLoad49(): Int8 = region r {
        let x = Array#{Array#{1i8, 2i8, 3i8, 4i8} @ r,Array#{1i8} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 0), 3)
    }

    @test
    def testArrayLoad50(): Int8 = region r {
        let x = Array#{Array#{1i8} @ r, Array#{1i8, 2i8, 3i8, 4i8} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 3)
    }

    @test
    def testArrayLoad51(): Int8 = region r {
        let x = Array#{Array#{1i8, 2i8, 3i8} @ r, Array#{1i8, 2i8, 3i8} @ r, Array#{1i8, 2i8, 3i8} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 2), 2)
    }

    @test
    def testArrayLoad52(): Int16 = region r {
        let x = Array#{Array#{1i16, 2i16, 3i16} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 0), 1)
    }

    @test
    def testArrayLoad53(): Int16 = region r {
        let x = Array#{Array#{1i16, 2i16} @ r, Array#{1i16, 2i16} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 1)
    }

    @test
    def testArrayLoad54(): Int16 = region r {
        let x = Array#{Array#{1i16, 2i16, 3i16, 4i16} @ r, Array#{1i16} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 0), 1)
    }

    @test
    def testArrayLoad55(): Int16 = region r {
        let x = Array#{Array#{1i16} @ r, Array#{1i16, 2i16, 3i16, 4i16} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 1)
    }

    @test
    def testArrayLoad56(): Int16 = region r {
        let x = Array#{Array#{1i16, 2i16, 3i16} @ r, Array#{1i16, 2i16, 3i16} @ r, Array#{1i16, 2i16, 3i16} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 2)
    }

    @test
    def testArrayLoad57(): Int64 = region r {
        let x = Array#{Array#{1i64, 2i64, 3i64} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 0), 2)
    }

    @test
    def testArrayLoad58(): Int64 = region r {
        let x = Array#{Array#{1i64, 2i64} @ r, Array#{1i64, 2i64} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 1)
    }

    @test
    def testArrayLoad59(): Int64 = region r {
        let x = Array#{Array#{1i64} @ r, Array#{1i64, 2i64, 3i64, 4i64} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 2)
    }

    @test
    def testArrayLoad60(): Int64 = region r {
        let x = Array#{Array#{1i64, 2i64, 3i64, 4i64} @ r, Array#{1i64} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 0)
    }

    @test
    def testArrayLoad61(): Int64 = region r {
        let x = Array#{Array#{1i64, 2i64, 3i64} @ r, Array#{1i64, 2i64, 3i64} @ r, Array#{1i64, 2i64, 3i64} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 2), 0)
    }

    @test
    def testArrayLoad62(): Float32 = region r {
        let x = Array#{Array#{1.0f32, 2.0f32, 3.0f32} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 0), 0)
    }

    @test
    def testArrayLoad63(): Float32 = region r {
        let x = Array#{Array#{1.0f32, 2.0f32} @ r, Array#{1.0f32, 2.0f32} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 1)
    }

    @test
    def testArrayLoad64(): Float32 = region r {
        let x = Array#{Array#{1.0f32} @ r, Array#{1.0f32, 2.0f32, 3.0f32, 4.0f32} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 2)
    }

    @test
    def testArrayLoad65(): Float32 = region r {
        let x = Array#{Array#{1.0f32, 2.0f32, 3.0f32, 4.0f32} @ r, Array#{1.0f32} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 0), 2)
    }

    @test
    def testArrayLoad66(): Float32 = region r {
        let x = Array#{Array#{1.0f32, 2.0f32, 3.0f32} @ r, Array#{1.0f32, 2.0f32, 3.0f32} @ r, Array#{1.0f32, 2.0f32, 3.0f32} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 2), 2)
    }

    @test
    def testArrayLoad67(): Float64 = region r {
        let x = Array#{Array#{1.0f64, 2.0f64, 3.0f64} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 0), 0)
    }

    @test
    def testArrayLoad68(): Float64 = region r {
        let x = Array#{Array#{1.0f64, 2.0f64} @ r, Array#{1.0f64, 2.0f64} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 1)
    }

    @test
    def testArrayLoad69(): Float64 = region r {
        let x = Array#{Array#{1.0f64, 2.0f64, 3.0f64, 4.0f64} @ r, Array#{1.0f64} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 0), 1)
    }

    @test
    def testArrayLoad70(): Float64 = region r {
        let x = Array#{Array#{1.0f64, 2.0f64, 3.0f64} @ r, Array#{1.0f64, 2.0f64, 3.0f64} @ r, Array#{1.0f64, 2.0f64, 3.0f64} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 2), 2)
    }

    @test
    def testArrayLoad71(): String = region r {
        let x = Array#{Array#{"Hello", "Hello", "Hello"} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 0), 0)
    }

    @test
    def testArrayLoad72(): String = region r {
        let x = Array#{Array#{"Hello", "Hello"} @ r, Array#{"World", "World"} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 1)
    }

    @test
    def testArrayLoad73(): String = region r {
        let x = Array#{Array#{"Hello"} @ r, Array#{"World", "World", "World", "World"} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 3)
    }

    @test
    def testArrayLoad74(): String = region r {
        let x = Array#{Array#{"World", "World", "World", "World"} @ r, Array#{"Hello"} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 0), 3)
    }

    @test
    def testArrayLoad75(): String = region r {
        let x = Array#{Array#{"World", "World", "World"} @ r, Array#{"World", "World", "World"} @ r, Array#{"World", "World", "World"} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 2)
    }

    @test
    def testArrayLoad77(): Option[Int32] = region r {
        let x = Array#{Some(5), Some(6), Some(7)} @ r;
        $ARRAY_LOAD$(x, 1)
    }

    @test
    def testArrayLoad78(): Option[Int32] = region r {
        let x = Array#{Array#{None, Some(4), Some(999)} @ r, Array#{Some(9), Some(5), None, None} @ r} @ r;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 2)
    }

    @test
    def testArrayLoad79(): Unit = region r {
        let x = Array#{Array#{1 :: Nil} @ r, Array#{3 :: Nil} @ r} @ r;
        discard $ARRAY_LOAD$(x, 0);
        ()
    }
}

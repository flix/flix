mod Test.Exp.ArrayLoad {

    @test
    def testArrayLoad01(): Int32 = region rc {
        let x = Array#{1} @ rc;
        $ARRAY_LOAD$(x, 0)
    }

    @test
    def testArrayLoad02(): Int32 = region rc {
        let x = Array#{1, 2, 3} @ rc;
        $ARRAY_LOAD$(x, 2)
    }

    @test
    def testArrayLoad03(): Int32 = region rc {
        let x = Array#{1, 2, 3, 4} @ rc;
        $ARRAY_LOAD$(x, 2)
    }

    @test
    def testArrayLoad04(): Int32 = region rc {
        let x = Array#{1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20} @ rc;
        $ARRAY_LOAD$(x, 10)
    }

    @test
    def testArrayLoad05(): Int32 = region rc {
        let x = Array#{1, 2} @ rc;
        $ARRAY_LOAD$(x, 1)
    }

    @test
    def testArrayLoad06(): Bool = region rc {
        let x = Array#{true} @ rc;
        $ARRAY_LOAD$(x, 0)
    }

    @test
    def testArrayLoad07(): Bool = region rc {
        let x = Array#{false} @ rc;
        not $ARRAY_LOAD$(x, 0)
    }

    @test
    def testArrayLoad08(): Bool = region rc {
        let x = Array#{true, false} @ rc;
        not $ARRAY_LOAD$(x, 1)
    }

    @test
    def testArrayLoad9(): Bool = region rc {
        let x = Array#{false, true, false} @ rc;
        not $ARRAY_LOAD$(x, 2)
    }

    @test
    def testArrayLoad10(): Bool = region rc {
        let x = Array#{false, false, false, false, true} @ rc;
        not $ARRAY_LOAD$(x, 3)
    }

    @test
    def testArrayLoad11(): Char = region rc {
        let x = Array#{'c'} @ rc;
        $ARRAY_LOAD$(x, 0)
    }

    @test
    def testArrayLoad12(): Char = region rc {
        let x = Array#{'a', 'b'} @ rc;
        $ARRAY_LOAD$(x, 0)
    }

    @test
    def testArrayLoad13(): Char = region rc {
        let x = Array#{'a', 'b', 'c'} @ rc;
        $ARRAY_LOAD$(x, 1)
    }

    @test
    def testArrayLoad14(): Char = region rc {
        let x = Array#{'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n'} @ rc;
        $ARRAY_LOAD$(x, 9)
    }

    @test
    def testArrayLoad15(): Int8 = region rc {
        let x = Array#{1i8} @ rc;
        $ARRAY_LOAD$(x, 0)
    }

    @test
    def testArrayLoad16(): Int8 = region rc {
        let x = Array#{1i8, 2i8, 3i8} @ rc;
        $ARRAY_LOAD$(x, 1)
    }

    @test
    def testArrayLoad17(): Int8 = region rc {
        let x = Array#{1i8, 2i8, 3i8, 4i8} @ rc;
        $ARRAY_LOAD$(x, 2)
    }

    @test
    def testArrayLoad18(): Int16 = region rc {
        let x = Array#{1i16} @ rc;
        $ARRAY_LOAD$(x, 0)
    }

    @test
    def testArrayLoad19(): Int16 = region rc {
        let x = Array#{1i16, 2i16, 3i16} @ rc;
        $ARRAY_LOAD$(x, 1)
    }

    @test
    def testArrayLoad20(): Int16 = region rc {
        let x = Array#{1i16, 2i16, 3i16, 4i16} @ rc;
        $ARRAY_LOAD$(x, 2)
    }

    @test
    def testArrayLoad21(): Int64 = region rc {
        let x = Array#{1i64} @ rc;
        $ARRAY_LOAD$(x, 0)
    }

    @test
    def testArrayLoad22(): Int64 = region rc {
        let x = Array#{1i64, 2i64, 3i64} @ rc;
        $ARRAY_LOAD$(x, 1)
    }

    @test
    def testArrayLoad23(): Int64 = region rc {
        let x = Array#{1i64, 2i64, 3i64, 4i64} @ rc;
        $ARRAY_LOAD$(x, 2)
    }

    @test
    def testArrayLoad24(): Float32 = region rc {
        let x = Array#{1.0f32} @ rc;
        $ARRAY_LOAD$(x, 0)
    }

    @test
    def testArrayLoad25(): Float32 = region rc {
        let x = Array#{1.0f32, 2.0f32, 3.0f32} @ rc;
        $ARRAY_LOAD$(x, 1)
    }

    @test
    def testArrayLoad26(): Float32 = region rc {
        let x = Array#{1.0f32, 2.0f32, 3.0f32, 4.0f32} @ rc;
        $ARRAY_LOAD$(x, 2)
    }

    @test
    def testArrayLoad27(): Float64 = region rc {
        let x = Array#{1.0f64} @ rc;
        $ARRAY_LOAD$(x, 0)
    }

    @test
    def testArrayLoad28(): Float64 = region rc {
        let x = Array#{1.0f64, 2.0f64, 3.0f64} @ rc;
        $ARRAY_LOAD$(x, 1)
    }

    @test
    def testArrayLoad29(): Float64 = region rc {
        let x = Array#{1.0f64, 2.0f64, 3.0f64, 4.0f64} @ rc;
        $ARRAY_LOAD$(x, 3)
    }

    @test
    def testArrayLoad30(): String = region rc {
        let x = Array#{"Hello"} @ rc;
        $ARRAY_LOAD$(x, 0)
    }

    @test
    def testArrayLoad31(): String = region rc {
        let x = Array#{"Hello", "World"} @ rc;
        $ARRAY_LOAD$(x, 1)
    }

    @test
    def testArrayLoad32(): String = region rc {
        let x = Array#{"World", "Hello", "World"} @ rc;
        $ARRAY_LOAD$(x, 2)
    }

    @test
    def testArrayLoad33(): Int32 = region rc {
        let x = Array#{Array#{1, 2, 3} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 0), 1)
    }

    @test
    def testArrayLoad34(): Int32 = region rc {
        let x = Array#{Array#{1, 2} @ rc, Array#{1, 2} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 0)
    }

    @test
    def testArrayLoad35(): Int32 = region rc {
        let x = Array#{Array#{1} @ rc, Array#{1, 2, 3, 4, 5, 6, 7, 8} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 5)
    }

    @test
    def testArrayLoad36(): Int32 = region rc {
        let x = Array#{Array#{1, 2, 3, 4, 5} @ rc, Array#{2} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 0), 3)
    }

    @test
    def testArrayLoad37(): Int32 = region rc {
        let x = Array#{Array#{1, 2, 3} @ rc, Array#{1, 2, 3} @ rc, Array#{1, 2, 3} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 2), 2)
    }

    @test
    def testArrayLoad38(): Bool = region rc {
        let x = Array#{Array#{true, false, true} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 0), 2)
    }

    @test
    def testArrayLoad39(): Bool = region rc {
        let x = Array#{Array#{true, true} @ rc, Array#{true, false} @ rc} @ rc;
        not $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 1)
    }

    @test
    def testArrayLoad40(): Bool = region rc {
        let x = Array#{Array#{true} @ rc, Array#{true, false, true, false} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 2)
    }

    @test
    def testArrayLoad41(): Bool = region rc {
        let x = Array#{Array#{true, false, true} @ rc, Array#{true, false, true} @ rc, Array#{true, false, true} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 2), 2)
    }

    @test
    def testArrayLoad42(): Char = region rc {
        let x = Array#{Array#{'a', 'b', 'c'} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 0), 1)
    }

    @test
    def testArrayLoad43(): Char = region rc {
        let x = Array#{Array#{'a', 'b'} @ rc, Array#{'a', 'b'} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 1)
    }

    @test
    def testArrayLoad44(): Char = region rc {
        let x = Array#{Array#{'a'} @ rc, Array#{'a', 'b', 'c', 'd', 'e'} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 1)
    }

    @test
    def testArrayLoad45(): Char = region rc {
        let x = Array#{Array#{'a', 'b', 'c', 'd', 'e'} @ rc, Array#{'a'} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 0), 3)
    }

    @test
    def testArrayLoad46(): Char = region rc {
        let x = Array#{Array#{'a', 'b', 'c'} @ rc, Array#{'a', 'b', 'c'} @ rc, Array#{'a', 'b', 'c'} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 2), 1)
    }

    @test
    def testArrayLoad47(): Int8 = region rc {
        let x = Array#{Array#{1i8, 2i8, 3i8} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 0), 1)
    }

    @test
    def testArrayLoad48(): Int8 = region rc {
        let x = Array#{Array#{1i8, 2i8} @ rc, Array#{1i8, 2i8} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 0)
    }

    @test
    def testArrayLoad49(): Int8 = region rc {
        let x = Array#{Array#{1i8, 2i8, 3i8, 4i8} @ rc,Array#{1i8} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 0), 3)
    }

    @test
    def testArrayLoad50(): Int8 = region rc {
        let x = Array#{Array#{1i8} @ rc, Array#{1i8, 2i8, 3i8, 4i8} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 3)
    }

    @test
    def testArrayLoad51(): Int8 = region rc {
        let x = Array#{Array#{1i8, 2i8, 3i8} @ rc, Array#{1i8, 2i8, 3i8} @ rc, Array#{1i8, 2i8, 3i8} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 2), 2)
    }

    @test
    def testArrayLoad52(): Int16 = region rc {
        let x = Array#{Array#{1i16, 2i16, 3i16} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 0), 1)
    }

    @test
    def testArrayLoad53(): Int16 = region rc {
        let x = Array#{Array#{1i16, 2i16} @ rc, Array#{1i16, 2i16} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 1)
    }

    @test
    def testArrayLoad54(): Int16 = region rc {
        let x = Array#{Array#{1i16, 2i16, 3i16, 4i16} @ rc, Array#{1i16} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 0), 1)
    }

    @test
    def testArrayLoad55(): Int16 = region rc {
        let x = Array#{Array#{1i16} @ rc, Array#{1i16, 2i16, 3i16, 4i16} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 1)
    }

    @test
    def testArrayLoad56(): Int16 = region rc {
        let x = Array#{Array#{1i16, 2i16, 3i16} @ rc, Array#{1i16, 2i16, 3i16} @ rc, Array#{1i16, 2i16, 3i16} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 2)
    }

    @test
    def testArrayLoad57(): Int64 = region rc {
        let x = Array#{Array#{1i64, 2i64, 3i64} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 0), 2)
    }

    @test
    def testArrayLoad58(): Int64 = region rc {
        let x = Array#{Array#{1i64, 2i64} @ rc, Array#{1i64, 2i64} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 1)
    }

    @test
    def testArrayLoad59(): Int64 = region rc {
        let x = Array#{Array#{1i64} @ rc, Array#{1i64, 2i64, 3i64, 4i64} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 2)
    }

    @test
    def testArrayLoad60(): Int64 = region rc {
        let x = Array#{Array#{1i64, 2i64, 3i64, 4i64} @ rc, Array#{1i64} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 0)
    }

    @test
    def testArrayLoad61(): Int64 = region rc {
        let x = Array#{Array#{1i64, 2i64, 3i64} @ rc, Array#{1i64, 2i64, 3i64} @ rc, Array#{1i64, 2i64, 3i64} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 2), 0)
    }

    @test
    def testArrayLoad62(): Float32 = region rc {
        let x = Array#{Array#{1.0f32, 2.0f32, 3.0f32} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 0), 0)
    }

    @test
    def testArrayLoad63(): Float32 = region rc {
        let x = Array#{Array#{1.0f32, 2.0f32} @ rc, Array#{1.0f32, 2.0f32} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 1)
    }

    @test
    def testArrayLoad64(): Float32 = region rc {
        let x = Array#{Array#{1.0f32} @ rc, Array#{1.0f32, 2.0f32, 3.0f32, 4.0f32} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 2)
    }

    @test
    def testArrayLoad65(): Float32 = region rc {
        let x = Array#{Array#{1.0f32, 2.0f32, 3.0f32, 4.0f32} @ rc, Array#{1.0f32} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 0), 2)
    }

    @test
    def testArrayLoad66(): Float32 = region rc {
        let x = Array#{Array#{1.0f32, 2.0f32, 3.0f32} @ rc, Array#{1.0f32, 2.0f32, 3.0f32} @ rc, Array#{1.0f32, 2.0f32, 3.0f32} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 2), 2)
    }

    @test
    def testArrayLoad67(): Float64 = region rc {
        let x = Array#{Array#{1.0f64, 2.0f64, 3.0f64} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 0), 0)
    }

    @test
    def testArrayLoad68(): Float64 = region rc {
        let x = Array#{Array#{1.0f64, 2.0f64} @ rc, Array#{1.0f64, 2.0f64} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 1)
    }

    @test
    def testArrayLoad69(): Float64 = region rc {
        let x = Array#{Array#{1.0f64, 2.0f64, 3.0f64, 4.0f64} @ rc, Array#{1.0f64} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 0), 1)
    }

    @test
    def testArrayLoad70(): Float64 = region rc {
        let x = Array#{Array#{1.0f64, 2.0f64, 3.0f64} @ rc, Array#{1.0f64, 2.0f64, 3.0f64} @ rc, Array#{1.0f64, 2.0f64, 3.0f64} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 2), 2)
    }

    @test
    def testArrayLoad71(): String = region rc {
        let x = Array#{Array#{"Hello", "Hello", "Hello"} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 0), 0)
    }

    @test
    def testArrayLoad72(): String = region rc {
        let x = Array#{Array#{"Hello", "Hello"} @ rc, Array#{"World", "World"} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 1)
    }

    @test
    def testArrayLoad73(): String = region rc {
        let x = Array#{Array#{"Hello"} @ rc, Array#{"World", "World", "World", "World"} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 3)
    }

    @test
    def testArrayLoad74(): String = region rc {
        let x = Array#{Array#{"World", "World", "World", "World"} @ rc, Array#{"Hello"} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 0), 3)
    }

    @test
    def testArrayLoad75(): String = region rc {
        let x = Array#{Array#{"World", "World", "World"} @ rc, Array#{"World", "World", "World"} @ rc, Array#{"World", "World", "World"} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 2)
    }

    @test
    def testArrayLoad77(): Option[Int32] = region rc {
        let x = Array#{Some(5), Some(6), Some(7)} @ rc;
        $ARRAY_LOAD$(x, 1)
    }

    @test
    def testArrayLoad78(): Option[Int32] = region rc {
        let x = Array#{Array#{None, Some(4), Some(999)} @ rc, Array#{Some(9), Some(5), None, None} @ rc} @ rc;
        $ARRAY_LOAD$($ARRAY_LOAD$(x, 1), 2)
    }

    @test
    def testArrayLoad79(): Unit = region rc {
        let x = Array#{Array#{1 :: Nil} @ rc, Array#{3 :: Nil} @ rc} @ rc;
        discard $ARRAY_LOAD$(x, 0);
        ()
    }
}

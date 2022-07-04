namespace Test/Exp/ArrayLoad {

    @test
    def testArrayLoad01(): Int32 = region r {
        let x = [1] @ r;
        x[0]
    }

    @test
    def testArrayLoad02(): Int32 = region r {
        let x = [1, 2, 3] @ r;
        x[2]
    }

    @test
    def testArrayLoad03(): Int32 = region r {
        let x = [1, 2, 3, 4] @ r;
        x[2]
    }

    @test
    def testArrayLoad04(): Int32 = region r {
        let x = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20] @ r;
        x[10]
    }

    @test
    def testArrayLoad05(): Int32 = region r {
        let x = [1, 2] @ r;
        x[1]
    }

    @test
    def testArrayLoad06(): Bool = region r {
        let x = [true] @ r;
        x[0]
    }

    @test
    def testArrayLoad07(): Bool = region r {
        let x = [false] @ r;
        not x[0]
    }

    @test
    def testArrayLoad08(): Bool = region r {
        let x = [true, false] @ r;
        not x[1]
    }

    @test
    def testArrayLoad9(): Bool = region r {
        let x = [false, true, false] @ r;
        not x[2]
    }

    @test
    def testArrayLoad10(): Bool = region r {
        let x = [false, false, false, false, true] @ r;
        not x[3]
    }

    @test
    def testArrayLoad11(): Char = region r {
        let x = ['c'] @ r;
        x[0]
    }

    @test
    def testArrayLoad12(): Char = region r {
        let x = ['a', 'b'] @ r;
        x[0]
    }

    @test
    def testArrayLoad13(): Char = region r {
        let x = ['a', 'b', 'c'] @ r;
        x[1]
    }

    @test
    def testArrayLoad14(): Char = region r {
        let x = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n'] @ r;
        x[9]
    }

    @test
    def testArrayLoad15(): Int8 = region r {
        let x = [1i8] @ r;
        x[0]
    }

    @test
    def testArrayLoad16(): Int8 = region r {
        let x = [1i8, 2i8, 3i8] @ r;
        x[1]
    }

    @test
    def testArrayLoad17(): Int8 = region r {
        let x = [1i8, 2i8, 3i8, 4i8] @ r;
        x[2]
    }

    @test
    def testArrayLoad18(): Int16 = region r {
        let x = [1i16] @ r;
        x[0]
    }

    @test
    def testArrayLoad19(): Int16 = region r {
        let x = [1i16, 2i16, 3i16] @ r;
        x[1]
    }

    @test
    def testArrayLoad20(): Int16 = region r {
        let x = [1i16, 2i16, 3i16, 4i16] @ r;
        x[2]
    }

    @test
    def testArrayLoad21(): Int64 = region r {
        let x = [1i64] @ r;
        x[0]
    }

    @test
    def testArrayLoad22(): Int64 = region r {
        let x = [1i64, 2i64, 3i64] @ r;
        x[1]
    }

    @test
    def testArrayLoad23(): Int64 = region r {
        let x = [1i64, 2i64, 3i64, 4i64] @ r;
        x[2]
    }

    @test
    def testArrayLoad24(): Float32 = region r {
        let x = [1.0f32] @ r;
        x[0]
    }

    @test
    def testArrayLoad25(): Float32 = region r {
        let x = [1.0f32, 2.0f32, 3.0f32] @ r;
        x[1]
    }

    @test
    def testArrayLoad26(): Float32 = region r {
        let x = [1.0f32, 2.0f32, 3.0f32, 4.0f32] @ r;
        x[2]
    }

    @test
    def testArrayLoad27(): Float64 = region r {
        let x = [1.0f64] @ r;
        x[0]
    }

    @test
    def testArrayLoad28(): Float64 = region r {
        let x = [1.0f64, 2.0f64, 3.0f64] @ r;
        x[1]
    }

    @test
    def testArrayLoad29(): Float64 = region r {
        let x = [1.0f64, 2.0f64, 3.0f64, 4.0f64] @ r;
        x[3]
    }

    @test
    def testArrayLoad30(): String = region r {
        let x = ["Hello"] @ r;
        x[0]
    }

    @test
    def testArrayLoad31(): String = region r {
        let x = ["Hello", "World"] @ r;
        x[1]
    }

    @test
    def testArrayLoad32(): String = region r {
        let x = ["World", "Hello", "World"] @ r;
        x[2]
    }

    @test
    def testArrayLoad33(): Int32 = region r {
        let x = [[1, 2, 3] @ r] @ r;
        x[0][1]
    }

    @test
    def testArrayLoad34(): Int32 = region r {
        let x = [[1, 2] @ r, [1, 2] @ r] @ r;
        x[1][0]
    }

    @test
    def testArrayLoad35(): Int32 = region r {
        let x = [[1] @ r, [1, 2, 3, 4, 5, 6, 7, 8] @ r] @ r;
        x[1][5]
    }

    @test
    def testArrayLoad36(): Int32 = region r {
        let x = [[1, 2, 3, 4, 5] @ r, [2] @ r] @ r;
        x[0][3]
    }

    @test
    def testArrayLoad37(): Int32 = region r {
        let x = [[1, 2, 3] @ r, [1, 2, 3] @ r, [1, 2, 3] @ r] @ r;
        x[2][2]
    }

    @test
    def testArrayLoad38(): Bool = region r {
        let x = [[true, false, true] @ r] @ r;
        x[0] [2]
    }

    @test
    def testArrayLoad39(): Bool = region r {
        let x = [[true, true] @ r, [true, false] @ r] @ r;
        not x[1] [1]
    }

    @test
    def testArrayLoad40(): Bool = region r {
        let x = [[true] @ r, [true, false, true, false] @ r] @ r;
        x[1] [2]
    }

    @test
    def testArrayLoad41(): Bool = region r {
        let x = [[true, false, true] @ r, [true, false, true] @ r, [true, false, true] @ r] @ r;
        x[2] [2]
    }

    @test
    def testArrayLoad42(): Char = region r {
        let x = [['a', 'b', 'c'] @ r] @ r;
        x[0][1]
    }

    @test
    def testArrayLoad43(): Char = region r {
        let x = [['a', 'b'] @ r, ['a', 'b'] @ r] @ r;
        x[1][1]
    }

    @test
    def testArrayLoad44(): Char = region r {
        let x = [['a'] @ r, ['a', 'b', 'c', 'd', 'e'] @ r] @ r;
        x[1][1]
    }

    @test
    def testArrayLoad45(): Char = region r {
        let x = [['a', 'b', 'c', 'd', 'e'] @ r, ['a'] @ r] @ r;
        x[0][3]
    }

    @test
    def testArrayLoad46(): Char = region r {
        let x = [['a', 'b', 'c'] @ r, ['a', 'b', 'c'] @ r, ['a', 'b', 'c'] @ r] @ r;
        x[2][1]
    }

    @test
    def testArrayLoad47(): Int8 = region r {
        let x = [[1i8, 2i8, 3i8] @ r] @ r;
        x[0][1]
    }

    @test
    def testArrayLoad48(): Int8 = region r {
        let x = [[1i8, 2i8] @ r, [1i8, 2i8] @ r] @ r;
        x[1][0]
    }

    @test
    def testArrayLoad49(): Int8 = region r {
        let x = [[1i8, 2i8, 3i8, 4i8] @ r,[1i8] @ r] @ r;
        x[0][3]
    }

    @test
    def testArrayLoad50(): Int8 = region r {
        let x = [[1i8] @ r, [1i8, 2i8, 3i8, 4i8] @ r] @ r;
        x[1][3]
    }

    @test
    def testArrayLoad51(): Int8 = region r {
        let x = [[1i8, 2i8, 3i8] @ r, [1i8, 2i8, 3i8] @ r, [1i8, 2i8, 3i8] @ r] @ r;
        x[2][2]
    }

    @test
    def testArrayLoad52(): Int16 = region r {
        let x = [[1i16, 2i16, 3i16] @ r] @ r;
        x[0][1]
    }

    @test
    def testArrayLoad53(): Int16 = region r {
        let x = [[1i16, 2i16] @ r, [1i16, 2i16] @ r] @ r;
        x[1][1]
    }

    @test
    def testArrayLoad54(): Int16 = region r {
        let x = [[1i16, 2i16, 3i16, 4i16] @ r, [1i16] @ r] @ r;
        x[0][1]
    }

    @test
    def testArrayLoad55(): Int16 = region r {
        let x = [[1i16] @ r, [1i16, 2i16, 3i16, 4i16] @ r] @ r;
        x[1][1]
    }

    @test
    def testArrayLoad56(): Int16 = region r {
        let x = [[1i16, 2i16, 3i16] @ r, [1i16, 2i16, 3i16] @ r, [1i16, 2i16, 3i16] @ r] @ r;
        x[1][2]
    }

    @test
    def testArrayLoad57(): Int64 = region r {
        let x = [[1i64, 2i64, 3i64] @ r] @ r;
        x[0][2]
    }

    @test
    def testArrayLoad58(): Int64 = region r {
        let x = [[1i64, 2i64] @ r, [1i64, 2i64] @ r] @ r;
        x[1][1]
    }

    @test
    def testArrayLoad59(): Int64 = region r {
        let x = [[1i64] @ r, [1i64, 2i64, 3i64, 4i64] @ r] @ r;
        x[1][2]
    }

    @test
    def testArrayLoad60(): Int64 = region r {
        let x = [[1i64, 2i64, 3i64, 4i64] @ r, [1i64] @ r] @ r;
        x[1][0]
    }

    @test
    def testArrayLoad61(): Int64 = region r {
        let x = [[1i64, 2i64, 3i64] @ r, [1i64, 2i64, 3i64] @ r, [1i64, 2i64, 3i64] @ r] @ r;
        x[2][0]
    }

    @test
    def testArrayLoad62(): Float32 = region r {
        let x = [[1.0f32, 2.0f32, 3.0f32] @ r] @ r;
        x[0][0]
    }

    @test
    def testArrayLoad63(): Float32 = region r {
        let x = [[1.0f32, 2.0f32] @ r, [1.0f32, 2.0f32] @ r] @ r;
        x[1][1]
    }

    @test
    def testArrayLoad64(): Float32 = region r {
        let x = [[1.0f32] @ r, [1.0f32, 2.0f32, 3.0f32, 4.0f32] @ r] @ r;
        x[1][2]
    }

    @test
    def testArrayLoad65(): Float32 = region r {
        let x = [[1.0f32, 2.0f32, 3.0f32, 4.0f32] @ r, [1.0f32] @ r] @ r;
        x[0][2]
    }

    @test
    def testArrayLoad66(): Float32 = region r {
        let x = [[1.0f32, 2.0f32, 3.0f32] @ r, [1.0f32, 2.0f32, 3.0f32] @ r, [1.0f32, 2.0f32, 3.0f32] @ r] @ r;
        x[2][2]
    }

    @test
    def testArrayLoad67(): Float64 = region r {
        let x = [[1.0f64, 2.0f64, 3.0f64] @ r] @ r;
        x[0][0]
    }

    @test
    def testArrayLoad68(): Float64 = region r {
        let x = [[1.0f64, 2.0f64] @ r, [1.0f64, 2.0f64] @ r] @ r;
        x[1][1]
    }

    @test
    def testArrayLoad69(): Float64 = region r {
        let x = [[1.0f64, 2.0f64, 3.0f64, 4.0f64] @ r, [1.0f64] @ r] @ r;
        x[0][1]
    }

    @test
    def testArrayLoad70(): Float64 = region r {
        let x = [[1.0f64, 2.0f64, 3.0f64] @ r, [1.0f64, 2.0f64, 3.0f64] @ r, [1.0f64, 2.0f64, 3.0f64] @ r] @ r;
        x[2][2]
    }

    @test
    def testArrayLoad71(): String = region r {
        let x = [["Hello", "Hello", "Hello"] @ r] @ r;
        x[0][0]
    }

    @test
    def testArrayLoad72(): String = region r {
        let x = [["Hello", "Hello"] @ r, ["World", "World"] @ r] @ r;
        x[1][1]
    }

    @test
    def testArrayLoad73(): String = region r {
        let x = [["Hello"] @ r, ["World", "World", "World", "World"] @ r] @ r;
        x[1][3]
    }

    @test
    def testArrayLoad74(): String = region r {
        let x = [["World", "World", "World", "World"] @ r, ["Hello"] @ r] @ r;
        x[0][3]
    }

    @test
    def testArrayLoad75(): String = region r {
        let x = [["World", "World", "World"] @ r, ["World", "World", "World"] @ r, ["World", "World", "World"] @ r] @ r;
        x[1][2]
    }

    @test
    def testArrayLoad77(): Option[Int32] = region r {
        let x = [Some(5), Some(6), Some(7)] @ r;
        x[1]
    }

    @test
    def testArrayLoad78(): Option[Int32] = region r {
        let x = [[None, Some(4), Some(999)] @ r, [Some(9), Some(5), None, None] @ r] @ r;
        x[1][2]
    }

    @test
    def testArrayLoad79(): Unit = region r {
        let x = [[1 :: Nil] @ r, [3 :: Nil] @ r] @ r;
        discard x[0];
        ()
    }
}

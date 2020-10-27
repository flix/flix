namespace Test/Exp/ArrayLoad {

    @test
    def testArrayLoad01(): Int & Impure = let x = [1]; x[0]

    @test
    def testArrayLoad02(): Int & Impure = let x = [1, 2, 3]; x[2]

    @test
    def testArrayLoad03(): Int & Impure = let x = [1, 2, 3, 4]; x[2]

    @test
    def testArrayLoad04(): Int & Impure = let x = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]; x[10]

    @test
    def testArrayLoad05(): Int & Impure = let x = [1, 2]; x[1]

    @test
    def testArrayLoad06(): Bool & Impure = let x = [true]; x[0]

    @test
    def testArrayLoad07(): Bool & Impure = let x = [false]; !x[0]

    @test
    def testArrayLoad08(): Bool & Impure = let x = [true, false]; !x[1]

    @test
    def testArrayLoad9(): Bool & Impure = let x = [false, true, false]; !x[2]

    @test
    def testArrayLoad10(): Bool & Impure = let x = [false, false, false, false, true]; !x[3]

    @test
    def testArrayLoad11(): Char & Impure = let x = ['c']; x[0]

    @test
    def testArrayLoad12(): Char & Impure = let x = ['a', 'b']; x[0]

    @test
    def testArrayLoad13(): Char & Impure = let x = ['a', 'b', 'c']; x[1]

    @test
    def testArrayLoad14(): Char & Impure = let x = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n']; x[9]

    @test
    def testArrayLoad15(): Int8 & Impure = let x = [1i8]; x[0]

    @test
    def testArrayLoad16(): Int8 & Impure = let x = [1i8, 2i8, 3i8]; x[1]

    @test
    def testArrayLoad17(): Int8 & Impure = let x = [1i8, 2i8, 3i8, 4i8]; x[2]

    @test
    def testArrayLoad18(): Int16 & Impure = let x = [1i16]; x[0]

    @test
    def testArrayLoad19(): Int16 & Impure = let x = [1i16, 2i16, 3i16]; x[1]

    @test
    def testArrayLoad20(): Int16 & Impure = let x = [1i16, 2i16, 3i16, 4i16]; x[2]

    @test
    def testArrayLoad21(): Int64 & Impure = let x = [1i64]; x[0]

    @test
    def testArrayLoad22(): Int64 & Impure = let x = [1i64, 2i64, 3i64]; x[1]

    @test
    def testArrayLoad23(): Int64 & Impure = let x = [1i64, 2i64, 3i64, 4i64]; x[2]

    @test
    def testArrayLoad24(): Float32 & Impure = let x = [1.0f32]; x[0]

    @test
    def testArrayLoad25(): Float32 & Impure = let x = [1.0f32, 2.0f32, 3.0f32]; x[1]

    @test
    def testArrayLoad26(): Float32 & Impure = let x = [1.0f32, 2.0f32, 3.0f32, 4.0f32]; x[2]

    @test
    def testArrayLoad27(): Float64 & Impure = let x = [1.0f64]; x[0]

    @test
    def testArrayLoad28(): Float64 & Impure = let x = [1.0f64, 2.0f64, 3.0f64]; x[1]

    @test
    def testArrayLoad29(): Float64 & Impure = let x = [1.0f64, 2.0f64, 3.0f64, 4.0f64]; x[3]

    @test
    def testArrayLoad30(): String & Impure = let x = ["Hello"]; x[0]

    @test
    def testArrayLoad31(): String & Impure = let x = ["Hello", "World"]; x[1]

    @test
    def testArrayLoad32(): String & Impure = let x = ["World", "Hello", "World"]; x[2]

    @test
    def testArrayLoad33(): Int & Impure = let x = [[1, 2, 3]]; x[0][1]

    @test
    def testArrayLoad34(): Int & Impure = let x = [[1, 2], [1, 2]]; x[1][0]

    @test
    def testArrayLoad35(): Int & Impure = let x = [[1], [1, 2, 3, 4, 5, 6, 7, 8]]; x[1][5]

    @test
    def testArrayLoad36(): Int & Impure = let x = [[1, 2, 3, 4, 5], [2]]; x[0][3]

    @test
    def testArrayLoad37(): Int & Impure = let x = [[1, 2, 3], [1, 2, 3], [1, 2, 3]]; x[2][2]

    @test
    def testArrayLoad38(): Bool & Impure = let x = [[true, false, true]]; x[0] [2]

    @test
    def testArrayLoad39(): Bool & Impure = let x = [[true, true], [true, false]]; !x[1] [1]

    @test
    def testArrayLoad40(): Bool & Impure = let x = [[true], [true, false, true, false]]; x[1] [2]

    @test
    def testArrayLoad41(): Bool & Impure = let x = [[true, false, true], [true, false, true], [true, false, true]]; x[2] [2]

    @test
    def testArrayLoad42(): Char & Impure = let x = [['a', 'b', 'c']]; x[0][1]

    @test
    def testArrayLoad43(): Char & Impure = let x = [['a', 'b'], ['a', 'b']]; x[1][1]

    @test
    def testArrayLoad44(): Char & Impure = let x = [['a'], ['a', 'b', 'c', 'd', 'e']]; x[1][1]

    @test
    def testArrayLoad45(): Char & Impure = let x = [['a', 'b', 'c', 'd', 'e'], ['a']]; x[0][3]

    @test
    def testArrayLoad46(): Char & Impure = let x = [['a', 'b', 'c'], ['a', 'b', 'c'], ['a', 'b', 'c']]; x[2][1]

    @test
    def testArrayLoad47(): Int8 & Impure = let x = [[1i8, 2i8, 3i8]]; x[0][1]

    @test
    def testArrayLoad48(): Int8 & Impure = let x = [[1i8, 2i8], [1i8, 2i8]]; x[1][0]

    @test
    def testArrayLoad49(): Int8 & Impure = let x = [[1i8, 2i8, 3i8, 4i8],[1i8]]; x[0][3]

    @test
    def testArrayLoad50(): Int8 & Impure = let x = [[1i8], [1i8, 2i8, 3i8, 4i8]]; x[1][3]

    @test
    def testArrayLoad51(): Int8 & Impure = let x = [[1i8, 2i8, 3i8], [1i8, 2i8, 3i8], [1i8, 2i8, 3i8]]; x[2][2]

    @test
    def testArrayLoad52(): Int16 & Impure = let x = [[1i16, 2i16, 3i16]]; x[0][1]

    @test
    def testArrayLoad53(): Int16 & Impure = let x = [[1i16, 2i16], [1i16, 2i16]]; x[1][1]

    @test
    def testArrayLoad54(): Int16 & Impure = let x = [[1i16, 2i16, 3i16, 4i16], [1i16]]; x[0][1]

    @test
    def testArrayLoad55(): Int16 & Impure = let x = [[1i16], [1i16, 2i16, 3i16, 4i16]]; x[1][1]

    @test
    def testArrayLoad56(): Int16 & Impure = let x = [[1i16, 2i16, 3i16], [1i16, 2i16, 3i16], [1i16, 2i16, 3i16]]; x[1][2]

    @test
    def testArrayLoad57(): Int64 & Impure = let x = [[1i64, 2i64, 3i64]]; x[0][2]

    @test
    def testArrayLoad58(): Int64 & Impure = let x = [[1i64, 2i64], [1i64, 2i64]]; x[1][1]

    @test
    def testArrayLoad59(): Int64 & Impure = let x = [[1i64], [1i64, 2i64, 3i64, 4i64]]; x[1][2]

    @test
    def testArrayLoad60(): Int64 & Impure = let x = [[1i64, 2i64, 3i64, 4i64], [1i64]]; x[1][0]

    @test
    def testArrayLoad61(): Int64 & Impure = let x = [[1i64, 2i64, 3i64], [1i64, 2i64, 3i64], [1i64, 2i64, 3i64]]; x[2][0]

    @test
    def testArrayLoad62(): Float32 & Impure = let x = [[1.0f32, 2.0f32, 3.0f32]]; x[0][0]

    @test
    def testArrayLoad63(): Float32 & Impure = let x = [[1.0f32, 2.0f32], [1.0f32, 2.0f32]]; x[1][1]

    @test
    def testArrayLoad64(): Float32 & Impure = let x = [[1.0f32], [1.0f32, 2.0f32, 3.0f32, 4.0f32]]; x[1][2]

    @test
    def testArrayLoad65(): Float32 & Impure = let x = [[1.0f32, 2.0f32, 3.0f32, 4.0f32], [1.0f32]]; x[0][2]

    @test
    def testArrayLoad66(): Float32 & Impure = let x = [[1.0f32, 2.0f32, 3.0f32], [1.0f32, 2.0f32, 3.0f32], [1.0f32, 2.0f32, 3.0f32]]; x[2][2]

    @test
    def testArrayLoad67(): Float64 & Impure = let x = [[1.0f64, 2.0f64, 3.0f64]]; x[0][0]

    @test
    def testArrayLoad68(): Float64 & Impure = let x = [[1.0f64, 2.0f64], [1.0f64, 2.0f64]]; x[1][1]

    @test
    def testArrayLoad69(): Float64 & Impure = let x = [[1.0f64, 2.0f64, 3.0f64, 4.0f64], [1.0f64]]; x[0][1]

    @test
    def testArrayLoad70(): Float64 & Impure = let x = [[1.0f64, 2.0f64, 3.0f64], [1.0f64, 2.0f64, 3.0f64], [1.0f64, 2.0f64, 3.0f64]]; x[2][2]

    @test
    def testArrayLoad71(): String & Impure = let x = [["Hello", "Hello", "Hello"]]; x[0][0]

    @test
    def testArrayLoad72(): String & Impure = let x = [["Hello", "Hello"], ["World", "World"]]; x[1][1]

    @test
    def testArrayLoad73(): String & Impure = let x = [["Hello"], ["World", "World", "World", "World"]]; x[1][3]

    @test
    def testArrayLoad74(): String & Impure = let x = [["World", "World", "World", "World"], ["Hello"]]; x[0][3]

    @test
    def testArrayLoad75(): String & Impure = let x = [["World", "World", "World"], ["World", "World", "World"], ["World", "World", "World"]]; x[1][2]

    @test
    def testArrayLoad77(): Option[Int] & Impure = let x = [Some(5), Some(6), Some(7)]; x[1]

    @test
    def testArrayLoad78(): Option[Int] & Impure = let x = [[None, Some(4), Some(999)], [Some(9), Some(5), None, None]]; x[1][2]

    @test
    def testArrayLoad79(): Array[List[Int]] & Impure = let x = [[1 :: Nil], [3 :: Nil]]; x[0]

}

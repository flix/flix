namespace Test/Exp/ArrayLit {

    @test
    def testArrayLit01(): Array[Unit] & Impure = []

    @test
    def testArrayLit02(): Array[Int] & Impure = [1]

    @test
    def testArrayLit04(): Array[Int] & Impure = [1, 2, 3, 4]

    @test
    def testArrayLit05(): Array[Int] & Impure = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]

    @test
    def testArrayLit06(): Array[Int] & Impure = [1, 2]

    @test
    def testArrayLit07(): Array[Bool] & Impure = [true]

    @test
    def testArrayLit08(): Array[Bool] & Impure = [false]

    @test
    def testArrayLit09(): Array[Bool] & Impure = [true, false]

    @test
    def testArrayLit10(): Array[Bool] & Impure = [false, true, false]

    @test
    def testArrayLit11(): Array[Bool] & Impure = [false, false, false, false, true]

    @test
    def testArrayLit12(): Array[Char] & Impure = ['c']

    @test
    def testArrayLit13(): Array[Char] & Impure = ['a', 'b']

    @test
    def testArrayLit14(): Array[Char] & Impure = ['a', 'b', 'c']

    @test
    def testArrayLit15(): Array[Char] & Impure = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n']

    @test
    def testArrayLit16(): Array[Int8] & Impure = [1i8]

    @test
    def testArrayLit17(): Array[Int8] & Impure = [1i8, 2i8, 3i8]

    @test
    def testArrayLit18(): Array[Int8] & Impure = [1i8, 2i8, 3i8, 4i8]

    @test
    def testArrayLit19(): Array[Int16] & Impure = [1i16]

    @test
    def testArrayLit20(): Array[Int16] & Impure = [1i16, 2i16, 3i16]

    @test
    def testArrayLit21(): Array[Int16] & Impure = [1i16, 2i16, 3i16, 4i16]

    @test
    def testArrayLit22(): Array[Int64] & Impure = [1i64]

    @test
    def testArrayLit23(): Array[Int64] & Impure = [1i64, 2i64, 3i64]

    @test
    def testArrayLit24(): Array[Int64] & Impure = [1i64, 2i64, 3i64, 4i64]

    @test
    def testArrayLit25(): Array[Float32] & Impure = [1.0f32]

    @test
    def testArrayLit26(): Array[Float32] & Impure = [1.0f32, 2.0f32, 3.0f32]

    @test
    def testArrayLit27(): Array[Float32] & Impure = [1.0f32, 2.0f32, 3.0f32, 4.0f32]

    @test
    def testArrayLit28(): Array[Float64] & Impure = [1.0f64]

    @test
    def testArrayLit29(): Array[Float64] & Impure = [1.0f64, 2.0f64, 3.0f64]

    @test
    def testArrayLit30(): Array[Float64] & Impure = [1.0f64, 2.0f64, 3.0f64, 4.0f64]

    @test
    def testArrayLit31(): Array[String] & Impure = ["Hello"]

    @test
    def testArrayLit32(): Array[String] & Impure = ["Hello", "World"]

    @test
    def testArrayLit33(): Array[String] & Impure = ["World", "Hello", "World"]

    @test
    def testArrayLit34(): Array[Array[Int]] & Impure = [[1,2,3]]

    @test
    def testArrayLit35(): Array[Array[Int]] & Impure = [[1, 2], [1, 2]]

    @test
    def testArrayLit36(): Array[Array[Int]] & Impure = [[1], [1, 2, 3, 4, 5, 6, 7, 8]]

    @test
    def testArrayLit37(): Array[Array[Int]] & Impure = [[1, 2, 3, 4, 5], [2]]

    @test
    def testArrayLit38(): Array[Array[Int]] & Impure = [[1, 2, 3], [1, 2, 3], [1, 2, 3]]

    @test
    def testArrayLit39(): Array[Array[Bool]] & Impure = [[true, false]]

    @test
    def testArrayLit40(): Array[Array[Bool]] & Impure = [[true, false], [true, false]]

    @test
    def testArrayLit41(): Array[Array[Bool]] & Impure = [[true], [true, true, false, false]]

    @test
    def testArrayLit42(): Array[Array[Bool]] & Impure = [[true, false, true, false, true, false], [true]]

    @test
    def testArrayLit43(): Array[Array[Bool]] & Impure = [[true, true, true], [false, false, false], [false, true, false]]

    @test
    def testArrayLit44(): Array[Array[Char]] & Impure = [['a', 'b', 'c']]

    @test
    def testArrayLit45(): Array[Array[Char]] & Impure = [['a', 'b'], ['a', 'b']]

    @test
    def testArrayLit46(): Array[Array[Char]] & Impure = [['a'], ['a', 'b', 'c', 'd', 'e']]

    @test
    def testArrayLit47(): Array[Array[Char]] & Impure = [['a', 'b', 'c', 'd', 'e'], ['a']]

    @test
    def testArrayLit48(): Array[Array[Char]] & Impure = [['a', 'b', 'c'], ['a', 'b', 'c'], ['a', 'b', 'c']]

    @test
    def testArrayLit49(): Array[Array[Int8]] & Impure = [[1i8, 2i8, 3i8]]

    @test
    def testArrayLit50(): Array[Array[Int8]] & Impure = [[1i8, 2i8], [1i8, 2i8]]

    @test
    def testArrayLit51(): Array[Array[Int8]] & Impure = [[1i8], [1i8, 2i8, 3i8, 4i8, 5i8, 6i8]]

    @test
    def testArrayLit52(): Array[Array[Int8]] & Impure = [[1i8, 2i8, 3i8, 4i8, 5i8, 6i8], [1i8]]

    @test
    def testArrayLit53(): Array[Array[Int8]] & Impure = [[1i8, 2i8, 3i8], [1i8, 2i8, 3i8], [1i8, 2i8, 3i8]]

    @test
    def testArrayLit54(): Array[Array[Int16]] & Impure = [[1i16, 2i16, 3i16]]

    @test
    def testArrayLit55(): Array[Array[Int16]] & Impure = [[1i16, 2i16], [1i16, 2i16]]

    @test
    def testArrayLit56(): Array[Array[Int16]] & Impure = [[1i16], [1i16, 2i16, 3i16, 4i16, 5i16, 6i16]]

    @test
    def testArrayLit57(): Array[Array[Int16]] & Impure = [[1i16, 2i16, 3i16, 4i16, 5i16, 6i16], [1i16]]

    @test
    def testArrayLit58(): Array[Array[Int16]] & Impure = [[1i16, 2i16, 3i16], [1i16, 2i16, 3i16], [1i16, 2i16, 3i16]]

    @test
    def testArrayLit59(): Array[Array[Int64]] & Impure = [[1i64, 2i64, 3i64]]

    @test
    def testArrayLit60(): Array[Array[Int64]] & Impure = [[1i64, 2i64], [1i64, 2i64]]

    @test
    def testArrayLit61(): Array[Array[Int64]] & Impure = [[1i64], [1i64, 2i64, 3i64, 4i64, 5i64, 6i64]]

    @test
    def testArrayLit62(): Array[Array[Int64]] & Impure = [[1i64, 2i64, 3i64, 4i64, 5i64, 6i64], [1i64]]

    @test
    def testArrayLit63(): Array[Array[Int64]] & Impure= [[1i64, 2i64, 3i64], [1i64, 2i64, 3i64], [1i64, 2i64, 3i64]]

    @test
    def testArrayLit64(): Array[Array[Float32]] & Impure = [[1.0f32, 2.0f32, 3.0f32]]

    @test
    def testArrayLit65(): Array[Array[Float32]] & Impure = [[1.0f32, 2.0f32], [1.0f32, 2.0f32]]

    @test
    def testArrayLit66(): Array[Array[Float32]] & Impure = [[1.0f32], [1.0f32, 2.0f32, 3.0f32, 4.0f32, 5.0f32, 6.0f32]]

    @test
    def testArrayLit67(): Array[Array[Float32]] & Impure = [[1.0f32, 2.0f32, 3.0f32, 4.0f32, 5.0f32, 6.0f32], [1.0f32]]

    @test
    def testArrayLit68(): Array[Array[Float64]]  & Impure = [[1.0f64, 2.0f64, 3.0f64]]

    @test
    def testArrayLit69(): Array[Array[Float64]] & Impure = [[1.0f64, 2.0f64], [3.0f64, 4.0f64]]

    @test
    def testArrayLit70(): Array[Array[Float64]] & Impure = [[1.0f64], [1.0f64, 2.0f64, 3.0f64, 4.0f64, 5.0f64, 6.0f64]]

    @test
    def testArrayLit71(): Array[Array[Float64]] & Impure = [[1.0f64, 2.0f64, 3.0f64, 4.0f64, 5.0f64, 6.0f64], [1.0f64]]

    @test
    def testArrayLit72(): Array[Array[Float64]] & Impure = [[1.0f64, 2.0f64, 3.0f64], [1.0f64, 2.0f64, 3.0f64], [1.0f64, 2.0f64, 3.0f64]]

    @test
    def testArrayLit73(): Array[Array[String]] & Impure = [["Hello"]]

    @test
    def testArrayLit74(): Array[Array[String]] & Impure = [["Hello"], ["World"]]

    @test
    def testArrayLit75(): Array[Array[String]] & Impure = [["Hello"], ["Hello", "World", "World", "Hello"]]

    @test
    def testArrayLit76(): Array[Array[String]] & Impure = [["Hello", "World", "World", "Hello"] ,["Hello"]]

    @test
    def testArrayLit77(): Array[(Int, Int)] & Impure = [(12, 42)]

    @test
    def testArrayLit78(): Array[(Int, Int, Int)] & Impure = [(12, 42, 34), (12, 42, 34)]

    @test
    def testArrayLit79(): Array[(Int, Int, Int)] & Impure = [(12, 42, 12), (12, 23, 12), (12,12,12)]

    @test
    def testArrayLit80(): Array[(Int, Int, Int, Int)] & Impure = [(12, 42, 12, 12), (12, 23, 12, 12), (12,12,12, 31)]

    @test
    def testArrayLit81(): Array[(Int, Int)] & Impure =[(1,1), (1,1), (2,2), (3,3), (1,1), (2,1), (5,5)]

    @test
    def testArrayLit82(): Array[(Bool, Bool)] & Impure =[(true, true)]

    @test
    def testArrayLit83(): Array[(Bool, Bool, Bool)] & Impure =[(true, true, true), (true, true, true)]

    @test
    def testArrayLit84(): Array[(Bool, Bool, Bool)] & Impure =[(true, true, true), (true, false, false), (true, true, false)]

    @test
    def testArrayLit85(): Array[(List[Int])] & Impure = [(Nil)]

    @test
    def testArrayLit86(): Array[(List[Int])] & Impure = [(1 :: Nil)]

    @test
    def testArrayLit88(): Array[Option[String]] & Impure = [Some("Hello World")]

    @test
    def testArrayLit89(): Array[Option[Int]] & Impure = [Some(1), Some(2), None]

    @test
    def testArrayLit90(): Array[Vector[Int, 4]] & Impure = [[|1, 2, 3, 4|]]

}

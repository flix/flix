namespace Test/Exp/ArrayLit {

    @test
    def testArrayLit01(): ScopedArray[Unit, Impure] & Impure =
        []

    @test
    def testArrayLit02(): ScopedArray[Int32, Impure] & Impure =
        [1]

    @test
    def testArrayLit04(): ScopedArray[Int32, Impure] & Impure =
        [1, 2, 3, 4]

    @test
    def testArrayLit05(): ScopedArray[Int32, Impure] & Impure =
        [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]

    @test
    def testArrayLit06(): ScopedArray[Int32, Impure] & Impure =
        [1, 2]

    @test
    def testArrayLit07(): ScopedArray[Bool, Impure] & Impure =
        [true]

    @test
    def testArrayLit08(): ScopedArray[Bool, Impure] & Impure =
        [false]

    @test
    def testArrayLit09(): ScopedArray[Bool, Impure] & Impure =
        [true, false]

    @test
    def testArrayLit10(): ScopedArray[Bool, Impure] & Impure =
        [false, true, false]

    @test
    def testArrayLit11(): ScopedArray[Bool, Impure] & Impure =
        [false, false, false, false, true]

    @test
    def testArrayLit12(): ScopedArray[Char, Impure] & Impure =
        ['c']

    @test
    def testArrayLit13(): ScopedArray[Char, Impure] & Impure =
        ['a', 'b']

    @test
    def testArrayLit14(): ScopedArray[Char, Impure] & Impure =
        ['a', 'b', 'c']

    @test
    def testArrayLit15(): ScopedArray[Char, Impure] & Impure =
        ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n']

    @test
    def testArrayLit16(): ScopedArray[Int8, Impure] & Impure =
        [1i8]

    @test
    def testArrayLit17(): ScopedArray[Int8, Impure] & Impure =
        [1i8, 2i8, 3i8]

    @test
    def testArrayLit18(): ScopedArray[Int8, Impure] & Impure =
        [1i8, 2i8, 3i8, 4i8]

    @test
    def testArrayLit19(): ScopedArray[Int16, Impure] & Impure =
        [1i16]

    @test
    def testArrayLit20(): ScopedArray[Int16, Impure] & Impure =
        [1i16, 2i16, 3i16]

    @test
    def testArrayLit21(): ScopedArray[Int16, Impure] & Impure =
        [1i16, 2i16, 3i16, 4i16]

    @test
    def testArrayLit22(): ScopedArray[Int64, Impure] & Impure =
        [1i64]

    @test
    def testArrayLit23(): ScopedArray[Int64, Impure] & Impure =
        [1i64, 2i64, 3i64]

    @test
    def testArrayLit24(): ScopedArray[Int64, Impure] & Impure =
        [1i64, 2i64, 3i64, 4i64]

    @test
    def testArrayLit25(): ScopedArray[Float32, Impure] & Impure =
        [1.0f32]

    @test
    def testArrayLit26(): ScopedArray[Float32, Impure] & Impure =
        [1.0f32, 2.0f32, 3.0f32]

    @test
    def testArrayLit27(): ScopedArray[Float32, Impure] & Impure =
        [1.0f32, 2.0f32, 3.0f32, 4.0f32]

    @test
    def testArrayLit28(): ScopedArray[Float64, Impure] & Impure =
        [1.0f64]

    @test
    def testArrayLit29(): ScopedArray[Float64, Impure] & Impure =
        [1.0f64, 2.0f64, 3.0f64]

    @test
    def testArrayLit30(): ScopedArray[Float64, Impure] & Impure =
        [1.0f64, 2.0f64, 3.0f64, 4.0f64]

    @test
    def testArrayLit31(): ScopedArray[String, Impure] & Impure =
        ["Hello"]

    @test
    def testArrayLit32(): ScopedArray[String, Impure] & Impure =
        ["Hello", "World"]

    @test
    def testArrayLit33(): ScopedArray[String, Impure] & Impure =
        ["World", "Hello", "World"]

    @test
    def testArrayLit34(): ScopedArray[ScopedArray[Int32, Impure], Impure] & Impure =
        [[1,2,3]]

    @test
    def testArrayLit35(): ScopedArray[ScopedArray[Int32, Impure], Impure] & Impure =
        [[1, 2], [1, 2]]

    @test
    def testArrayLit36(): ScopedArray[ScopedArray[Int32, Impure], Impure] & Impure =
        [[1], [1, 2, 3, 4, 5, 6, 7, 8]]

    @test
    def testArrayLit37(): ScopedArray[ScopedArray[Int32, Impure], Impure] & Impure =
        [[1, 2, 3, 4, 5], [2]]

    @test
    def testArrayLit38(): ScopedArray[ScopedArray[Int32, Impure], Impure] & Impure =
        [[1, 2, 3], [1, 2, 3], [1, 2, 3]]

    @test
    def testArrayLit39(): ScopedArray[ScopedArray[Bool, Impure], Impure] & Impure =
        [[true, false]]

    @test
    def testArrayLit40(): ScopedArray[ScopedArray[Bool, Impure], Impure] & Impure =
        [[true, false], [true, false]]

    @test
    def testArrayLit41(): ScopedArray[ScopedArray[Bool, Impure], Impure] & Impure =
        [[true], [true, true, false, false]]

    @test
    def testArrayLit42(): ScopedArray[ScopedArray[Bool, Impure], Impure] & Impure =
        [[true, false, true, false, true, false], [true]]

    @test
    def testArrayLit43(): ScopedArray[ScopedArray[Bool, Impure], Impure] & Impure =
        [[true, true, true], [false, false, false], [false, true, false]]

    @test
    def testArrayLit44(): ScopedArray[ScopedArray[Char, Impure], Impure] & Impure =
        [['a', 'b', 'c']]

    @test
    def testArrayLit45(): ScopedArray[ScopedArray[Char, Impure], Impure] & Impure =
        [['a', 'b'], ['a', 'b']]

    @test
    def testArrayLit46(): ScopedArray[ScopedArray[Char, Impure], Impure] & Impure =
        [['a'], ['a', 'b', 'c', 'd', 'e']]

    @test
    def testArrayLit47(): ScopedArray[ScopedArray[Char, Impure], Impure] & Impure =
        [['a', 'b', 'c', 'd', 'e'], ['a']]

    @test
    def testArrayLit48(): ScopedArray[ScopedArray[Char, Impure], Impure] & Impure =
        [['a', 'b', 'c'], ['a', 'b', 'c'], ['a', 'b', 'c']]

    @test
    def testArrayLit49(): ScopedArray[ScopedArray[Int8, Impure], Impure] & Impure =
        [[1i8, 2i8, 3i8]]

    @test
    def testArrayLit50(): ScopedArray[ScopedArray[Int8, Impure], Impure] & Impure =
        [[1i8, 2i8], [1i8, 2i8]]

    @test
    def testArrayLit51(): ScopedArray[ScopedArray[Int8, Impure], Impure] & Impure =
        [[1i8], [1i8, 2i8, 3i8, 4i8, 5i8, 6i8]]

    @test
    def testArrayLit52(): ScopedArray[ScopedArray[Int8, Impure], Impure] & Impure =
        [[1i8, 2i8, 3i8, 4i8, 5i8, 6i8], [1i8]]

    @test
    def testArrayLit53(): ScopedArray[ScopedArray[Int8, Impure], Impure] & Impure =
        [[1i8, 2i8, 3i8], [1i8, 2i8, 3i8], [1i8, 2i8, 3i8]]

    @test
    def testArrayLit54(): ScopedArray[ScopedArray[Int16, Impure], Impure] & Impure =
        [[1i16, 2i16, 3i16]]

    @test
    def testArrayLit55(): ScopedArray[ScopedArray[Int16, Impure], Impure] & Impure =
        [[1i16, 2i16], [1i16, 2i16]]

    @test
    def testArrayLit56(): ScopedArray[ScopedArray[Int16, Impure], Impure] & Impure =
        [[1i16], [1i16, 2i16, 3i16, 4i16, 5i16, 6i16]]

    @test
    def testArrayLit57(): ScopedArray[ScopedArray[Int16, Impure], Impure] & Impure =
        [[1i16, 2i16, 3i16, 4i16, 5i16, 6i16], [1i16]]

    @test
    def testArrayLit58(): ScopedArray[ScopedArray[Int16, Impure], Impure] & Impure =
        [[1i16, 2i16, 3i16], [1i16, 2i16, 3i16], [1i16, 2i16, 3i16]]

    @test
    def testArrayLit59(): ScopedArray[ScopedArray[Int64, Impure], Impure] & Impure =
        [[1i64, 2i64, 3i64]]

    @test
    def testArrayLit60(): ScopedArray[ScopedArray[Int64, Impure], Impure] & Impure =
        [[1i64, 2i64], [1i64, 2i64]]

    @test
    def testArrayLit61(): ScopedArray[ScopedArray[Int64, Impure], Impure] & Impure =
        [[1i64], [1i64, 2i64, 3i64, 4i64, 5i64, 6i64]]

    @test
    def testArrayLit62(): ScopedArray[ScopedArray[Int64, Impure], Impure] & Impure =
        [[1i64, 2i64, 3i64, 4i64, 5i64, 6i64], [1i64]]

    @test
    def testArrayLit63(): ScopedArray[ScopedArray[Int64, Impure], Impure] & Impure =
        [[1i64, 2i64, 3i64], [1i64, 2i64, 3i64], [1i64, 2i64, 3i64]]

    @test
    def testArrayLit64(): ScopedArray[ScopedArray[Float32, Impure], Impure] & Impure =
        [[1.0f32, 2.0f32, 3.0f32]]

    @test
    def testArrayLit65(): ScopedArray[ScopedArray[Float32, Impure], Impure] & Impure =
        [[1.0f32, 2.0f32], [1.0f32, 2.0f32]]

    @test
    def testArrayLit66(): ScopedArray[ScopedArray[Float32, Impure], Impure] & Impure =
        [[1.0f32], [1.0f32, 2.0f32, 3.0f32, 4.0f32, 5.0f32, 6.0f32]]

    @test
    def testArrayLit67(): ScopedArray[ScopedArray[Float32, Impure], Impure] & Impure =
        [[1.0f32, 2.0f32, 3.0f32, 4.0f32, 5.0f32, 6.0f32], [1.0f32]]

    @test
    def testArrayLit68(): ScopedArray[ScopedArray[Float64, Impure], Impure] & Impure =
        [[1.0f64, 2.0f64, 3.0f64]]

    @test
    def testArrayLit69(): ScopedArray[ScopedArray[Float64, Impure], Impure] & Impure =
        [[1.0f64, 2.0f64], [3.0f64, 4.0f64]]

    @test
    def testArrayLit70(): ScopedArray[ScopedArray[Float64, Impure], Impure] & Impure =
        [[1.0f64], [1.0f64, 2.0f64, 3.0f64, 4.0f64, 5.0f64, 6.0f64]]

    @test
    def testArrayLit71(): ScopedArray[ScopedArray[Float64, Impure], Impure] & Impure =
        [[1.0f64, 2.0f64, 3.0f64, 4.0f64, 5.0f64, 6.0f64], [1.0f64]]

    @test
    def testArrayLit72(): ScopedArray[ScopedArray[Float64, Impure], Impure] & Impure =
        [[1.0f64, 2.0f64, 3.0f64], [1.0f64, 2.0f64, 3.0f64], [1.0f64, 2.0f64, 3.0f64]]

    @test
    def testArrayLit73(): ScopedArray[ScopedArray[String, Impure], Impure] & Impure =
        [["Hello"]]

    @test
    def testArrayLit74(): ScopedArray[ScopedArray[String, Impure], Impure] & Impure =
        [["Hello"], ["World"]]

    @test
    def testArrayLit75(): ScopedArray[ScopedArray[String, Impure], Impure] & Impure =
        [["Hello"], ["Hello", "World", "World", "Hello"]]

    @test
    def testArrayLit76(): ScopedArray[ScopedArray[String, Impure], Impure] & Impure =
        [["Hello", "World", "World", "Hello"] ,["Hello"]]

    @test
    def testArrayLit77(): ScopedArray[(Int32, Int32), Impure] & Impure =
        [(12, 42)]

    @test
    def testArrayLit78(): ScopedArray[(Int32, Int32, Int32), Impure] & Impure =
        [(12, 42, 34), (12, 42, 34)]

    @test
    def testArrayLit79(): ScopedArray[(Int32, Int32, Int32), Impure] & Impure =
        [(12, 42, 12), (12, 23, 12), (12,12,12)]

    @test
    def testArrayLit80(): ScopedArray[(Int32, Int32, Int32, Int32), Impure] & Impure =
        [(12, 42, 12, 12), (12, 23, 12, 12), (12,12,12, 31)]

    @test
    def testArrayLit81(): ScopedArray[(Int32, Int32), Impure] & Impure =
        [(1,1), (1,1), (2,2), (3,3), (1,1), (2,1), (5,5)]

    @test
    def testArrayLit82(): ScopedArray[(Bool, Bool), Impure] & Impure =
        [(true, true)]

    @test
    def testArrayLit83(): ScopedArray[(Bool, Bool, Bool), Impure] & Impure =
        [(true, true, true), (true, true, true)]

    @test
    def testArrayLit84(): ScopedArray[(Bool, Bool, Bool), Impure] & Impure =
        [(true, true, true), (true, false, false), (true, true, false)]

    @test
    def testArrayLit85(): ScopedArray[(List[Int32]), Impure] & Impure =
        [(Nil)]

    @test
    def testArrayLit86(): ScopedArray[(List[Int32]), Impure] & Impure =
        [(1 :: Nil)]

    @test
    def testArrayLit88(): ScopedArray[Option[String], Impure] & Impure =
        [Some("Hello World")]

    @test
    def testArrayLit89(): ScopedArray[Option[Int32], Impure] & Impure =
        [Some(1), Some(2), None]

}

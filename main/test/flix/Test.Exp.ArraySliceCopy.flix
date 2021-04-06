namespace Test/Exp/ArraySliceCopy {

    @test
    def testArraySliceCopy01(): Array[Int] & Impure = let x = [1, 2, 3]; x[..]

    @test
    def testArraySliceCopy02(): Array[Int] & Impure = let x = [1, 2, 3, 4]; x[..]

    @test
    def testArraySliceCopy03(): Array[Int] & Impure = let x = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]; x[..]

    @test
    def testArraySliceCopy04(): Array[Int] & Impure = let x = [1, 2]; x[..]

    @test
    def testArraySliceCopy05(): Array[Bool] & Impure = let x = [true, false]; x[..]

    @test
    def testArraySliceCopy06(): Array[Bool] & Impure = let x = [false, true, false]; x[..]

    @test
    def testArraySliceCopy07(): Array[Bool] & Impure = let x = [false, false, false, false, true]; x[..]

    @test
    def testArraySliceCopy08(): Array[Char] & Impure = let x = ['a', 'b']; x[..]

    @test
    def testArraySliceCopy09(): Array[Char] & Impure = let x = ['a', 'b', 'c']; x[..]

    @test
    def testArraySliceCopy10(): Array[Char] & Impure = let x = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n']; x[..]

    @test
    def testArraySliceCopy11(): Array[Int8] & Impure = let x = [1i8, 2i8, 3i8]; x[..]

    @test
    def testArraySliceCopy12(): Array[Int8] & Impure = let x = [1i8, 2i8, 3i8, 4i8]; x[..]

    @test
    def testArraySliceCopy13(): Array[Int16] & Impure = let x = [1i16, 2i16, 3i16]; x[..]

    @test
    def testArraySliceCopy14(): Array[Int16] & Impure = let x = [1i16, 2i16, 3i16, 4i16]; x[..]

    @test
    def testArraySliceCopy15(): Array[Int64] & Impure = let x = [1i64, 2i64, 3i64]; x[..]

    @test
    def testArraySliceCopy16(): Array[Int64] & Impure = let x = [1i64, 2i64, 3i64, 4i64]; x[..]

    @test
    def testArraySliceCopy17(): Array[Float32] & Impure = let x = [1.0f32, 2.0f32, 3.0f32]; x[..]

    @test
    def testArraySliceCopy18(): Array[Float32] & Impure = let x = [1.0f32, 2.0f32, 3.0f32, 4.0f32]; x[..]

    @test
    def testArraySliceCopy19(): Array[Float64] & Impure = let x = [1.0f64, 2.0f64, 3.0f64]; x[..]

    @test
    def testArraySliceCopy20(): Array[Float64] & Impure = let x = [1.0f64, 2.0f64, 3.0f64, 4.0f64]; x[..]

    @test
    def testArraySliceCopy21(): Array[String] & Impure = let x = ["Hello", "World"]; x[..]

    @test
    def testArraySliceCopy22(): Array[String] & Impure = let x = ["World", "Hello", "World"]; x[..]

    @test
    def testArraySliceCopy23(): Array[Array[Int]]  & Impure= let x = [[1, 2], [1, 2]]; x[..]

    @test
    def testArraySliceCopy24(): Array[Array[Int]] & Impure = let x = [[1], [1, 2, 3, 4, 5, 6, 7, 8]]; x[..]

    @test
    def testArraySliceCopy25(): Array[Array[Int]] & Impure = let x = [[1, 2, 3, 4, 5], [2]]; x[..]

    @test
    def testArraySliceCopy26(): Array[Array[Int]] & Impure = let x = [[1, 2, 3], [1, 2, 3], [1, 2, 3]]; x[..]

    @test
    def testArraySliceCopy27(): Array[Int] & Impure = let x = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]; x[..]

    @test
    def testArraySliceCopy28(): Array[(List[Int])] & Impure = let x = [(1 :: Nil), (2 :: Nil)]; x[..]

    @test
    def testArraySliceCopy29(): Array[(Bool, Bool)] & Impure = let x = [(true, true), (true, false), (true, true)]; x[..]

    @test
    def testArraySliceCopy30(): Array[Option[Int]] & Impure = let x = [None, None, Some(2), Some(2)]; x[..]

}

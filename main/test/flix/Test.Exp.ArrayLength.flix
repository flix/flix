namespace Test/Exp/ArrayLength {

    @test
    def testArrayLength01(): Int & Impure = let x = []; x.length

    @test
    def testArrayLength02(): Int & Impure = let x = [1]; x.length

    @test
    def testArrayLength03(): Int & Impure = let x = [1, 2, 3]; x.length

    @test
    def testArrayLength04(): Int & Impure = let x = [1, 2, 3, 4]; x.length

    @test
    def testArrayLength05(): Int & Impure = let x = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20]; x.length

    @test
    def testArrayLength06(): Int & Impure = let x = [1, 2]; x.length

    @test
    def testArrayLength07(): Int & Impure = let x = [true]; x.length

    @test
    def testArrayLength08(): Int & Impure = let x = [false]; x.length

    @test
    def testArrayLength09(): Int & Impure = let x = [true, false]; x.length

    @test
    def testArrayLength10(): Int & Impure = let x = [false, true, false]; x.length

    @test
    def testArrayLength11(): Int & Impure = let x = [false, false, false, false, true]; x.length

    @test
    def testArrayLength12(): Int & Impure = let x = ['c']; x.length

    @test
    def testArrayLength13(): Int & Impure = let x = ['a', 'b']; x.length

    @test
    def testArrayLength14(): Int & Impure = let x = ['a', 'b', 'c']; x.length

    @test
    def testArrayLength15(): Int & Impure = let x = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n']; x.length

    @test
    def testArrayLength16(): Int & Impure = let x = [1i8]; x.length

    @test
    def testArrayLength17(): Int & Impure = let x = [1i8, 2i8, 3i8]; x.length

    @test
    def testArrayLength18(): Int & Impure = let x = [1i8, 2i8, 3i8, 4i8]; x.length

    @test
    def testArrayLength19(): Int & Impure = let x = [1i16]; x.length

    @test
    def testArrayLength20(): Int & Impure = let x = [1i16, 2i16, 3i16]; x.length

    @test
    def testArrayLength21(): Int & Impure = let x = [1i16, 2i16, 3i16, 4i16]; x.length

    @test
    def testArrayLength22(): Int & Impure = let x = [1i64]; x.length

    @test
    def testArrayLength23(): Int & Impure = let x = [1i64, 2i64, 3i64]; x.length

    @test
    def testArrayLength24(): Int & Impure = let x = [1i64, 2i64, 3i64, 4i64]; x.length

    @test
    def testArrayLength25(): Int & Impure = let x = [1.0f32]; x.length

    @test
    def testArrayLength26(): Int & Impure = let x = [1.0f32, 2.0f32, 3.0f32]; x.length

    @test
    def testArrayLength27(): Int & Impure = let x = [1.0f32, 2.0f32, 3.0f32, 4.0f32]; x.length

    @test
    def testArrayLength28(): Int & Impure = let x = [1.0f64]; x.length

    @test
    def testArrayLength29(): Int & Impure = let x = [1.0f64, 2.0f64, 3.0f64]; x.length

    @test
    def testArrayLength30(): Int & Impure = let x = [1.0f64, 2.0f64, 3.0f64, 4.0f64]; x.length

    @test
    def testArrayLength31(): Int & Impure = let x = ["Hello"]; x.length

    @test
    def testArrayLength32(): Int & Impure = let x = ["Hello", "World"]; x.length

    @test
    def testArrayLength33(): Int & Impure = let x = ["World", "Hello", "World"]; x.length

    @test
    def testArrayLength34(): Int & Impure = let x = [[1, 2, 3]]; x.length

    @test
    def testArrayLength35(): Int & Impure = let x = [[1, 2], [1, 2]]; x.length

    @test
    def testArrayLength36(): Int & Impure = let x = [[1], [1, 2, 3, 4, 5, 6, 7, 8]]; x.length

    @test
    def testArrayLength37(): Int & Impure = let x = [[1, 2, 3, 4, 5], [2]]; x.length

    @test
    def testArrayLength38(): Int & Impure = let x = [[1, 2, 3], [1, 2, 3], [1, 2, 3]]; x.length

    @test
    def testArrayLength39(): Int & Impure = let x = [[None, Some(1), None], [Some(2), Some(45), Some(9), None]]; x.length

    @test
    def testArrayLength40(): Int & Impure = let x = [(1, 2), (3, 4)]; x.length

    @test
    def testArrayLength41(): Int & Impure = let x = [[1 :: Nil], [3 :: 4 :: 5 :: Nil]]; x.length

}

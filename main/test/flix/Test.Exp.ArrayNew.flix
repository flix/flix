namespace Test/Exp/ArrayNew {

    @test
    def testArrayNew01(): Array[Int] & Impure = [10; 0]

    @test
    def testArrayNew02(): Array[Int] & Impure = [1; 1]

    @test
    def testArrayNew03(): Array[Int] & Impure = [0; 2]

    @test
    def testArrayNew04(): Array[Int] & Impure = [0; 113]

    @test
    def testArrayNew05(): Array[Int] & Impure = [0; 500]

    @test
    def testArrayNew06(): Array[Unit] & Impure = [(); 5]

    @test
    def testArrayNew07(): Array[Bool] & Impure = [true; 5]

    @test
    def testArrayNew08(): Array[Bool] & Impure = [false; 5]

    @test
    def testArrayNew09(): Array[Char] & Impure = ['a'; 5]

    @test
    def testArrayNew10(): Array[Int8] & Impure = [10i8; 5]

    @test
    def testArrayNew11(): Array[Int16] & Impure = [10i16; 5]

    @test
    def testArrayNew12(): Array[Int32] & Impure = [10; 5]

    @test
    def testArrayNew13(): Array[Int64] & Impure = [10i64; 5]

    @test
    def testArrayNew14(): Array[BigInt] & Impure = [10ii; 5]

    @test
    def testArrayNew15(): Array[Float32] & Impure= [3.0f32; 5]

    @test
    def testArrayNew16(): Array[Float64] & Impure = [3.0f64; 5]

    @test
    def testArrayNew17(): Array[String] & Impure = ["foo"; 5]

    @test
    def testArrayNew18(): Array[Array[Int]] & Impure = [[10; 10]; 5]

    @test
    def testArrayNew19(): Array[(Int, Int)] & Impure = [(1, 2); 5]

    @test
    def testArrayNew20(): Array[(List[Int])] & Impure = [(1 :: Nil); 5]

    @test
    def testArrayNew21(): Array[Option[Int]] & Impure = [None; 1]

}

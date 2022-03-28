namespace Test/Exp/Ref/Ref {

    @test
    def testRef01(): Ref[Unit] & Impure = ref ()

    @test
    def testRef02(): Ref[Bool] & Impure = ref true

    @test
    def testRef03(): Ref[Bool] & Impure = ref false

    @test
    def testRef04(): Ref[Char] & Impure = ref 'a'

    @test
    def testRef05(): Ref[Float32] & Impure = ref 0.0f32

    @test
    def testRef06(): Ref[Float64] & Impure = ref 0.0f64

    @test
    def testRef07(): Ref[Int8] & Impure = ref 0i8

    @test
    def testRef08(): Ref[Int16] & Impure = ref 0i16

    @test
    def testRef09(): Ref[Int32] & Impure = ref 0i32

    @test
    def testRef10(): Ref[Int64] & Impure = ref 0i64

    @test
    def testRef11(): Ref[BigInt] & Impure = ref 0ii

    @test
    def testRef12(): Ref[String] & Impure = ref "Hello World!"

    @test
    def testRef13(): Ref[(Int32, Int32, Int32)] & Impure = ref (1, 2, 3)

    @test
    def testRef14(): Ref[Option[Int32]] & Impure = ref None : Option[Int32]

    @test
    def testRef15(): Ref[Option[Int32]] & Impure = ref Some(42)

    @test
    def testRef16(): Ref[Result[Int32, String]] & Impure = ref Ok(42): Result[Int32, String]

    @test
    def testRef17(): Ref[Result[Int32, String]] & Impure = ref Err("Goodbye World!"): Result[Int32, String]

}

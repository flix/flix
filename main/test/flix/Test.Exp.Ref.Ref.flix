namespace Test/Exp/Ref/Ref {

    @test
    def testRef01(): Ref[Unit, static] & Impure = ref ()

    @test
    def testRef02(): Ref[Bool, static] & Impure = ref true

    @test
    def testRef03(): Ref[Bool, static] & Impure = ref false

    @test
    def testRef04(): Ref[Char, static] & Impure = ref 'a'

    @test
    def testRef05(): Ref[Float32, static] & Impure = ref 0.0f32

    @test
    def testRef06(): Ref[Float64, static] & Impure = ref 0.0f64

    @test
    def testRef07(): Ref[Int8, static] & Impure = ref 0i8

    @test
    def testRef08(): Ref[Int16, static] & Impure = ref 0i16

    @test
    def testRef09(): Ref[Int32, static] & Impure = ref 0i32

    @test
    def testRef10(): Ref[Int64, static] & Impure = ref 0i64

    @test
    def testRef11(): Ref[BigInt, static] & Impure = ref 0ii

    @test
    def testRef12(): Ref[String, static] & Impure = ref "Hello World!"

    @test
    def testRef13(): Ref[(Int32, Int32, Int32), static] & Impure = ref (1, 2, 3)

    @test
    def testRef14(): Ref[Option[Int32], static] & Impure = ref None : Option[Int32]

    @test
    def testRef15(): Ref[Option[Int32], static] & Impure = ref Some(42)

    @test
    def testRef16(): Ref[Result[Int32, String], static] & Impure = ref Ok(42): Result[Int32, String]

    @test
    def testRef17(): Ref[Result[Int32, String], static] & Impure = ref Err("Goodbye World!"): Result[Int32, String]

    @test
    def testRefRegion01(): Unit = // Pure
        region r {
            let _u = ref () @ r;
            ()
        }

    @test
    def testRefRegion02(): Unit = // Pure
        region r {
            let _u = ref true @ r;
            ()
        }

    @test
    def testRefRegion03(): Unit = // Pure
        region r {
            let _u = ref 'a' @ r;
            ()
        }

}

namespace Test/Exp/Ref/Ref {

    @test
    def testRef01(): FooRef[Unit] & Impure = ref ()

    @test
    def testRef02(): FooRef[Bool] & Impure = ref true

    @test
    def testRef03(): FooRef[Bool] & Impure = ref false

    @test
    def testRef04(): FooRef[Char] & Impure = ref 'a'

    @test
    def testRef05(): FooRef[Float32] & Impure = ref 0.0f32

    @test
    def testRef06(): FooRef[Float64] & Impure = ref 0.0f64

    @test
    def testRef07(): FooRef[Int8] & Impure = ref 0i8

    @test
    def testRef08(): FooRef[Int16] & Impure = ref 0i16

    @test
    def testRef09(): FooRef[Int32] & Impure = ref 0i32

    @test
    def testRef10(): FooRef[Int64] & Impure = ref 0i64

    @test
    def testRef11(): FooRef[BigInt] & Impure = ref 0ii

    @test
    def testRef12(): FooRef[String] & Impure = ref "Hello World!"

    @test
    def testRef13(): FooRef[(Int32, Int32, Int32)] & Impure = ref (1, 2, 3)

    @test
    def testRef14(): FooRef[Option[Int32]] & Impure = ref None : Option[Int32]

    @test
    def testRef15(): FooRef[Option[Int32]] & Impure = ref Some(42)

    @test
    def testRef16(): FooRef[Result[Int32, String]] & Impure = ref Ok(42): Result[Int32, String]

    @test
    def testRef17(): FooRef[Result[Int32, String]] & Impure = ref Err("Goodbye World!"): Result[Int32, String]

    @test
    def testScopedRef01(): Unit = // Pure
        region r {
            let _u = ref () @ r;
            ()
        }

    @test
    def testScopedRef02(): Unit = // Pure
        region r {
            let _u = ref true @ r;
            ()
        }

    @test
    def testScopedRef03(): Unit = // Pure
        region r {
            let _u = ref 'a' @ r;
            ()
        }

}

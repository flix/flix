namespace Test/Exp/Ref/Ref {

    @test
    def testRef01(): Ref[Unit, Static] \ IO = ref ()

    @test
    def testRef02(): Ref[Bool, Static] \ IO = ref true

    @test
    def testRef03(): Ref[Bool, Static] \ IO = ref false

    @test
    def testRef04(): Ref[Char, Static] \ IO = ref 'a'

    @test
    def testRef05(): Ref[Float32, Static] \ IO = ref 0.0f32

    @test
    def testRef06(): Ref[Float64, Static] \ IO = ref 0.0f64

    @test
    def testRef07(): Ref[Int8, Static] \ IO = ref 0i8

    @test
    def testRef08(): Ref[Int16, Static] \ IO = ref 0i16

    @test
    def testRef09(): Ref[Int32, Static] \ IO = ref 0i32

    @test
    def testRef10(): Ref[Int64, Static] \ IO = ref 0i64

    @test
    def testRef11(): Ref[BigInt, Static] \ IO = ref 0ii

    @test
    def testRef12(): Ref[BigDecimal, Static] \ IO = ref 0.0ff

    @test
    def testRef13(): Ref[String, Static] \ IO = ref "Hello World!"

    @test
    def testRef14(): Ref[(Int32, Int32, Int32), Static] \ IO = ref (1, 2, 3)

    @test
    def testRef15(): Ref[Option[Int32], Static] \ IO = ref None : Option[Int32]

    @test
    def testRef16(): Ref[Option[Int32], Static] \ IO = ref Some(42)

    @test
    def testRef17(): Ref[Result[Int32, String], Static] \ IO = ref Ok(42): Result[Int32, String]

    @test
    def testRef18(): Ref[Result[Int32, String], Static] \ IO = ref Err("Goodbye World!"): Result[Int32, String]

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

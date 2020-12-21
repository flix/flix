namespace Test/Exp/Reference/Deref {

    @test
    def testDeref01(): Bool & Impure =
        let l = ref ();
        deref l == ()

    @test
    def testDeref02(): Bool & Impure =
        let l = ref true;
        deref l == true

    @test
    def testDeref03(): Bool & Impure =
        let l = ref false;
        deref l == false

    @test
    def testDeref04(): Bool & Impure =
        let l = ref 'a';
        deref l == 'a'

    @test
    def testDeref05(): Bool & Impure =
        let l = ref 123.456f32;
        deref l == 123.456f32

    @test
    def testDeref06(): Bool & Impure =
        let l = ref 123.456f64;
        deref l == 123.456f64

    @test
    def testDeref07(): Bool & Impure =
        let l = ref 42i8;
        deref l == 42i8

    @test
    def testDeref08(): Bool & Impure =
        let l = ref 42i16;
        deref l == 42i16

    @test
    def testDeref09(): Bool & Impure =
        let l = ref 42i32;
        deref l == 42i32

    @test
    def testDeref10(): Bool & Impure =
        let l = ref 42i64;
        deref l == 42i64

    @test
    def testDeref11(): Bool & Impure =
        let l = ref 42ii;
        deref l == 42ii

    @test
    def testDeref12(): Bool & Impure =
        let l = ref "Hello World!";
        deref l == "Hello World!"

    @test
    def testDeref13(): Bool & Impure =
        let l = ref (1, 2, 3);
        deref l == (1, 2, 3)

    @test
    def testDeref14(): Bool & Impure =
        let l = ref None : Option[Int];
        deref l == None

    @test
    def testDeref15(): Bool & Impure =
        let l = ref Some(42);
        deref l == Some(42)

///    @test
///    def testDeref16(): Bool & Impure =
///        let l = ref Ok(42);
///        deref l == Ok(42)

///    @test
///    def testDeref17(): Bool & Impure =
///        let l = ref Err("Goodbye World!");
///        deref l == Err("Goodbye World!")

}

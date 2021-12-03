namespace Test/Exp/LetRec {

    @test
    def testLetRecUnit01(): Bool =
        def loop(n) = if (n == 0) () else loop(n - 1);
        loop(10) == ()

    @test
    def testLetRecBool01(): Bool =
        def loop(n) = if (n == 0) true else loop(n - 1);
        loop(10) == true

    @test
    def testLetRecChar01(): Bool =
        def loop(n) = if (n == 0) 'a' else loop(n - 1);
        loop(10) == 'a'

    @test
    def testLetRecFloat3201(): Bool =
        def loop(n) = if (n == 0) 123.456f32 else loop(n - 1);
        loop(10) == 123.456f32

    @test
    def testLetRecFloat6401(): Bool =
        def loop(n) = if (n == 0) 123.456f64 else loop(n - 1);
        loop(10) == 123.456f64



}


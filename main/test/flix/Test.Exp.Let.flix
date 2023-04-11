mod Test.Exp.Let {

    @test
    def testLet01(): Bool =
        let x = 1;
        x == 1

    @test
    def testLet02(): Bool =
        let x = 1;
        let y = 2;
        x + y == 1 + 2

    @test
    def testLet03(): Bool =
        let x = 1;
        let y = 2;
        let z = 3;
        x + y + z == 1 + 2 + 3

    @test
    def testLet04(): Bool =
        let x = {
            let x1 = 1;
            x1
        };
        let y = {
            let y1 = 2;
            y1
        };
        x + y == 1 + 2

    @test
    def testLet05(): Bool =
        let x = {
            let x1 = 1;
            let x2 = 2;
            x1 + x2
        };
        let y = {
            let y1 = 3;
            let y2 = 4;
            y1 + y2
        };
        x + y == 1 + 2 + 3 + 4

    @test
    def testLet06(): Bool =
        if (let x = true; x) true else false

    @test
    def testLet07(): Bool =
        if (true) {
            let x = 1;
            x == 1
        } else {
            false
        }

    @test
    def testLet08(): Bool =
        if (false) {
            false
        } else {
            let x = 1;
            x == 1
        }

    @test
    def testLetUnit01(): Bool =
        let x = ();
        x == ()

    @test
    def testLetBool01(): Bool =
        let x = true;
        x == true

    @test
    def testLetBool02(): Bool =
        let x = false;
        x == false

    @test
    def testLetChar01(): Bool =
        let x = 'a';
        x == 'a'

    @test
    def testLetFloat3201(): Bool =
        let x = 123.456f32;
        x == 123.456f32

    @test
    def testLetFloat6401(): Bool =
        let x = 123.456f64;
        x == 123.456f64

    @test
    def testLetInt8(): Bool =
        let x = 123i8;
        x == 123i8

    @test
    def testLetInt16(): Bool =
        let x = 123i16;
        x == 123i16

    @test
    def testLetInt32(): Bool =
        let x = 123i32;
        x == 123i32

    @test
    def testLetInt64(): Bool =
        let x = 123i64;
        x == 123i64

    @test
    def testLetString01(): Bool =
        let x = "Hello World!";
        x == "Hello World!"

}

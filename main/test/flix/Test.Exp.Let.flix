mod Test.Exp.Let {

    @Test
    def testLet01(): Bool =
        let x = 1;
        x == 1

    @Test
    def testLet02(): Bool =
        let x = 1;
        let y = 2;
        x + y == 1 + 2

    @Test
    def testLet03(): Bool =
        let x = 1;
        let y = 2;
        let z = 3;
        x + y + z == 1 + 2 + 3

    @Test
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

    @Test
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

    @Test
    def testLet06(): Bool =
        if (let x = true; x) true else false

    @Test
    def testLet07(): Bool =
        if (true) {
            let x = 1;
            x == 1
        } else {
            false
        }

    @Test
    def testLet08(): Bool =
        if (false) {
            false
        } else {
            let x = 1;
            x == 1
        }

    @Test
    def testLetUnit01(): Bool =
        let x = ();
        x == ()

    @Test
    def testLetBool01(): Bool =
        let x = true;
        x == true

    @Test
    def testLetBool02(): Bool =
        let x = false;
        x == false

    @Test
    def testLetChar01(): Bool =
        let x = 'a';
        x == 'a'

    @Test
    def testLetFloat3201(): Bool =
        let x = 123.456f32;
        x == 123.456f32

    @Test
    def testLetFloat6401(): Bool =
        let x = 123.456f64;
        x == 123.456f64

    @Test
    def testLetInt8(): Bool =
        let x = 123i8;
        x == 123i8

    @Test
    def testLetInt16(): Bool =
        let x = 123i16;
        x == 123i16

    @Test
    def testLetInt32(): Bool =
        let x = 123i32;
        x == 123i32

    @Test
    def testLetInt64(): Bool =
        let x = 123i64;
        x == 123i64

    @Test
    def testLetString01(): Bool =
        let x = "Hello World!";
        x == "Hello World!"

}

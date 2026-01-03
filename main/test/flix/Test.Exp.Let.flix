mod Test.Exp.Let {

    use Assert.assertEq

    @Test
    def testLet01(): Unit \ Assert =
        let x = 1;
        assertEq(expected = true, x == 1)

    @Test
    def testLet02(): Unit \ Assert =
        let x = 1;
        let y = 2;
        assertEq(expected = true, x + y == 1 + 2)

    @Test
    def testLet03(): Unit \ Assert =
        let x = 1;
        let y = 2;
        let z = 3;
        assertEq(expected = true, x + y + z == 1 + 2 + 3)

    @Test
    def testLet04(): Unit \ Assert =
        let x = {
            let x1 = 1;
            x1
        };
        let y = {
            let y1 = 2;
            y1
        };
        assertEq(expected = true, x + y == 1 + 2)

    @Test
    def testLet05(): Unit \ Assert =
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
        assertEq(expected = true, x + y == 1 + 2 + 3 + 4)

    @Test
    def testLet06(): Unit \ Assert =
        assertEq(expected = true, if (let x = true; x) true else false)

    @Test
    def testLet07(): Unit \ Assert =
        assertEq(expected = true, if (true) {
            let x = 1;
            x == 1
        } else {
            false
        })

    @Test
    def testLet08(): Unit \ Assert =
        assertEq(expected = true, if (false) {
            false
        } else {
            let x = 1;
            x == 1
        })

    @Test
    def testLetUnit01(): Unit \ Assert =
        let x = ();
        assertEq(expected = true, x == ())

    @Test
    def testLetBool01(): Unit \ Assert =
        let x = true;
        assertEq(expected = true, x == true)

    @Test
    def testLetBool02(): Unit \ Assert =
        let x = false;
        assertEq(expected = true, x == false)

    @Test
    def testLetChar01(): Unit \ Assert =
        let x = 'a';
        assertEq(expected = true, x == 'a')

    @Test
    def testLetFloat3201(): Unit \ Assert =
        let x = 123.456f32;
        assertEq(expected = true, x == 123.456f32)

    @Test
    def testLetFloat6401(): Unit \ Assert =
        let x = 123.456f64;
        assertEq(expected = true, x == 123.456f64)

    @Test
    def testLetInt8(): Unit \ Assert =
        let x = 123i8;
        assertEq(expected = true, x == 123i8)

    @Test
    def testLetInt16(): Unit \ Assert =
        let x = 123i16;
        assertEq(expected = true, x == 123i16)

    @Test
    def testLetInt32(): Unit \ Assert =
        let x = 123i32;
        assertEq(expected = true, x == 123i32)

    @Test
    def testLetInt64(): Unit \ Assert =
        let x = 123i64;
        assertEq(expected = true, x == 123i64)

    @Test
    def testLetString01(): Unit \ Assert =
        let x = "Hello World!";
        assertEq(expected = true, x == "Hello World!")

}

mod Test.Exp.Infix {

    use Assert.assertEq

    @Test
    def testInfix01(): Unit \ Assert =
        assertEq(expected = 1 + 2, 1 `add` 2)

    @Test
    def testInfix02(): Unit \ Assert =
        assertEq(expected = 1 - 2, 1 `sub` 2)

    @Test
    def testInfix03(): Unit \ Assert =
        assertEq(expected = (1 + 2) + 3, 1 `add` 2 `add` 3)

    @Test
    def testInfix04(): Unit \ Assert =
        assertEq(expected = (1 + 2) - 3, 1 `add` 2 `sub` 3)

    @Test
    def testInfix05(): Unit \ Assert =
        assertEq(expected = 2 * 3, 2 `Test.Exp.Infix.A.mul` 3)

    @Test
    def testInfix06(): Unit \ Assert =
        assertEq(expected = 3 / 2, 3 `Test.Exp.Infix.Divisible.div` 2)

    def add(x: Int32, y: Int32): Int32 = x + y
    def sub(x: Int32, y: Int32): Int32 = x - y

    mod A {
        pub def mul(x: Int32, y: Int32): Int32 = x * y
    }

    trait Divisible[a] {
        pub def div(x: a, y: a): a
    }

    instance Divisible[Int32] {
        pub def div(x: Int32, y: Int32): Int32 = x / y
    }

}

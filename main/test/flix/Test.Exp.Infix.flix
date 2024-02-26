mod Test.Exp.Infix {

    @test
    def testInfix01(): Bool = 1 `add` 2 == 1 + 2

    @test
    def testInfix02(): Bool = 1 `sub` 2 == 1 - 2

    @test
    def testInfix03(): Bool = 1 `add` 2 `add` 3 == (1 + 2) + 3

    @test
    def testInfix04(): Bool = 1 `add` 2 `sub` 3 == (1 + 2) - 3

    @test
    def testInfix05(): Bool = 2 `Test.Exp.Infix.A.mul` 3 == 2 * 3

    @test
    def testInfix06(): Bool = 3 `Test.Exp.Infix.Divisible.div` 2 == 3 / 2

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

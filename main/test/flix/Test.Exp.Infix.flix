namespace Test/Exp/Infix {

    @test
    def testInfix01(): Bool = 1 `add` 2 == 1 + 2

    @test
    def testInfix02(): Bool = 1 `sub` 2 == 1 - 2

    @test
    def testInfix03(): Bool = 1 `add` 2 `add` 3 == (1 + 2) + 3

    @test
    def testInfix04(): Bool = 1 `add` 2 `sub` 3 == (1 + 2) - 3

    @test
    def testInfix05(): Bool = 2 `Test/Exp/Infix/A.mul` 3 == 2 * 3

    @test
    def testInfix06(): Bool = 3 `Test/Exp/Infix/Divisible.div` 2 == 3 / 2

    def add(x: Int, y: Int): Int = x + y
    def sub(x: Int, y: Int): Int = x - y

    namespace A {
        pub def mul(x: Int, y: Int): Int = x * y
    }

    lawless class Divisible[a] {
        pub def div(x: a, y: a): a
    }

    instance Divisible[Int] {
        pub def div(x: Int, y: Int): Int = x / y
    }

}

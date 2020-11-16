namespace Test/Exp/Apply/Tail {

    @test
    def testApplyTail01(): Bool = odd(1)

    @test
    def testApplyTail02(): Bool = even(12)

    @test
    def testApplyTail03(): Bool = odd(123)

    @test
    def testApplyTail04(): Bool = even(1234)

    @test
    def testApplyTail05(): Bool = odd(12345)

    @test
    def testApplyTail06(): Bool = even(123456)

    @test
    def testApplyTail07(): Bool = odd(1234567)

    @test
    def testApplyTail08(): Bool = a(42)

    @test
    def testApplyTail09(): Bool = a1(42, z -> z + 1)

    @test
    def testApplyTail10(): Int =  ping(123, x -> x)

    @test
    def testApplyTail11(): Int =  ping(12345, x -> x)

    @test
    def testApplyTail12(): Int =  ping(1234567, x -> x)

    def odd(n: Int): Bool = match n {
        case 0 => false
        case 1 => true
        case x => even(x - 1)
    }

    def even(n: Int): Bool = match n {
        case 0 => true
        case 1 => false
        case x => odd(x - 1)
    }

    def a(x: Int): Bool = b(x + 1)

    def b(x: Int): Bool = c(x - 1)

    def c(x: Int): Bool = x == 42

    def a1(x: Int, f: Int -> Int): Bool = b1(f(x), y -> y - 1)

    def b1(x: Int, f: Int -> Int): Bool = c1(f(x), y -> y == 42)

    def c1(x: Int, f: Int -> Bool): Bool = f(x)

    def ping(n: Int, c: Int -> Int): Int = match n {
        case 0 => c(0)
        case 1 => c(1)
        case x => pong(x - 1, z -> c(z + 1))
    }

    def pong(n: Int, c: Int -> Int): Int = match n {
        case 0 => c(0)
        case 1 => c(1)
        case x => ping(x - 1, z -> c(z + 1))
    }

}

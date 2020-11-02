namespace Test/Exp/Postfix {

    @test
    def testPostfix01(): Bool = 123.abs() == 123

    @test
    def testPostfix02(): Bool = (-123).abs() == 123

    @test
    def testPostfix03(): Bool = 123.inc() == 123 + 1

    @test
    def testPostfix04(): Bool = 123.dec() == 123 - 1

    @test
    def testPostfix05(): Bool = 123.inc().inc() == 123 + 2

    @test
    def testPostfix06(): Bool = 123.dec().dec() == 123 - 2

    @test
    def testPostfix07(): Bool = 123.inc().dec() == 123

    @test
    def testPostfix08(): Bool = 123.add(456) == 123 + 456

    @test
    def testPostfix09(): Bool = 123.add(456).add(789) == 123 + 456 + 789

    @test
    def testPostfix10(): Bool = 123.sub(456) == 123 - 456

    @test
    def testPostfix11(): Bool = 123.sub(456).sub(789) == 123 - 456 - 789

    def abs(x: Int): Int = if(x >= 0) x else -x
    def inc(x: Int): Int = x + 1
    def dec(x: Int): Int = x - 1
    def add(x: Int, y: Int): Int = x + y
    def sub(x: Int, y: Int): Int = x - y

}

namespace Test/Exp/Let/Match {

    @test
    def testLetMatch01(): Bool =
        let (x, y) = (1, 2);
            x + y == 1 + 2

    @test
    def testLetMatch02(): Bool =
        let (x, y, z) = (1, 2, 3);
            x + y + z == 1 + 2 + 3

    @test
    def testLetMatch03(): Bool =
        let ((x, y), (z, w)) = ((1, 2), (3, 4));
            x + w + y + z == 1 + 2 + 3 + 4

    @test
    def testLetMatch04(): Bool =
        let Celsius(x) = Celsius(42);
            x == 42

    @test
    def testLetMatch05(): Bool =
        let Fahrenheit(x) = Fahrenheit(42);
            x == 42

    @test
    def testLetMatch06(): Bool =
        let Fahrenheit(x) = Fahrenheit(42);
            x == 42

    @test
    def testLetMatch07(): Bool =
        let Box((x, y)) = Box((1, 2));
            x + y == 1 + 2

    @test
    def testLetMatch08(): Bool =
        let Box((x, y, z)) = Box((1, 2, 3));
            x + y + z == 1 + 2 + 3

    @test
    def testLetMatch09(): Bool =
        let Box((x, y), (z, w)) = Box(((1, 2), (3, 4)));
            x + y + z + w == 1 + 2 + 3 + 4

    @test
    def testLetMatch10(): Bool =
        let Box(Box(x)) = Box(Box(42));
            x == 42

    @test
    def testLetMatch11(): Bool =
        let Box((Box(x), Box(y))) = Box((Box(1), Box(2)));
            x + y == 1 + 2

    @test
    def testLetMatch12(): Bool =
        let Box((Box((x, y)), Box((z, w)))) = Box((Box((1, 2)), Box((3, 4))));
            x + y + z + w == 1 + 2 + 3 + 4

    @test
    def testLetMatch13(): Bool =
        let Box(Celsius(x)) = Box(Celsius(42));
            x == 42

    enum Celsius {
        case Celsius(Int)
    }

    enum Fahrenheit {
        case Fahrenheit(Int)
    }

    enum Box[a] {
        case Box(a)
    }

}

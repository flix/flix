mod Test.Exp.Let.Match {

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
        let Celsius.Celsius(x) = Celsius.Celsius(42);
            x == 42

    @test
    def testLetMatch05(): Bool =
        let Fahrenheit.Fahrenheit(x) = Fahrenheit.Fahrenheit(42);
            x == 42

    @test
    def testLetMatch06(): Bool =
        let Fahrenheit.Fahrenheit(x) = Fahrenheit.Fahrenheit(42);
            x == 42

    @test
    def testLetMatch07(): Bool =
        let Box.Box((x, y)) = Box.Box((1, 2));
            x + y == 1 + 2

    @test
    def testLetMatch08(): Bool =
        let Box.Box((x, y, z)) = Box.Box((1, 2, 3));
            x + y + z == 1 + 2 + 3

    @test
    def testLetMatch09(): Bool =
        let Box.Box((x, y), (z, w)) = Box.Box(((1, 2), (3, 4)));
            x + y + z + w == 1 + 2 + 3 + 4

    @test
    def testLetMatch10(): Bool =
        let Box.Box(Box.Box(x)) = Box.Box(Box.Box(42));
            x == 42

    @test
    def testLetMatch11(): Bool =
        let Box.Box((Box.Box(x), Box.Box(y))) = Box.Box((Box.Box(1), Box.Box(2)));
            x + y == 1 + 2

    @test
    def testLetMatch12(): Bool =
        let Box.Box((Box.Box((x, y)), Box.Box((z, w)))) = Box.Box((Box.Box((1, 2)), Box.Box((3, 4))));
            x + y + z + w == 1 + 2 + 3 + 4

    @test
    def testLetMatch13(): Bool =
        let Box.Box(Celsius.Celsius(x)) = Box.Box(Celsius.Celsius(42));
            x == 42

    enum Celsius {
        case Celsius(Int32)
    }

    enum Fahrenheit {
        case Fahrenheit(Int32)
    }

    enum Box[a] {
        case Box(a)
    }

}

namespace Test/Eff/Polymorphism {

    @test
    def testEffPoly01(): Bool =
        let f = flip((x, y) -> x && y);
        f(true, true)

    @test
    def testEffPoly02(): Bool @ Impure =
        let f = flip((x, y) -> {impure(); x && y});
        f(true, true)

    @test
    def testEffPoly03(): Bool =
        let f = flip(flip((x, y) -> x && y));
        f(true, true)

    @test
    def testEffPoly04(): Bool @ Impure =
        let f = flip(flip((x, y) -> {[1, 2, 3]; x && y}));
        f(true, true)

    @test
    def testEffPoly05(): Bool =
        let f = x -> y -> x && y;
        let g = f(true);
        g(true)

    @test
    def testEffPoly06(): Bool @ Impure =
        let f = x -> y -> {impure(); x && y};
        let g = f(true);
        g(true)

    @test
    def testEffPoly07(): Bool =
        let f = x -> y -> {impure(); x && y};
        let _ = f(true);
        true

    @test
    def testEffPoly08(): Bool @ Impure =
        let f = x -> {impure(); y -> x && y};
        let _ = f(true);
        true

    @test
    def testEffPoly09(): Bool @ Impure =
        let f = x -> {impure(); y -> x && y};
        let g = f(true);
        g(true)

    def impure(): Unit @ Impure = () as _ @ Impure

    def flip(f: (a, b) ->{e} c): (b, a) ->{e} c = (x, y) -> f(y, x)


}


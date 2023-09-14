mod Test.Eff.Polymorphism {

    @Test
    def testEffPoly01(): Bool =
        let f = flip((x, y) -> x and y);
        f(true, true)

    @Test
    def testEffPoly02(): Bool \ IO =
        let f = flip((x, y) -> {impure(); x and y});
        f(true, true)

    @Test
    def testEffPoly03(): Bool =
        let f = flip(flip((x, y) -> x and y));
        f(true, true)

    @Test
    def testEffPoly04(): Bool \ IO =
        let f = flip(flip((x, y) -> {discard Array#{1, 2, 3} @ Static; x and y}));
        f(true, true)

    @Test
    def testEffPoly05(): Bool =
        let f = x -> y -> x and y;
        let g = f(true);
        g(true)

    @Test
    def testEffPoly06(): Bool \ IO =
        let f = x -> y -> {impure(); x and y};
        let g = f(true);
        g(true)

    @Test
    def testEffPoly07(): Bool =
        let f = x -> y -> {impure(); x and y};
        let _ = f(true);
        true

    @Test
    def testEffPoly08(): Bool \ IO =
        let f = x -> {impure(); y -> x and y};
        discard f(true);
        true

    @Test
    def testEffPoly09(): Bool \ IO =
        let f = x -> {impure(); y -> x and y};
        let g = f(true);
        g(true)

    @Test
    def testEffPoly10(): Bool =
        let f = x -> x;
        let g = y -> y;
        (f >> g)(true)

    @Test
    def testEffPoly11(): Bool \ IO =
        let f = x -> {impure(); x};
        let g = y -> y;
        (f >> g)(true)

    @Test
    def testEffPoly12(): Bool \ IO =
        let f = x -> x;
        let g = y -> {impure(); y};
        (f >> g)(true)

    @Test
    def testEffPoly13(): Bool \ IO =
        let f = x -> {impure(); x};
        let g = y -> {impure(); y};
        (f >> g)(true)

    @Test
    def testEffPoly14(): Bool =
        let f = x -> {impure(); x};
        let g = y -> {impure(); y};
        let _ = f >> g;
        true

    @Test
    def testEffPoly15(): Bool =
        let hof = f -> f(true);
        hof(x -> x)

    @Test
    def testEffPoly16(): Bool \ IO =
        let hof = f -> {impure(); f(true)};
        hof(x -> x)

    @Test
    def testEffPoly17(): Bool -> Bool =
        x -> not x

    @Test
    def testEffPoly18(): Bool -> Bool \ IO =
        x -> {impure(); not x}

    @Test
    def testEffPoly19(): Bool -> Bool -> Bool =
        x -> y -> x and y

    @Test
    def testEffPoly20(): Bool -> (Bool -> Bool \ IO) =
        x -> y -> {impure(); x and y}

    @Test
    def testEffPoly21(): Bool -> (Bool -> Bool) \ IO =
        x -> {impure(); y -> x and y}

    @Test
    def testEffPoly22(): Bool -> (Bool -> Bool \ IO) \ IO =
        x -> {impure(); y -> {impure(); x and y}}

    def impure(): Unit \ IO = discard Array#{42} @ Static; ()

    def flip(f: (a, b) -> c \ ef): (b, a) -> c \ ef = (x, y) -> f(y, x)

}

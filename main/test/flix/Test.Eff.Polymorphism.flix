mod Test.Eff.Polymorphism {

    use Assert.assertTrue

    @Test
    def testEffPoly01(): Unit \ Assert =
        let f = flip((x, y) -> x and y);
        assertTrue(f(true, true))

    @Test
    def testEffPoly02(): Unit \ Assert + IO =
        let f = flip((x, y) -> {io(); x and y});
        assertTrue(f(true, true))

    @Test
    def testEffPoly03(): Unit \ Assert =
        let f = flip(flip((x, y) -> x and y));
        assertTrue(f(true, true))

    @Test
    def testEffPoly04(): Unit \ Assert + IO =
        let f = flip(flip((x, y) -> {discard Array#{1, 2, 3} @ Static; x and y}));
        assertTrue(f(true, true))

    @Test
    def testEffPoly05(): Unit \ Assert =
        let f = x -> y -> x and y;
        let g = f(true);
        assertTrue(g(true))

    @Test
    def testEffPoly06(): Unit \ Assert + IO =
        let f = x -> y -> {io(); x and y};
        let g = f(true);
        assertTrue(g(true))

    @Test
    def testEffPoly07(): Unit \ Assert =
        let f = x -> y -> {io(); x and y};
        let _ = f(true);
        assertTrue(true)

    @Test
    def testEffPoly08(): Unit \ Assert + IO =
        let f = x -> {io(); y -> x and y};
        discard f(true);
        assertTrue(true)

    @Test
    def testEffPoly09(): Unit \ Assert + IO =
        let f = x -> {io(); y -> x and y};
        let g = f(true);
        assertTrue(g(true))

    @Test
    def testEffPoly10(): Unit \ Assert =
        let f = x -> x;
        let g = y -> y;
        assertTrue((f >> g)(true))

    @Test
    def testEffPoly11(): Unit \ Assert + IO =
        let f = x -> {io(); x};
        let g = y -> y;
        assertTrue((f >> g)(true))

    @Test
    def testEffPoly12(): Unit \ Assert + IO =
        let f = x -> x;
        let g = y -> {io(); y};
        assertTrue((f >> g)(true))

    @Test
    def testEffPoly13(): Unit \ Assert + IO =
        let f = x -> {io(); x};
        let g = y -> {io(); y};
        assertTrue((f >> g)(true))

    @Test
    def testEffPoly14(): Unit \ Assert =
        let f = x -> {io(); x};
        let g = y -> {io(); y};
        let _ = f >> g;
        assertTrue(true)

    @Test
    def testEffPoly15(): Unit \ Assert =
        let hof = f -> f(true);
        assertTrue(hof(x -> x))

    @Test
    def testEffPoly16(): Unit \ Assert + IO =
        let hof = f -> {io(); f(true)};
        assertTrue(hof(x -> x))

    @CompileTest
    def testEffPoly17(): Bool -> Bool =
        x -> not x

    @CompileTest
    def testEffPoly18(): Bool -> Bool \ IO =
        x -> {io(); not x}

    @CompileTest
    def testEffPoly19(): Bool -> Bool -> Bool =
        x -> y -> x and y

    @CompileTest
    def testEffPoly20(): Bool -> (Bool -> Bool \ IO) =
        x -> y -> {io(); x and y}

    @CompileTest
    def testEffPoly21(): Bool -> (Bool -> Bool) \ IO =
        x -> {io(); y -> x and y}

    @CompileTest
    def testEffPoly22(): Bool -> (Bool -> Bool \ IO) \ IO =
        x -> {io(); y -> {io(); x and y}}

    def io(): Unit \ IO = discard Array#{42} @ Static; ()

    def flip(f: (a, b) -> c \ ef): (b, a) -> c \ ef = (x, y) -> f(y, x)

}

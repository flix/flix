namespace Test/Exp/Par {

    def f(x: Int32): Bool = x == 1

    @test
    def testPar01(): (Int32, Int32, Int32) =
        par (1, 2, 3)

    @test
    def testPar02(): Bool =
        (par (1, 2, 3)) == (1, 2, 3)

    @test
    def testPar03(): Bool =
        (par (f(1), f(2), f(3))) == (true, false, false)

    @test
    def testPar04(): Bool =
        (par (par (1, 2, 3), par (4, 5, 6), par (7, 8, 9))) == ((1, 2, 3), (4, 5, 6), (7, 8, 9))

    @test
    def testPar05(): Bool =
        ((par (1, 2)) :: (par (3, 4)) :: (par (5, 6)) :: Nil) == (1, 2) :: (3, 4) :: (5, 6) :: Nil

    @test
    def testPar06(): Bool =
        ((par ((1, 2), par (3, 4))) :: (par ((5, 6), par (7, 8))) :: (par (par (9, 10), par (11, 12))) :: Nil) ==
            ((1, 2), (3, 4)) :: ((5, 6), (7, 8)) :: ((9, 10), (11, 12)) :: Nil

    @test
    def testPar07(): Bool =
        ((par ((1, 2), par (3, 4))) :: (par ((5, 6), par (7, 8))) :: (par (par (9, 10), par (11, 12))) :: Nil) ==
            (par ((1, 2), par (3, 4))) :: (par ((5, 6), par (7, 8))) :: (par (par (9, 10), par (11, 12))) :: Nil

    @test
    def testPar08(): Bool =
        par f(1)

    @test
    def testPar09(): Bool =
        let f = (a, b, c) -> a and b and c;
        let a = () -> List.range(0, 2000) |> List.forall(x -> x > -1);
        let b = () -> a();
        let c = () -> b();
        par f(a(), b(), c())

    @test
    def testPar10(): Bool =
        let f = (a, b, c) -> a and b and c;
        let a = () -> List.range(0, 2000) |> List.forall(x -> x > -1);
        let b = () -> a();
        let c = () -> b();
        par f(par a(), par b(), par c())

    @test
    def testPar11(): Bool =
        let f = a -> fst(a) and snd(a);
        let a = () -> List.range(0, 2000) |> List.forall(x -> x > -1);
        let b = () -> a();
        par f(par (par a(), par b()))

    @test
    def testPar12(): Bool =
        let g = (a, b, c) -> a and b and c;
        let a = () -> List.range(0, 2000) |> List.forall(x -> x > -1);
        let b = () -> List.range(0, 2000) |> List.forall(x -> x > -1);
        par g(true, a(), b()) // Should spawn threads for a and b apply exps

    @test
    def testPar13(): Bool =
        let g = (a, b, c) -> a and b and c;
        par g(true, true, true) // Should not spawn any new threads

}

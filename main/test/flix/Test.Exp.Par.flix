mod Test.Exp.Par {

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
    def testPar08(): (Int32, Int32) =
        let x = 1;
        par (x, 10)

    @test
    def testPar09(): (Int32, Int32, Bool) =
        let x = 2;
        par (x, 10, f(10))

    @test
    def testPar10(): (Bool, Int32, Int32, Bool) =
        let x = 2;
        par (f(10), x, 10, f(10))

    @test
    def testPar11(): (Int32, Bool, Int32, Int32, Bool) =
        let x = 2;
        par (x, f(1), x, 10, f(10))

    @test
    def testPar12(): (Bool, Bool, Int32, Int32, Bool) =
        let x = 2;
        par (f(1), f(1), 4, x, f(10))

    @test
    def testPar13(): (Bool, Bool, Int32, Int32) =
        let x = 2;
        par (f(1), f(1), 4, x)

    @test
    def testPar14(): (Bool, Int32, Int32, Int32, Bool, Int32, Int32, Bool, Int32, Int32) =
        let x = 1;
        par (f(1), 1, 2, x, f(2), x, 2, f(3), x, 2)

    @test
    def testPar15(): Bool =
        let x = 1;
        (par (f(1), 1, 2, x, f(2), x, 2, f(3), x, 2)) == (true, 1, 2, 1, false, 1, 2, false, 1, 2)

}

mod Test.Exp.Match.List {

    // Define an enum with a `Cons` tag to ensure that it's not used in any of the following tests
    pub enum Foo {
        case Cons
    }

    @test
    def testMatchList01(): Bool = match Nil {
        case Nil => true
        case _   => false
    }

    @test
    def testMatchList02(): Bool = match Nil {
        case Nil      => true
        case 1 :: Nil => false
        case _        => false
    }

    @test
    def testMatchList03(): Bool = match Nil {
        case Nil           => true
        case 1 :: Nil      => false
        case 1 :: 2 :: Nil => false
        case _             => false
    }

    @test
    def testMatchList04(): Bool = match 1 :: Nil {
        case Nil      => false
        case 1 :: Nil => true
        case _        => false
    }

    @test
    def testMatchList05(): Bool = match 1 :: 2 :: Nil {
        case Nil      => false
        case _ :: _   => true
    }

    @test
    def testMatchList06(): Bool = match 1 :: 2 :: Nil {
        case Nil            => false
        case 1 :: Nil       => false
        case 1 :: 2 :: Nil  => true
        case _              => false
    }

    @test
    def testMatchList07(): Bool = match 1 :: 2 :: Nil {
        case x :: y :: Nil  => x + 1 == y
        case _              => false
    }

    @test
    def testMatchList08(): Bool = match 1 :: 2 :: 3 :: Nil {
        case x :: 2 :: z :: Nil => x + 2 == z
        case _                  => false
    }

    @test
    def testPatternOption01(): Bool = match Nil {
        case Nil => true
        case _   => false
    }

    @test
    def testPatternOption02(): Bool = match None :: Nil {
        case Nil         => false
        case None :: Nil => true
        case _           => false
    }

    @test
    def testPatternOption03(): Bool = match Some(123) :: Nil {
        case Nil              => false
        case Some(x) :: Nil   => x == 123
        case _                => false
    }

    @test
    def testListList01(): Bool = match (Nil : List[List[Int32]]) {
        case Nil              => true
        case _                => false
    }

    @test
    def testListList02(): Bool = match (1 :: Nil) :: Nil {
        case Nil                => false
        case (1 :: Nil) :: Nil  => true
        case _                  => false
    }

    @test
    def testListList03(): Bool = match (1 :: Nil) :: (2 :: Nil) :: Nil {
        case Nil                             => false
        case (x :: Nil) :: (y :: Nil) :: Nil => x + 1 == y
        case _                               => false
    }

}

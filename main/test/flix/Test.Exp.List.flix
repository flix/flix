mod Test.Exp.List {

    // Define an enum with a `Cons` tag to ensure that it's not used in any of the following tests
    pub enum Foo {
        case Cons
    }

    @test
    def testList01(): List[Int32] = Nil

    @test
    def testList02(): List[Int32] = 1 :: Nil

    @test
    def testList03(): List[Int32] = 1 :: 2 :: Nil

    @test
    def testListOption01(): List[Option[Int32]] = Nil

    @test
    def testListOption02(): List[Option[Int32]] = None :: Nil

    @test
    def testListOption03(): List[Option[Int32]] = None :: None :: Nil

    @test
    def testListOption04(): List[Option[Int32]] = None :: Some(42) :: Nil

    @test
    def testListOption05(): List[Option[Int32]] = Some(21) :: Some(42) :: Nil

    @test
    def testListList01(): List[List[Int32]] = Nil

    @test
    def testListList02(): List[List[Int32]] = Nil :: Nil

    @test
    def testListList03(): List[List[Int32]] = (1 :: Nil) :: Nil

    @test
    def testListList04(): List[List[Int32]] = (1 :: Nil) :: (2 :: Nil) :: Nil

    @test
    def testListList05(): List[List[Int32]] = (1 :: Nil) :: (2 :: Nil) :: (Nil) :: Nil

    @test
    def testListAppend01(): List[Int32] = Nil ::: Nil

    @test
    def testListAppend02(): List[Int32] = 1 :: Nil ::: 2 :: Nil

    @test
    def testListAppend03(): List[Int32] = 1 :: 2 :: Nil ::: 3 :: 4 :: Nil

    @test
    def testListAppend05(): List[Int32] = Nil ::: Nil ::: Nil

    @test
    def testListAppend06(): List[Int32] = 1 :: Nil ::: 2 :: Nil ::: 3 :: Nil

}

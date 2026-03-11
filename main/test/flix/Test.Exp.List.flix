mod Test.Exp.List {

    // Define an enum with a `Cons` tag to ensure that it's not used in any of the following tests
    pub enum Foo {
        case Cons
    }

    @CompileTest
    def testList01(): List[Int32] = Nil

    @CompileTest
    def testList02(): List[Int32] = 1 :: Nil

    @CompileTest
    def testList03(): List[Int32] = 1 :: 2 :: Nil

    @CompileTest
    def testListOption01(): List[Option[Int32]] = Nil

    @CompileTest
    def testListOption02(): List[Option[Int32]] = None :: Nil

    @CompileTest
    def testListOption03(): List[Option[Int32]] = None :: None :: Nil

    @CompileTest
    def testListOption04(): List[Option[Int32]] = None :: Some(42) :: Nil

    @CompileTest
    def testListOption05(): List[Option[Int32]] = Some(21) :: Some(42) :: Nil

    @CompileTest
    def testListList01(): List[List[Int32]] = Nil

    @CompileTest
    def testListList02(): List[List[Int32]] = Nil :: Nil

    @CompileTest
    def testListList03(): List[List[Int32]] = (1 :: Nil) :: Nil

    @CompileTest
    def testListList04(): List[List[Int32]] = (1 :: Nil) :: (2 :: Nil) :: Nil

    @CompileTest
    def testListList05(): List[List[Int32]] = (1 :: Nil) :: (2 :: Nil) :: (Nil) :: Nil

    @CompileTest
    def testListAppend01(): List[Int32] = Nil ::: Nil

    @CompileTest
    def testListAppend02(): List[Int32] = 1 :: Nil ::: 2 :: Nil

    @CompileTest
    def testListAppend03(): List[Int32] = 1 :: 2 :: Nil ::: 3 :: 4 :: Nil

    @CompileTest
    def testListAppend05(): List[Int32] = Nil ::: Nil ::: Nil

    @CompileTest
    def testListAppend06(): List[Int32] = 1 :: Nil ::: 2 :: Nil ::: 3 :: Nil

}

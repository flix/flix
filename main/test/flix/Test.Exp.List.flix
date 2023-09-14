mod Test.Exp.List {

    // Define an enum with a `Cons` tag to ensure that it's not used in any of the following tests
    pub enum Foo {
        case Cons
    }

    @Test
    def testList01(): List[Int32] = Nil

    @Test
    def testList02(): List[Int32] = 1 :: Nil

    @Test
    def testList03(): List[Int32] = 1 :: 2 :: Nil

    @Test
    def testListOption01(): List[Option[Int32]] = Nil

    @Test
    def testListOption02(): List[Option[Int32]] = None :: Nil

    @Test
    def testListOption03(): List[Option[Int32]] = None :: None :: Nil

    @Test
    def testListOption04(): List[Option[Int32]] = None :: Some(42) :: Nil

    @Test
    def testListOption05(): List[Option[Int32]] = Some(21) :: Some(42) :: Nil

    @Test
    def testListList01(): List[List[Int32]] = Nil

    @Test
    def testListList02(): List[List[Int32]] = Nil :: Nil

    @Test
    def testListList03(): List[List[Int32]] = (1 :: Nil) :: Nil

    @Test
    def testListList04(): List[List[Int32]] = (1 :: Nil) :: (2 :: Nil) :: Nil

    @Test
    def testListList05(): List[List[Int32]] = (1 :: Nil) :: (2 :: Nil) :: (Nil) :: Nil

    @Test
    def testListAppend01(): List[Int32] = Nil ::: Nil

    @Test
    def testListAppend02(): List[Int32] = 1 :: Nil ::: 2 :: Nil

    @Test
    def testListAppend03(): List[Int32] = 1 :: 2 :: Nil ::: 3 :: 4 :: Nil

    @Test
    def testListAppend05(): List[Int32] = Nil ::: Nil ::: Nil

    @Test
    def testListAppend06(): List[Int32] = 1 :: Nil ::: 2 :: Nil ::: 3 :: Nil

}

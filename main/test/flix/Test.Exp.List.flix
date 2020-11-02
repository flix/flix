namespace Test/Exp/List {

    @test
    def testList01(): List[Int] = Nil

    @test
    def testList02(): List[Int] = 1 :: Nil

    @test
    def testList03(): List[Int] = 1 :: 2 :: Nil

    @test
    def testListOption01(): List[Option[Int]] = Nil

    @test
    def testListOption02(): List[Option[Int]] = None :: Nil

    @test
    def testListOption03(): List[Option[Int]] = None :: None :: Nil

    @test
    def testListOption04(): List[Option[Int]] = None :: Some(42) :: Nil

    @test
    def testListOption05(): List[Option[Int]] = Some(21) :: Some(42) :: Nil

    @test
    def testListList01(): List[List[Int]] = Nil

    @test
    def testListList02(): List[List[Int]] = Nil :: Nil

    @test
    def testListList03(): List[List[Int]] = (1 :: Nil) :: Nil

    @test
    def testListList04(): List[List[Int]] = (1 :: Nil) :: (2 :: Nil) :: Nil

    @test
    def testListList05(): List[List[Int]] = (1 :: Nil) :: (2 :: Nil) :: (Nil) :: Nil

    @test
    def testListAppend01(): List[Int] = Nil ::: Nil

    @test
    def testListAppend02(): List[Int] = 1 :: Nil ::: 2 :: Nil

    @test
    def testListAppend03(): List[Int] = 1 :: 2 :: Nil ::: 3 :: 4 :: Nil

    @test
    def testListAppend05(): List[Int] = Nil ::: Nil ::: Nil

    @test
    def testListAppend06(): List[Int] = 1 :: Nil ::: 2 :: Nil ::: 3 :: Nil

}

enum Option[t] {
    case None,
    case Some(t)
}

enum List[t] {
    case Nil,
    case Cons(t, List[t])
}

namespace List {

    pub def append(xs: List[a], ys: List[a]): List[a] = {
        foldRight((x, acc) -> x :: acc, ys, xs)
    }

}

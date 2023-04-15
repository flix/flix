mod Test.Equality.Tag {

    @test
    def testEqTag01(): Bool = None: Option[Unit] == None

    @test
    def testEqTag02(): Bool = Some(42) == Some(42)

    @test
    def testEqTag03(): Bool = Ok(42): Result[Unit, _] == Ok(42)

    @test
    def testEqTag04(): Bool = Err(42): Result[_, Unit] == Err(42)

    @test
    def testEqTag05(): Bool = Nil: List[Unit] == Nil

    @test
    def testEqTag06(): Bool = (42 :: Nil) == (42 :: Nil)

    @test
    def testEqTag07(): Bool = Some(Ok(42)): Option[Result[Unit, _]] == Some(Ok(42))

    @test
    def testEqTag08(): Bool = Ok(Some(42)): Result[Unit, _] == Ok(Some(42))

    @test
    def testEqTag09(): Bool = Some((Ok(1) :: Err(2) :: Nil)) == Some((Ok(1) :: Err(2) :: Nil))

    @test
    def testNeqTag01(): Bool = None != Some(42)

    @test
    def testNeqTag02(): Bool = Some(21) != Some(42)

    @test
    def testNeqTag03(): Bool = Ok(21): Result[Unit, _] != Ok(42)

    @test
    def testNeqTag04(): Bool = Err(21): Result[_, Unit] != Err(42)

    @test
    def testNeqTag05(): Bool = Nil != (42 :: Nil)

    @test
    def testNeqTag06(): Bool = (21 :: Nil) != (42 :: Nil)

    @test
    def testNeqTag07(): Bool = Some(Ok(21)) != Some(Err(42))

    @test
    def testNeqTag08(): Bool = Ok(None): Result[Unit, Option[Int32]] != Ok(Some(42))

    @test
    def testNeqTag09(): Bool = Some((Ok(1) :: Err(2) :: Nil)) != Some((Err(1) :: Ok(2) :: Nil))

}

namespace Test/Exp/LetMatchStar {

    @test
    def testOption01(): Option[Int] =
        let* _ = None;
        None

    @test
    def testOption02(): Option[Int] =
        let* _ = Some(123);
        None

    @test
    def testOption03(): Option[Int] =
        let* x = None;
        Some(x + 1)

    @test
    def testOption04(): Option[Int] =
        let* x = Some(123);
        Some(x + 1)

    @test
    def testOption05(): Option[Int] =
        let* _ = Some(123);
        let* _ = Some(456);
        None

    @test
    def testOption06(): Option[Int] =
        let* x = Some(123);
        let* _ = Some(456);
        Some(x + 1)

    @test
    def testOption07(): Option[Int] =
      let* _ = Some(123);
      let* y = Some(456);
      Some(y + 1)

    @test
    def testOption08(): Option[Int] =
      let* x = Some(123);
      let* y = Some(456);
      Some(x + y)

    @test
    def testOption09(): Option[Int] =
      let* x = Some(123);
      let y = 456;
      let* z = Some(789);
      let w = 321;
      Some(x + y + z + w)

    @test
    def testOption10(): Option[Int] =
        let* x = Some(123);
        let* y = Some(456);
        Monad.point(x + y)

    @test
    def testPatMatch01(): Option[Int] =
        let* (x, y, z) = Some((1, true, 3));
        if (y) Some(x) else Some(z)

    @test
    def testPatMatch02(): Option[Int] =
        let* (x, y, z) = Some((1, 2, 3));
        Some(x + y + z)

    @test
    def testPatMatch03(): Option[Int] =
        let* (x, y): (Int, Int) = Some((1, 2));
        Some(x + y)

    @test
    def testPatMatch04(): Option[Int] =
        let* (x, y, z): (Int, Bool, Int) = Some((1, true, 3));
        if (y) Some(x) else Some(z)

}

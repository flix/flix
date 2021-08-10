namespace Test/Exp/LetMatchStar {

    @test
    def testOption01(): Option[Int] =
        use Option.map;
        let+ _ = None;
        0

    @test
    def testOption02(): Option[Int] =
        use Option.map;
        let+ _ = Some(123);
        0

    @test
    def testOption03(): Option[Int] =
        use Option.map;
        let+ x = None;
        x + 1

    @test
    def testOption04(): Option[Int] =
        use Option.map;
        let+ x = Some(123);
        x + 1

    @test
    def testOption05(): Option[Int] =
        use Option.{flatMap, map};
        let* _ = Some(123);
        let+ _ = Some(456);
        0

    @test
    def testOption06(): Option[Int] =
        use Option.{flatMap, map};
        let* x = Some(123);
        let+ _ = Some(456);
        x + 1

    @test
    def testOption07(): Option[Int] =
      use Option.{flatMap, map};
      let* _ = Some(123);
      let+ y = Some(456);
      y + 1

    @test
    def testOption08(): Option[Int] =
      use Option.{flatMap, map};
      let* x = Some(123);
      let+ y = Some(456);
      x + y

    @test
    def testOption09(): Option[Int] =
      use Option.{flatMap, map};
      let* x = Some(123);
      let y = 456;
      let+ z = Some(789);
      let w = 321;
      x + y + z + w

    @test
    def testResult01(): Result[String, String] =
        use Result.map;
        let+ _ = Err("Hello World");
        "Hello World"

    @test
    def testResult02(): Result[String, String] =
        use Result.map;
        let+ _ = Ok(123);
        "Hello World"

    @test
    def testResult03(): Result[Int, String] =
        use Result.map;
        let+ x = Err("Hello World");
        x + 1

    @test
    def testResult04(): Result[Int, String] =
        use Result.map;
        let+ x = Ok(123);
        x + 1

    @test
    def testResult05(): Result[String, String] =
        use Result.{flatMap, map};
        let* _ = Ok(123);
        let+ _ = Ok(456);
        "Hello World"

    @test
    def testResult06(): Result[Int, String] =
        use Result.{flatMap, map};
        let* x = Ok(123);
        let+ _ = Ok(456);
        x + 1

    @test
    def testResult07(): Result[Int, String] =
      use Result.{flatMap, map};
      let* _ = Ok(123);
      let+ y = Ok(456);
      y + 1

    @test
    def testResult08(): Result[Int, String] =
      use Result.{flatMap, map};
      let* x = Ok(123);
      let+ y = Ok(456);
      x + y

    @test
    def testResult09(): Result[Int, String] =
      use Result.{flatMap, map};
      let* x = Ok(123);
      let y = 456;
      let+ z = Ok(789);
      let w = 321;
      x + y + z + w

    @test
    def testPatMatch01(): Option[Int] =
        use Option.map;
        let+ (x, y, z) = Some((1, true, 3));
        if (y) x else z

    @test
    def testPatMatch02(): Option[Int] =
        use Option.map;
        let+ (x, y, z) = Some((1, 2, 3));
        x + y + z

    @test
    def testPatMatch03(): Option[Int] =
        use Option.map;
        let+ (x, y): (Int, Int) = Some((1, 2));
        x + y

    @test
    def testPatMatch04(): Option[Int] =
        use Option.map;
        let+ (x, y, z): (Int, Bool, Int) = Some((1, true, 3));
        if (y) x else z

}

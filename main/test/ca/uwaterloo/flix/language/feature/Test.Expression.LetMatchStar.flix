namespace Test/Expression/LetMatchStar {

    @test
    def testOption01(): Option[Int] =
        let flatMap = Option.flatMap;
        let* _ = None;
        None

    @test
    def testOption02(): Option[Int] =
        let flatMap = Option.flatMap;
        let* _ = Some(123);
        None

    @test
    def testOption03(): Option[Int] =
        let flatMap = Option.flatMap;
        let* x = None;
        Some(x + 1)

    @test
    def testOption04(): Option[Int] =
        let flatMap = Option.flatMap;
        let* x = Some(123);
        Some(x + 1)

    @test
    def testOption05(): Option[Int] =
        let flatMap = Option.flatMap;
        let* _ = Some(123);
        let* _ = Some(456);
        None

    @test
    def testOption06(): Option[Int] =
        let flatMap = Option.flatMap;
        let* x = Some(123);
        let* _ = Some(456);
        Some(x + 1)

    @test
    def testOption07(): Option[Int] =
      let flatMap = Option.flatMap;
      let* _ = Some(123);
      let* y = Some(456);
      Some(y + 1)

    @test
    def testOption08(): Option[Int] =
      let flatMap = Option.flatMap;
      let* x = Some(123);
      let* y = Some(456);
      Some(x + y)

    @test
    def testOption09(): Option[Int] =
      let flatMap = Option.flatMap;
      let* x = Some(123);
      let y = 456;
      let* z = Some(789);
      let w = 321;
      Some(x + y + z + w)

    @test
    def testResult01(): Result[Int, Str] =
        let flatMap = Result.flatMap;
        let* _ = Err("Hello World");
        Err("Hello World")

    @test
    def testResult02(): Result[Int, Str] =
        let flatMap = Result.flatMap;
        let* _ = Ok(123);
        Err("Hello World")

    @test
    def testResult03(): Result[Int, Str] =
        let flatMap = Result.flatMap;
        let* x = Err("Hello World");
        Ok(x + 1)

    @test
    def testResult04(): Result[Int, Str] =
        let flatMap = Result.flatMap;
        let* x = Ok(123);
        Ok(x + 1)

    @test
    def testResult05(): Result[Int, Str] =
        let flatMap = Result.flatMap;
        let* _ = Ok(123);
        let* _ = Ok(456);
        Err("Hello World")

    @test
    def testResult06(): Result[Int, Str] =
        let flatMap = Result.flatMap;
        let* x = Ok(123);
        let* _ = Ok(456);
        Ok(x + 1)

    @test
    def testResult07(): Result[Int, Str] =
      let flatMap = Result.flatMap;
      let* _ = Ok(123);
      let* y = Ok(456);
      Ok(y + 1)

    @test
    def testResult08(): Result[Int, Str] =
      let flatMap = Result.flatMap;
      let* x = Ok(123);
      let* y = Ok(456);
      Ok(x + y)

    @test
    def testResult09(): Result[Int, Str] =
      let flatMap = Result.flatMap;
      let* x = Ok(123);
      let y = 456;
      let* z = Ok(789);
      let w = 321;
      Ok(x + y + z + w)

}

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




}

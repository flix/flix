namespace Test/Expression/LetMatchStar {

    @test
    def test01(): Option[Int] =
        let flatMap = Option.flatMap;
        let* _ = None;
        None

    @test
    def test02(): Option[Int] =
        let flatMap = Option.flatMap;
        let* _ = Some(123);
        None

    @test
    def test03(): Option[Int] =
        let flatMap = Option.flatMap;
        let* x = None;
        Some(x + 1)

    @test
    def test04(): Option[Int] =
        let flatMap = Option.flatMap;
        let* x = Some(123);
        Some(x + 1)









    @test
    def test06(): Result[Bool, Path.IOError] =
        let flatMap = Result.flatMap;
        let p = Path.new("flafgnyf.txt");
        let* isReadable = Path.isReadable(p);
        let* isWritable = Path.isWritable(p);
        let* isExecutable = Path.isExecutable(p);
            Result.Ok(isReadable && isWritable && isExecutable)


}

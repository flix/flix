namespace Test/Term/Lit/Result {

    @test
    def test01(): #{ R(Result[Int, String]) } =
        let x = R(Ok(123)).;
        solve x

    @test
    def test02(): #{ R(Result[Int, String]) } =
        let x = R(Err("hello world!")).;
        solve x

    @test
    def test03(): #{ R(Result[Int, String]) } =
        let x = R(Ok(123)).;
        let y = R(Err("hello world!")).;
        solve x <+> y

    @test
    def test04(): #{ R(Result[Int, String]) } =
        let x = R(Ok(123)).;
        let y = R(Ok(456)).;
        let z = R(Err("hello world!")).;
        solve x <+> y <+> z

    @test
    def test05(): #{ R(Result[Int, String]) } =
        let x = R(Ok(123)).;
        let y = R(Err("hello world!")).;
        let z = R(Err("Goodbye world!")).;
        solve x <+> y <+> z

    @test
    def test06(): #{ R(Result[Int, String]) } =
        let x = R(Ok(123)).;
        let y = R(Ok(456)).;
        let z = R(Err("hello world!")).;
        let w = R(Err("Goodbye world!")).;
        solve x <+> y <+> z <+> w

    enum Result[t, e] {
        case Ok(t),
        case Err(e)
    }

    instance ToString[Result[t, e]] with [t : ToString, e : ToString] {
        def toString(r: Result[t, e]): String = ???
    }

}

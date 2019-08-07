namespace Test/Term/Lit/Result {

    rel R(x: Result[Int, Str])
    
    @test
    def test01(): #{ R } =
        let x = R(Ok(123)).;
        solve x
    
    @test
    def test02(): #{ R } =
        let x = R(Err("hello world!")).;
        solve x
    
    @test
    def test03(): #{ R } =
        let x = R(Ok(123)).;
        let y = R(Err("hello world!")).;
        solve x <+> y
    
    @test
    def test04(): #{ R } =
        let x = R(Ok(123)).;
        let y = R(Ok(456)).;
        let z = R(Err("hello world!")).;
        solve x <+> y <+> z
    
    @test
    def test05(): #{ R } =
        let x = R(Ok(123)).;
        let y = R(Err("hello world!")).;
        let z = R(Err("Goodbye world!")).;
        solve x <+> y <+> z
    
    @test
    def test06(): #{ R } =
        let x = R(Ok(123)).;
        let y = R(Ok(456)).;
        let z = R(Err("hello world!")).;
        let w = R(Err("Goodbye world!")).;
        solve x <+> y <+> z <+> w

}

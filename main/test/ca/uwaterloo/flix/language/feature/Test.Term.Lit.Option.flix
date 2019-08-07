namespace Test/Term/Lit/Option {

    rel R(x: Option[Int])
    
    @test
    def test01(): #{ R } =
        let x = R(None).;
        solve x
    
    @test
    def test02(): #{ R } =
        let x = R(Some(123)).;
        solve x
    
    @test
    def test03(): #{ R } =
        let x = R(None).;
        let y = R(Some(123)).;
        solve x <+> y
    
    @test
    def test04(): #{ R } =
        let x = R(None).;
        let y = R(Some(123)).;
        let z = R(Some(456)).;
        solve x <+> y <+> z


}

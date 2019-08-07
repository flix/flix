namespace Test/Term/Lit/List {

    rel R(x: List[Int])
    
    @test
    def test01(): #{ R } =
        let x = R(Nil).;
        solve x
    
    @test
    def test02(): #{ R } =
        let x = R(1 :: Nil).;
        solve x
    
    @test
    def test03(): #{ R } =
        let x = R(Nil).;
        let y = R(1 :: Nil).;
        solve x <+> y
    
    @test
    def test04(): #{ R } =
        let x = R(Nil).;
        let y = R(1 :: Nil).;
        let z = R(1 :: 2 :: Nil).;
        solve x <+> y <+> z
    
    @test
    def test05(): #{ R } =
        let x = R(Nil).;
        let y = R(1 :: Nil).;
        let z = R(1 :: 2 :: Nil).;
        let w = R(1 :: 2 :: 3 :: Nil).;
        solve x <+> y <+> z <+> w

}

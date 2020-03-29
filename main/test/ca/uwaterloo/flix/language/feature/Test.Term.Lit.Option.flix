namespace Test/Term/Lit/Option {

    rel R(x: Option[Int])
    
//    @test
//    def test01(): #{ R } =
//        let x = R(None).;
//        solve x
//
    @test
    def test02(): #{ R } =
        let x = R(Some(123)).;
        solve x



}

namespace Test/Term/Lit/Set {

    @test
    def test01(): #{ R(Set[Int]) } =
        let x = R(Set#{}).;
        solve x

    @test
    def test02(): #{ R(Set[Int]) } =
        let x = R(Set#{42}).;
        solve x

    @test
    def test03(): #{ R(Set[Int]) } =
        let x = R(Set#{1, 2, 3}).;
        solve x

    @test
    def test04(): #{ R(Set[Int]) } =
        let x = R(Set#{1}).;
        let y = R(Set#{2}).;
        solve x <+> y

    @test
    def test05(): #{ R(Set[Int]) } =
        let x = R(Set#{1}).;
        let y = R(Set#{2}).;
        let z = R(Set#{3}).;
        solve x <+> y <+> z

    @test
    def test06(): #{ R(Set[Int]) } =
        let x = R(Set#{1, 2, 3}).;
        let y = R(Set#{4, 5, 6}).;
        let z = R(Set#{1, 2, 3}).;
        solve x <+> y <+> z

}

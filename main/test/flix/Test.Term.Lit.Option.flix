namespace Test/Term/Lit/Option {

    @test
    def test01(): #{ R(Option[Int]) } =
        let x = R(None).;
        solve x

    @test
    def test02(): #{ R(Option[Int]) } =
        let x = R(Some(123)).;
        solve x


    @test
    def test03(): #{ R(Option[Int]) } =
        let x = R(None).;
        let y = R(Some(123)).;
        solve x <+> y

    @test
    def test04(): #{ R(Option[Int]) } =
        let x = R(None).;
        let y = R(Some(123)).;
        let z = R(Some(456)).;
        solve x <+> y <+> z

    enum Option[t] {
        case None,
        case Some(t)
    }

    instance ToString[Option[a]] with [a : ToString] {
        def toString(o: Option[a]): String = match o {
            case None    => "None"
            case Some(y) => "Some(${y})"
        }
    }

}

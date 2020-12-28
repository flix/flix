namespace Test/Term/Lit/List {

    @test
    def test01(): #{ R(List[Int]) } =
        let x = R(Nil).;
        solve x

    @test
    def test02(): #{ R(List[Int]) } =
        let x = R(1 :: Nil).;
        solve x

    @test
    def test03(): #{ R(List[Int]) } =
        let x = R(Nil).;
        let y = R(1 :: Nil).;
        solve x <+> y

    @test
    def test04(): #{ R(List[Int]) } =
        let x = R(Nil).;
        let y = R(1 :: Nil).;
        let z = R(1 :: 2 :: Nil).;
        solve x <+> y <+> z

    @test
    def test05(): #{ R(List[Int]) } =
        let x = R(Nil).;
        let y = R(1 :: Nil).;
        let z = R(1 :: 2 :: Nil).;
        let w = R(1 :: 2 :: 3 :: Nil).;
        solve x <+> y <+> z <+> w

    enum List[t] {
        case Nil,
        case Cons(t, List[t])
    }

    instance ToString[List[a]] with [a : ToString] {
        def toString(xs: List[a]): String = ???
    }

}

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

    instance Eq[Option[a]] with [a : Eq] {
        pub def eq(o1: Option[a], o2: Option[a]): Bool = match (o1, o2) {
            case (None, None)           => true
            case (Some(v1), Some(v2))   => v1 == v2
            case _                      => false
        }
    }

    instance Hash[Option[a]] with [a : Hash] {
        pub def hash(o: Option[a]): Int = match o {
            case None       => 1046527
            case Some(x)    => 919 * Hash.hash(x)
        }
    }

    instance ToString[Option[a]] with [a : ToString] {
        pub def toString(o: Option[a]): String = match o {
            case None       => "None"
            case Some(x)    => "Some(${x})"
        }
    }

}

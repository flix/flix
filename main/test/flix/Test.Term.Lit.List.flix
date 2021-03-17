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

    instance Eq[List[a]] with [a : Eq] {
        pub def eq(xs: List[a], ys: List[a]): Bool = match (xs, ys) {
            case (Nil, Nil) => true
            case (x :: rs, y :: qs) => if (x != y) false else rs == qs
            case _ => false
        }
        pub def neq(xs: List[a], ys: List[a]): Bool = not (xs == ys)
    }

    instance Hash[List[a]] with [a : Hash] {
        pub def hash(xs: List[a]): Int =
            foldLeft((acc, x) -> acc * 31 + Hash.hash(x), 37199, xs)
    }

    pub def foldLeft(f: (b, a) -> b & e, s: b, xs: List[a]): b & e = match xs {
        case Nil => s
        case x :: rs => foldLeft(f, f(s, x), rs)
    }

    instance ToString[List[a]] with [a : ToString] {
        pub def toString(xs: List[a]): String = ???
    }

}


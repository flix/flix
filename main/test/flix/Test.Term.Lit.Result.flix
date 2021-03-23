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

    instance Eq[Result[t, e]] with Eq[t], Eq[e] {
        pub def eq(r1: Result[t, e], r2: Result[t, e]): Bool = match (r1, r2) {
            case (Ok(v1), Ok(v2)) => v1 == v2
            case (Err(w1), Err(w2)) => w1 == w2
            case _ => false
        }
    }

    instance Hash[Result[t, e]] with Hash[t], Hash[e] {
        pub def hash(r: Result[t, e]): Int = match r {
            case Ok(v)  => 5381 + 113 * Hash.hash(v)
            case Err(v) => 5351 + 97 * Hash.hash(v)
        }
    }

    instance ToString[Result[t, e]] with ToString[t], ToString[e] {
        pub def toString(r: Result[t, e]): String = ???
    }

}

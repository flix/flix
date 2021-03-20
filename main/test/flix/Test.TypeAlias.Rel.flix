namespace Test/TypeAlias/Rel {

    rel A(x: Int)
    rel B(x: Int, y: Int)
    rel C(x: Int, y: Int, z: Int)

    @test
    def testTypeAlias01(): #{A} = #{
        A(1). A(2). A(3).
    }

    @test
    def testTypeAlias02(): #{A, B} = #{
        A(1). A(2). A(3).
        B(1, 2). B(2, 3).
    }

    @test
    def testTypeAlias03(): #{A, B, C} = #{
        A(1). A(2). A(3).
        B(1, 2). B(2, 3).
        C(1, 2, 3).
    }

    rel D(x: List[Int])
    rel E(x: Option[Int])
    rel F(x: Result[Bool, String])

    @test
    def testTypeAlias04(): #{D} = #{
        D(Nil). D(1 :: Nil).
    }

    @test
    def testTypeAlias05(): #{D, E} = #{
        D(Nil). D(1 :: Nil).
        E(None). E(Some(123)).
    }

    @test
    def testTypeAlias06(): #{D, E, F} = #{
        D(Nil). D(1 :: Nil).
        E(None). E(Some(123)).
        F(Ok(true)). F(Err("Hello World!")).
    }

    rel G[a](x: a)
    rel H[a, b](x: a, y: b)
    rel I[a, b, c](x: a, y: b, z: c)

    @test
    def testPolyTypeAlias01(): #{G[Int]} = #{
        G(1). G(2). G(3).
    }

    @test
    def testPolyTypeAlias02(): #{G[String]} = #{
        G("x"). G("x"). G("x").
    }

    @test
    def testPolyTypeAlias03(): #{H[Bool, Int]} = #{
        H(true, 21). H(false, 42).
    }

    @test
    def testPolyTypeAlias04(): #{H[Int, Bool]} = #{
        H(21, true). H(42, false).
    }

    @test
    def testPolyTypeAlias05(): #{I[Bool, Int, String]} = #{
        I(true, 21, "Hello"). I(false, 42, "World!").
    }

    @test
    def testPolyTypeAlias06(): #{I[Int, String, Bool]} = #{
        I(21, "Hello", true). I(42, "World!", false).
    }

    @test
    def testPolyTypeAlias07(): #{G[Option[Int]]} = #{
        G(None). G(Some(21)). G(Some(42)).
    }

    @test
    def testPolyTypeAlias08(): #{G[Int], H[Bool, Int], I[Bool, Int, String]} = #{
        G(1). G(2). G(3).
        H(true, 21). H(false, 42).
        I(true, 21, "Hello"). I(false, 42, "World!").
    }

    @test
    def testIgnoredTypeAlias01(): #{A(String)} = #{
        A("x"). A("x"). A("x").
    }

    @test
    def testIgnoredTypeAlias02(): #{A(String), B(String)} = #{
        A("x"). A("x"). A("x").
        B("x"). B("x"). B("x").
    }

    @test
    def testIgnoredTypeAlias03(): #{A(String), B(String), C(String)} = #{
        A("x"). A("x"). A("x").
        B("x"). B("x"). B("x").
        C("x"). C("x"). C("x").
    }

    @test
    def testMixed01(): #{A, B(String), C(String)} = #{
        A(1). A(2). A(3).
        B("x"). B("x"). B("x").
        C("x"). C("x"). C("x").
    }

    @test
    def testMixed02(): #{A(String), B, C(String)} = #{
        A("x"). A("x"). A("x").
        B(1, 2). B(2, 3).
        C("x"). C("x"). C("x").
    }

    @test
    def testMixed03(): #{A(String), B(String), C} = #{
        A("x"). A("x"). A("x").
        B("x"). B("x"). B("x").
        C(1, 2, 3).
    }

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

    enum Result[t, e] {
        case Ok(t),
        case Err(e)
    }


    instance Eq[Result[t, e]] with [t : Eq, e : Eq] {
        pub def eq(r1: Result[t, e], r2: Result[t, e]): Bool = match (r1, r2) {
            case (Ok(v1), Ok(v2)) => v1 == v2
            case (Err(w1), Err(w2)) => w1 == w2
            case _ => false
        }
    }

    instance Hash[Result[t, e]] with [t : Hash, e : Hash] {
        pub def hash(r: Result[t, e]): Int = match r {
            case Ok(v)  => 5381 + 113 * Hash.hash(v)
            case Err(v) => 5351 + 97 * Hash.hash(v)
        }
    }

    instance ToString[Result[t, e]] with [t : ToString, e : ToString] {
        pub def toString(r: Result[t, e]): String = ???
    }

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

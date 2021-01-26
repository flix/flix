namespace Test/Equality/Tag {

    @test
    def testEqTag01(): Bool = None: Option[Unit] == None
    
    @test
    def testEqTag02(): Bool = Some(42) == Some(42)
    
    @test
    def testEqTag03(): Bool = Ok(42): Result[_, Unit] == Ok(42)
    
    @test
    def testEqTag04(): Bool = Err(42): Result[Unit, _] == Err(42)
    
    @test
    def testEqTag05(): Bool = Nil: List[Unit] == Nil
    
    @test
    def testEqTag06(): Bool = (42 :: Nil) == (42 :: Nil)
    
    @test
    def testEqTag07(): Bool = Some(Ok(42)): Option[Result[_, Unit]] == Some(Ok(42))

    @test
    def testEqTag08(): Bool = Ok(Some(42)): Result[_, Unit] == Ok(Some(42))
    
    @test
    def testEqTag09(): Bool = Some((Ok(1) :: Err(2) :: Nil)) == Some((Ok(1) :: Err(2) :: Nil))
    
    @test
    def testNeqTag01(): Bool = None != Some(42)
    
    @test
    def testNeqTag02(): Bool = Some(21) != Some(42)
    
    @test
    def testNeqTag03(): Bool = Ok(21): Result[_, Unit] != Ok(42)

    @test
    def testNeqTag04(): Bool = Err(21): Result[Unit, _] != Err(42)
    
    @test
    def testNeqTag05(): Bool = Nil != (42 :: Nil)
    
    @test
    def testNeqTag06(): Bool = (21 :: Nil) != (42 :: Nil)
    
    @test
    def testNeqTag07(): Bool = Some(Ok(21)) != Some(Err(42))
    
    @test
    def testNeqTag08(): Bool = Ok(None): Result[Option[Int32], Unit] != Ok(Some(42))
    
    @test
    def testNeqTag09(): Bool = Some((Ok(1) :: Err(2) :: Nil)) != Some((Err(1) :: Ok(2) :: Nil))

    enum Option[t] {
        case None,
        case Some(t)
    }

    enum Result[t, e] {
        case Ok(t),
        case Err(e)
    }

    enum List[t] {
        case Nil,
        case Cons(t, List[t])
    }

    instance Eq[Option[a]] with[a : Eq] {

        def eq(o1: Option[a], o2: Option[a]): Bool = match (o1, o2) {
            case (None, None) => true
            case (Some(v1), Some(v2)) => v1 == v2
            case _ => false
        }

        def neq(xs: Option[a], ys: Option[a]): Bool = not (xs == ys)

    }

    instance Eq[Result[t, e]] with[t : Eq, e : Eq] {

        def eq(r1: Result[t, e], r2: Result[t, e]): Bool = match (r1, r2) {
            case (Ok(v1), Ok(v2)) => v1 == v2
            case (Err(w1), Err(w2)) => w1 == w2
            case _ => false
        }

        def neq(r1: Result[t, e], r2: Result[t, e]): Bool = not (r1 == r2)

    }

    instance Eq[List[a]] with[a : Eq] {

        def eq(xs: List[a], ys: List[a]): Bool = match (xs, ys) {
            case (Nil, Nil) => true
            case (x :: rs, y :: qs) => if (x != y) false else rs == qs
            case _ => false
        }

        def neq(xs: List[a], ys: List[a]): Bool = not (xs == ys)

    }

}

namespace Test/Equality/Tag {

    @test
    def testEqTag01(): Bool = None == None
    
    @test
    def testEqTag02(): Bool = Some(42) == Some(42)
    
    @test
    def testEqTag03(): Bool = Ok(42) == Ok(42)
    
    @test
    def testEqTag04(): Bool = Err(42) == Err(42)
    
    @test
    def testEqTag05(): Bool = Nil == Nil
    
    @test
    def testEqTag06(): Bool = (42 :: Nil) == (42 :: Nil)
    
    @test
    def testEqTag07(): Bool = Some(Ok(42)) == Some(Ok(42))
    
    @test
    def testEqTag08(): Bool = Ok(Some(42)) == Ok(Some(42))
    
    @test
    def testEqTag09(): Bool = Some((Ok(1) :: Err(2) :: Nil)) == Some((Ok(1) :: Err(2) :: Nil))
    
    @test
    def testNeqTag01(): Bool = None != Some(42)
    
    @test
    def testNeqTag02(): Bool = Some(21) != Some(42)
    
    @test
    def testNeqTag03(): Bool = Ok(21) != Ok(42)
    
    @test
    def testNeqTag04(): Bool = Err(21) != Err(42)
    
    @test
    def testNeqTag05(): Bool = Nil != (42 :: Nil)
    
    @test
    def testNeqTag06(): Bool = (21 :: Nil) != (42 :: Nil)
    
    @test
    def testNeqTag07(): Bool = Some(Ok(21)) != Some(Err(42))
    
    @test
    def testNeqTag08(): Bool = Ok(None) != Ok(Some(42))
    
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

}

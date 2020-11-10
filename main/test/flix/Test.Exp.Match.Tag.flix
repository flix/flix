namespace Test/Exp/Match/Tag {

    @test
    def testMatchOption01(): Bool = match None {
        case None    => true
        case Some(_) => false
    }

    @test
    def testMatchOption02(): Bool = match None {
        case Some(_) => false
        case None    => true
    }

    @test
    def testMatchOption03(): Bool = match Some(123) {
        case None    => false
        case Some(_) => true
    }

    @test
    def testMatchOption04(): Bool = match Some(123) {
        case Some(_) => true
        case None    => false
    }

    @test
    def testMatchResult01(): Bool = match Ok(123) {
        case Ok(_)  => true
        case Err(_) => false
    }

    @test
    def testMatchResult02(): Bool = match Ok(123) {
        case Err(_) => false
        case Ok(_)  => true
    }

    @test
    def testMatchResult03(): Bool = match Err(123) {
        case Ok(_)  => false
        case Err(_) => true
    }

    @test
    def testMatchResult04(): Bool = match Err(123) {
        case Err(_) => true
        case Ok(_)  => false
    }

    @test
    def testMatchNested01(): Bool = match None {
        case None           => true
        case Some(None)     => false
        case Some(Some(_))  => false
    }

    @test
    def testMatchNested02(): Bool = match Some(None) {
        case None           => false
        case Some(None)     => true
        case Some(Some(_))  => false
    }

    @test
    def testMatchNested03(): Bool = match Some(Some(123)) {
        case None           => false
        case Some(None)     => false
        case Some(Some(_))  => true
    }

    enum Option[t] {
        case None,
        case Some(t)
    }

    enum Result[t, e] {
        case Ok(t),
        case Err(e)
    }

}

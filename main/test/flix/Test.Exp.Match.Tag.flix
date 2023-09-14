mod Test.Exp.Match.Tag {

    @Test
    def testMatchOption01(): Bool = match None {
        case None    => true
        case Some(_) => false
    }

    @Test
    def testMatchOption02(): Bool = match None {
        case Some(_) => false
        case None    => true
    }

    @Test
    def testMatchOption03(): Bool = match Some(123) {
        case None    => false
        case Some(_) => true
    }

    @Test
    def testMatchOption04(): Bool = match Some(123) {
        case Some(_) => true
        case None    => false
    }

    @Test
    def testMatchResult01(): Bool = match Ok(123) {
        case Ok(_)  => true
        case Err(_) => false
    }

    @Test
    def testMatchResult02(): Bool = match Ok(123) {
        case Err(_) => false
        case Ok(_)  => true
    }

    @Test
    def testMatchResult03(): Bool = match Err(123) {
        case Ok(_)  => false
        case Err(_) => true
    }

    @Test
    def testMatchResult04(): Bool = match Err(123) {
        case Err(_) => true
        case Ok(_)  => false
    }

    @Test
    def testMatchNested01(): Bool = match None {
        case None           => true
        case Some(None)     => false
        case Some(Some(_))  => false
    }

    @Test
    def testMatchNested02(): Bool = match Some(None) {
        case None           => false
        case Some(None)     => true
        case Some(Some(_))  => false
    }

    @Test
    def testMatchNested03(): Bool = match Some(Some(123)) {
        case None           => false
        case Some(None)     => false
        case Some(Some(_))  => true
    }

    @Test
    def testMatchNested04(): Bool = match None {
        case None         => true
        case Some(Ok(_))  => false
        case Some(Err(_)) => false
    }

    @Test
    def testMatchNested05(): Bool = match Some(Ok(123)) {
        case None         => false
        case Some(Ok(_))  => true
        case Some(Err(_)) => false
    }

    @Test
    def testMatchNested06(): Bool = match Some(Err(123)) {
        case None         => false
        case Some(Ok(_))  => false
        case Some(Err(_)) => true
    }

    @Test
    def testMatchTopLevel01(): Bool = match (Option.Some(1)) {
        case Option.None    => false
        case Option.Some(_) => true
    }
}

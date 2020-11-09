namespace Test/Exp/Match/Tag {

    @test
    def testMatchOption01(): Bool = match None {
        case None    => true
        case Some(_) => false
    }

    @test
    def testMatchOption02(): Bool = match Some(123) {
        case None    => false
        case Some(_) => true
    }

    // TODO: Add more
    // TODO: Add vars to tests

    enum Option[t] {
        case None,
        case Some(t)
    }

}

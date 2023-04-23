mod Test.Exp.HoleWithExp {

    @test
    def testHoleWithExp01(): Int32 = match (None : Option[Int32]) {
        case None    => 1
        case Some(n) => n?
    }

    @test
    def testHoleWithExp02(): Int32 = match (None : Option[Int32]) {
        case None    => 1
        case Some(n) => 5 + n?
    }

    @test
    def testHoleWithExp03(): Int32 = match (None : Option[Int32]) {
        case None    => 42
        case Some(n) => List.length(n?)
    }
}

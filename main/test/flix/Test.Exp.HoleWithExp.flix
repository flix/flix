mod Test.Exp.HoleWithExp {

    @Test
    def testHoleWithExp01(): Int32 = match (None : Option[Int32]) {
        case None    => 1
        case Some(n) => n?
    }

    @Test
    def testHoleWithExp02(): Int32 = match (None : Option[Int32]) {
        case None    => 1
        case Some(n) => 5 + n?
    }

    @Test
    def testHoleWithExp03(): Int32 = match (None : Option[Int32]) {
        case None    => 42
        case Some(n) => List.length(n?)
    }
}

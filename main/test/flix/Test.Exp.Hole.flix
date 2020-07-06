namespace Test/Exp/Hole {

    @test
    def testHole01(): Int = if (true) 42 else ?hole

    @test
    def testHole02(): Int = match Some(42) {
        case None    => ?hole
        case Some(n) => n
    }

    @test
    def testHole03(): Int = match (None : Option[Int]) {
        case None    => 1
        case Some(n) => n + ?hole
    }

    @test
    def testHole04(): Int = match (None : Option[Int]) {
        case None    => 1
        case Some(n) => ?hole(n)
    }

    @test
    def testHole05(): Int = match (None : Option[Int]) {
        case None    => 1
        case Some(n) => ?hole(n, n, n)
    }

    @test
    def testHole06(): Int = match (None : Option[Int]) {
        case None    => 42
        case Some(n) => List.length(?hole)
    }

    @test
    def testHole07(): Int = match (None : Option[Int]) {
        case None    => 42
        case Some(n) => ?hole1(List.unzip(?hole2))
    }

    @test
    def testHole08(): Int = match (None : Option[Int]) {
        case None    => 42
        case Some(n) => ?hole1(List.map(?hole2, ?hole3))
    }

    @test
    def testHole09(): Int = match (None : Option[Int]) {
        case None    => 42
        case Some(n) => ?hole1(List.map(?hole2, List.range(?hole3, ?hole4)))
    }

}
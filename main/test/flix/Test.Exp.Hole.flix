mod Test.Exp.Hole {

    @CompileTest
    def testHole01(): Int32 = if (true) 42 else ?hole

    @CompileTest
    def testHole02(): Int32 = match Some(42) {
        case None    => ?hole
        case Some(n) => n
    }

    @CompileTest
    def testHole03(): Int32 = match (None : Option[Int32]) {
        case None    => 1
        case Some(n) => n + ?hole
    }

    @CompileTest
    def testHole04(): Int32 = match (None : Option[Int32]) {
        case None    => 1
        case Some(n) => ?hole(n)
    }

    @CompileTest
    def testHole05(): Int32 = match (None : Option[Int32]) {
        case None    => 1
        case Some(n) => ?hole(n, n, n)
    }

    @CompileTest
    def testHole06(): Int32 = match (None : Option[Int32]) {
        case None    => 42
        case Some(n) => List.length(?hole)
    }

    @CompileTest
    def testHole07(): Int32 = match (None : Option[Int32]) {
        case None    => 42
        case Some(n) => ?hole1(List.unzip(?hole2))
    }

    @CompileTest
    def testHole08(): Int32 = match (None : Option[Int32]) {
        case None    => 42
        case Some(n) => ?hole1(List.map(?hole2, ?hole3))
    }

    @CompileTest
    def testHole09(): Int32 = match (None : Option[Int32]) {
        case None    => 42
        case Some(n) => ?hole1(List.map(?hole2, List.range(?hole3, ?hole4)))
    }

}

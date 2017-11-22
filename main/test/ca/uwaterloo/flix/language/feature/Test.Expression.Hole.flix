// TODO: Must be re-enabled in TestFeature

@test
def testHole01(): Int = if (true) 42 else ?hole

@test
def testHole02(): Int = match Some(42) with {
    case None    => ?hole
    case Some(n) => n
}

// TODO
//@test
//def testHole03(): Int = match (None : Option[Int]) with {
//    case None    => 42
//    case Some(n) => 42 + ?hole
//}

// TODO
//@test
//def testHole04(): Int = match (None : Option[Int]) with {
//    case None    => 42
//    case Some(n) => ?hole(n)
//}

// TODO
//@test
//def testHole05(): Int = match (None : Option[Int]) with {
//    case None    => 42
//    case Some(n) => ?hole(n, n, n)
//}

// TODO
//@test
//def testHole06(): Int = match (None : Option[Int]) with {
//    case None    => 42
//    case Some(n) => List.length(?hole)
//}

// TODO
//@test
//def testHole07(): Int = match (None : Option[Int]) with {
//    case None    => 42
//    case Some(n) => ?hole1(List.unzip(?hole2))
//}

// TODO
//@test
//def testHole08(): Int = match (None : Option[Int]) with {
//    case None    => 42
//    case Some(n) => ?hole1(List.map(?hole2, ?hole3))
//}

// TODO
//@test
//def testHole09(): Int = match (None : Option[Int]) with {
//    case None    => 42
//    case Some(n) => ?hole1(List.map(?hole2, List.range(?hole3, ?hole4)))
//}

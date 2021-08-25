namespace Test/Derivation {
    pub enum E with ToString {
      case C1
      case C2(Int)
    }

//    pub enum E1[a] with Eq {
//        case C1
//        case C2(a)
//    }
//
//    pub enum E2 with Eq, ToString
//
//    pub enum E3 with Eq, Order, ToString

    @test
    def testToString01(): Bool = ToString.toString(C1) == "C1(())"

    @test
    def testToString02(): Bool = ToString.toString(C2(123)) == "C2(123)"

    @test
    def empty(): Bool = true
}
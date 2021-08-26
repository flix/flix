namespace Test/Derivation {
    pub enum EnumWithToString with ToString {
      case EmptyCase
      case SingletonCase(Int)
      case TupleCase(Int, Bool)
    }

    pub enum PolyEnumWithToString[a] with ToString {
        case EmptyCase
        case SingletonCase(a)
        case TupleCase(a, a)
    }

    pub enum RecursiveEnumWithToString with ToString {
        case EmptyCase
        case RecursiveCase(RecursiveEnumWithToString)
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
    def testToString01(): Bool = ToString.toString(EnumWithToString.EmptyCase) == "EmptyCase"

    @test
    def testToString02(): Bool = ToString.toString(EnumWithToString.SingletonCase(123)) == "SingletonCase(123)"

    @test
    def testToString03(): Bool & Impure = println(EnumWithToString.TupleCase(123, true)); ToString.toString(EnumWithToString.TupleCase(123, true)) == "TupleCase(123, true)"

    @test
    def testToString04(): Bool = ToString.toString(PolyEnumWithToString.EmptyCase: PolyEnumWithToString[Int]) == "EmptyCase"

    @test
    def testToString05(): Bool = ToString.toString(PolyEnumWithToString.SingletonCase(123)) == "SingletonCase(123)"

    @test
    def testToString06(): Bool = ToString.toString(PolyEnumWithToString.TupleCase(123, 123)) == "TupleCase(123, 123)"

    @test
    def testToString07(): Bool = ToString.toString(RecursiveEnumWithToString.EmptyCase) == "EmptyCase"

    @test
    def testToString08(): Bool = ToString.toString(RecursiveEnumWithToString.RecursiveCase(RecursiveEnumWithToString.EmptyCase)) == "RecursiveCase(EmptyCase)"
}
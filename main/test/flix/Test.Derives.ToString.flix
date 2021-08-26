namespace Test/Derives/ToString {
    pub enum Enum with ToString {
      case EmptyCase
      case SingletonCase(Int)
      case TupleCase(Int, Bool)
    }

    pub enum PolyEnum[a] with ToString {
        case EmptyCase
        case SingletonCase(a)
        case TupleCase(a, a)
    }

    pub enum RecursiveEnum with ToString {
        case EmptyCase
        case RecursiveCase(RecursiveEnum)
    }

    @test
    def testToString01(): Bool = ToString.toString(Enum.EmptyCase) == "EmptyCase"

    @test
    def testToString02(): Bool = ToString.toString(Enum.SingletonCase(123)) == "SingletonCase(123)"

    @test
    def testToString03(): Bool = ToString.toString(Enum.TupleCase(123, true)) == "TupleCase(123, true)"

    @test
    def testToString04(): Bool = ToString.toString(PolyEnum.EmptyCase: PolyEnum[Int]) == "EmptyCase"

    @test
    def testToString05(): Bool = ToString.toString(PolyEnum.SingletonCase(123)) == "SingletonCase(123)"

    @test
    def testToString06(): Bool = ToString.toString(PolyEnum.TupleCase(123, 123)) == "TupleCase(123, 123)"

    @test
    def testToString07(): Bool = ToString.toString(RecursiveEnum.EmptyCase) == "EmptyCase"

    @test
    def testToString08(): Bool = ToString.toString(RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase)) == "RecursiveCase(EmptyCase)"
}

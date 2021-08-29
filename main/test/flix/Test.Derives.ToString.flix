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
    def testToString01(): Bool = "${Enum.EmptyCase}" == "EmptyCase"

    @test
    def testToString02(): Bool = "${Enum.SingletonCase(123)}" == "SingletonCase(123)"

    @test
    def testToString03(): Bool = "${Enum.TupleCase(123, true)}" == "TupleCase(123, true)"

    @test
    def testToString04(): Bool = "${PolyEnum.EmptyCase: PolyEnum[Int]}" == "EmptyCase"

    @test
    def testToString05(): Bool = "${PolyEnum.SingletonCase(123)}" == "SingletonCase(123)"

    @test
    def testToString06(): Bool = "${PolyEnum.TupleCase(123, 123)}" == "TupleCase(123, 123)"

    @test
    def testToString07(): Bool = "${RecursiveEnum.EmptyCase}" == "EmptyCase"

    @test
    def testToString08(): Bool = "${RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase)}" == "RecursiveCase(EmptyCase)"
}

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

    pub enum PolyMultiEnum[a, b] with ToString {
        case EmptyCase
        case LeftCase(a)
        case RightCase(b)
        case BothCase(a, b)
    }

    pub enum PolyBoolEnum[ef: Bool] with ToString {
        case EmptyCase
        case RecursiveCase(PolyBoolEnum[ef])
    }

    pub enum MutRecursiveEnum1 with ToString {
        case EmptyCase
        case RecursiveCase(MutRecursiveEnum2)
    }

    pub enum MutRecursiveEnum2 with ToString {
        case EmptyCase
        case RecursiveCase(MutRecursiveEnum1)
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

    @test
    def testToString09(): Bool = "${PolyMultiEnum.EmptyCase: PolyMultiEnum[Int, Bool]}" == "EmptyCase"

    @test
    def testToString10(): Bool = "${PolyMultiEnum.LeftCase(123): PolyMultiEnum[Int, Bool]}" == "LeftCase(123)"

    @test
    def testToString11(): Bool = "${PolyMultiEnum.RightCase(true): PolyMultiEnum[Int, Bool]}" == "RightCase(true)"

    @test
    def testToString12(): Bool = "${PolyMultiEnum.BothCase(123, true)}" == "BothCase(123, true)"

    @test
    def testToString13(): Bool = "${PolyBoolEnum.EmptyCase: PolyBoolEnum[Pure]}" == "EmptyCase"

    @test
    def testToString14(): Bool = "${PolyBoolEnum.RecursiveCase(PolyBoolEnum.EmptyCase): PolyBoolEnum[Pure]}" == "RecursiveCase(EmptyCase)"

    @test
    def testToString15(): Bool = "${MutRecursiveEnum1.EmptyCase}" == "EmptyCase"

    @test
    def testToString16(): Bool = "${MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.EmptyCase)}" == "RecursiveCase(EmptyCase)"

    @test
    def testToString17(): Bool = "${MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.RecursiveCase(MutRecursiveEnum1.EmptyCase))}" == "RecursiveCase(RecursiveCase(EmptyCase))"
}

mod Test.Derives.ToString {
    pub enum Enum with ToString {
      case EmptyCase
      case SingletonCase(Int32)
      case TupleCase(Int32, Bool)
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

    pub enum PolyBoolEnum[ef: Eff] with ToString {
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

    pub enum OpaqueType(Int32) with ToString

    @Test
    def testToString01(): Bool = "${Enum.EmptyCase}" == "EmptyCase"

    @Test
    def testToString02(): Bool = "${Enum.SingletonCase(123)}" == "SingletonCase(123)"

    @Test
    def testToString03(): Bool = "${Enum.TupleCase(123, true)}" == "TupleCase(123, true)"

    @Test
    def testToString04(): Bool = "${PolyEnum.EmptyCase: PolyEnum[Int32]}" == "EmptyCase"

    @Test
    def testToString05(): Bool = "${PolyEnum.SingletonCase(123)}" == "SingletonCase(123)"

    @Test
    def testToString06(): Bool = "${PolyEnum.TupleCase(123, 123)}" == "TupleCase(123, 123)"

    @Test
    def testToString07(): Bool = "${PolyEnum.EmptyCase: PolyEnum[Enum]}" == "EmptyCase"

    @Test
    def testToString08(): Bool = "${PolyEnum.SingletonCase(Enum.EmptyCase)}" == "SingletonCase(EmptyCase)"

    @Test
    def testToString09(): Bool = "${PolyEnum.SingletonCase(Enum.SingletonCase(123))}" == "SingletonCase(SingletonCase(123))"

    @Test
    def testToString10(): Bool = "${PolyEnum.EmptyCase: PolyEnum[PolyEnum[Int32]]}" == "EmptyCase"

    @Test
    def testToString11(): Bool = "${PolyEnum.SingletonCase(PolyEnum.EmptyCase): PolyEnum[PolyEnum[Int32]]}" == "SingletonCase(EmptyCase)"

    @Test
    def testToString12(): Bool = "${PolyEnum.SingletonCase(PolyEnum.SingletonCase(123))}" == "SingletonCase(SingletonCase(123))"

    @Test
    def testToString13(): Bool = "${RecursiveEnum.EmptyCase}" == "EmptyCase"

    @Test
    def testToString14(): Bool = "${RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase)}" == "RecursiveCase(EmptyCase)"

    @Test
    def testToString15(): Bool = "${PolyMultiEnum.EmptyCase: PolyMultiEnum[Int32, Bool]}" == "EmptyCase"

    @Test
    def testToString16(): Bool = "${PolyMultiEnum.LeftCase(123): PolyMultiEnum[Int32, Bool]}" == "LeftCase(123)"

    @Test
    def testToString17(): Bool = "${PolyMultiEnum.RightCase(true): PolyMultiEnum[Int32, Bool]}" == "RightCase(true)"

    @Test
    def testToString18(): Bool = "${PolyMultiEnum.BothCase(123, true)}" == "BothCase(123, true)"

    @Test
    def testToString19(): Bool = "${PolyBoolEnum.EmptyCase: PolyBoolEnum[Pure]}" == "EmptyCase"

    @Test
    def testToString20(): Bool = "${PolyBoolEnum.RecursiveCase(PolyBoolEnum.EmptyCase): PolyBoolEnum[Pure]}" == "RecursiveCase(EmptyCase)"

    @Test
    def testToString21(): Bool = "${MutRecursiveEnum1.EmptyCase}" == "EmptyCase"

    @Test
    def testToString22(): Bool = "${MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.EmptyCase)}" == "RecursiveCase(EmptyCase)"

    @Test
    def testToString23(): Bool = "${MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.RecursiveCase(MutRecursiveEnum1.EmptyCase))}" == "RecursiveCase(RecursiveCase(EmptyCase))"

    @Test
    def testToString24(): Bool = "${OpaqueType.OpaqueType(123)}" == "OpaqueType(123)"
}

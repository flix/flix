mod Test.Derives.Eq {

    pub enum Enum with Eq {
      case EmptyCase
      case SingletonCase(Int32)
      case TupleCase(Int32, Bool)
    }

    pub enum PolyEnum[a] with Eq {
        case EmptyCase
        case SingletonCase(a)
        case TupleCase(a, a)
    }

    pub enum RecursiveEnum with Eq {
        case EmptyCase
        case RecursiveCase(RecursiveEnum)
    }

    pub enum PolyMultiEnum[a, b] with Eq {
        case EmptyCase
        case LeftCase(a)
        case RightCase(b)
        case BothCase(a, b)
    }

    pub enum PolyBoolEnum[ef: Eff] with Eq {
        case EmptyCase
        case RecursiveCase(PolyBoolEnum[ef])
    }

    pub enum MutRecursiveEnum1 with Eq {
        case EmptyCase
        case RecursiveCase(MutRecursiveEnum2)
    }

    pub enum MutRecursiveEnum2 with Eq {
        case EmptyCase
        case RecursiveCase(MutRecursiveEnum1)
    }

    pub enum OpaqueType(Int32) with Eq

    @Test
    def testEq01(): Bool = Enum.EmptyCase == Enum.EmptyCase

    @Test
    def testEq02(): Bool = Enum.SingletonCase(123) == Enum.SingletonCase(123)

    @Test
    def testEq03(): Bool = Enum.TupleCase(123, true) == Enum.TupleCase(123, true)

    @Test
    def testEq04(): Bool = PolyEnum.EmptyCase: PolyEnum[Int32] == PolyEnum.EmptyCase: PolyEnum[Int32]

    @Test
    def testEq05(): Bool = PolyEnum.SingletonCase(123) == PolyEnum.SingletonCase(123)

    @Test
    def testEq06(): Bool = PolyEnum.TupleCase(123, 123) == PolyEnum.TupleCase(123, 123)

    @Test
    def testEq07(): Bool = PolyEnum.EmptyCase: PolyEnum[Enum] == PolyEnum.EmptyCase: PolyEnum[Enum]

    @Test
    def testEq08(): Bool = PolyEnum.SingletonCase(Enum.EmptyCase) == PolyEnum.SingletonCase(Enum.EmptyCase)

    @Test
    def testEq09(): Bool = PolyEnum.SingletonCase(Enum.SingletonCase(123)) == PolyEnum.SingletonCase(Enum.SingletonCase(123))

    @Test
    def testEq10(): Bool = PolyEnum.EmptyCase: PolyEnum[PolyEnum[Int32]] == PolyEnum.EmptyCase: PolyEnum[PolyEnum[Int32]]

    @Test
    def testEq11(): Bool = PolyEnum.SingletonCase(PolyEnum.EmptyCase): PolyEnum[PolyEnum[Int32]] == PolyEnum.SingletonCase(PolyEnum.EmptyCase): PolyEnum[PolyEnum[Int32]]

    @Test
    def testEq12(): Bool = PolyEnum.SingletonCase(PolyEnum.SingletonCase(123)) == PolyEnum.SingletonCase(PolyEnum.SingletonCase(123))

    @Test
    def testEq13(): Bool = RecursiveEnum.EmptyCase == RecursiveEnum.EmptyCase

    @Test
    def testEq14(): Bool = RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase) == RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase)

    @Test
    def testEq15(): Bool = PolyMultiEnum.EmptyCase: PolyMultiEnum[Int32, Bool] == PolyMultiEnum.EmptyCase: PolyMultiEnum[Int32, Bool]

    @Test
    def testEq16(): Bool = PolyMultiEnum.LeftCase(123): PolyMultiEnum[Int32, Bool] == PolyMultiEnum.LeftCase(123): PolyMultiEnum[Int32, Bool]

    @Test
    def testEq17(): Bool = PolyMultiEnum.RightCase(true): PolyMultiEnum[Int32, Bool] == PolyMultiEnum.RightCase(true): PolyMultiEnum[Int32, Bool]

    @Test
    def testEq18(): Bool = PolyMultiEnum.BothCase(123, true) == PolyMultiEnum.BothCase(123, true)

    @Test
    def testEq19(): Bool = PolyBoolEnum.EmptyCase: PolyBoolEnum[Pure] == PolyBoolEnum.EmptyCase: PolyBoolEnum[Pure]

    @Test
    def testEq20(): Bool = PolyBoolEnum.RecursiveCase(PolyBoolEnum.EmptyCase): PolyBoolEnum[Pure] == PolyBoolEnum.RecursiveCase(PolyBoolEnum.EmptyCase): PolyBoolEnum[Pure]

    @Test
    def testEq21(): Bool = MutRecursiveEnum1.EmptyCase == MutRecursiveEnum1.EmptyCase

    @Test
    def testEq22(): Bool = MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.EmptyCase) == MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.EmptyCase)

    @Test
    def testEq23(): Bool = MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.RecursiveCase(MutRecursiveEnum1.EmptyCase)) == MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.RecursiveCase(MutRecursiveEnum1.EmptyCase))

    @Test
    def testEq24(): Bool = OpaqueType.OpaqueType(123) == OpaqueType.OpaqueType(123)

    @Test
    def testNeq01(): Bool = Enum.EmptyCase != Enum.SingletonCase(123)

    @Test
    def testNeq02(): Bool = Enum.SingletonCase(123) != Enum.SingletonCase(456)

    @Test
    def testNeq03(): Bool = PolyEnum.EmptyCase != PolyEnum.SingletonCase(123)

    @Test
    def testNeq04(): Bool = PolyEnum.SingletonCase(123) != PolyEnum.SingletonCase(456)

    @Test
    def testNeq05(): Bool = PolyEnum.SingletonCase(Enum.EmptyCase) != PolyEnum.SingletonCase(Enum.SingletonCase(123))

    @Test
    def testNeq06(): Bool = RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase) != RecursiveEnum.RecursiveCase(RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase))

    @Test
    def testNeq07(): Bool = PolyMultiEnum.LeftCase(123) != PolyMultiEnum.RightCase(123)

    @Test
    def testNeq08(): Bool = PolyBoolEnum.EmptyCase: PolyBoolEnum[Pure] != PolyBoolEnum.RecursiveCase(PolyBoolEnum.EmptyCase): PolyBoolEnum[Pure]

    @Test
    def testNeq09(): Bool = MutRecursiveEnum1.EmptyCase != MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.EmptyCase)

    @Test
    def testNeq10(): Bool = OpaqueType.OpaqueType(123) != OpaqueType.OpaqueType(456)
}

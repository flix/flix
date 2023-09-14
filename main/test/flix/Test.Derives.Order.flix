mod Test.Derives.Order {
    use Order.compare
    use Comparison.{EqualTo, LessThan, GreaterThan}

    pub enum Enum with Eq, Order {
      case EmptyCase
      case SingletonCase(Int32)
      case TupleCase(Int32, Bool)
    }

    pub enum PolyEnum[a] with Eq, Order {
        case EmptyCase
        case SingletonCase(a)
        case TupleCase(a, a)
    }

    pub enum RecursiveEnum with Eq, Order {
        case EmptyCase
        case RecursiveCase(RecursiveEnum)
    }

    pub enum PolyMultiEnum[a, b] with Eq, Order {
        case EmptyCase
        case LeftCase(a)
        case RightCase(b)
        case BothCase(a, b)
    }

    pub enum PolyBoolEnum[ef: Eff] with Eq, Order {
        case EmptyCase
        case RecursiveCase(PolyBoolEnum[ef])
    }

    pub enum MutRecursiveEnum1 with Eq, Order {
        case EmptyCase
        case RecursiveCase(MutRecursiveEnum2)
    }

    pub enum MutRecursiveEnum2 with Eq, Order {
        case EmptyCase
        case RecursiveCase(MutRecursiveEnum1)
    }

    pub enum OpaqueType(Int32) with Eq, Order

    @Test
    def testEq01(): Bool = compare(Enum.EmptyCase, Enum.EmptyCase) == EqualTo

    @Test
    def testEq02(): Bool = compare(Enum.SingletonCase(123), Enum.SingletonCase(123)) == EqualTo

    @Test
    def testEq03(): Bool = compare(Enum.TupleCase(123, true), Enum.TupleCase(123, true)) == EqualTo

    @Test
    def testEq04(): Bool = compare(PolyEnum.EmptyCase: PolyEnum[Int32], PolyEnum.EmptyCase: PolyEnum[Int32]) == EqualTo

    @Test
    def testEq05(): Bool = compare(PolyEnum.SingletonCase(123), PolyEnum.SingletonCase(123)) == EqualTo

    @Test
    def testEq06(): Bool = compare(PolyEnum.TupleCase(123, 123), PolyEnum.TupleCase(123, 123)) == EqualTo

    @Test
    def testEq07(): Bool = compare(PolyEnum.EmptyCase: PolyEnum[Enum], PolyEnum.EmptyCase: PolyEnum[Enum]) == EqualTo

    @Test
    def testEq08(): Bool = compare(PolyEnum.SingletonCase(Enum.EmptyCase), PolyEnum.SingletonCase(Enum.EmptyCase)) == EqualTo

    @Test
    def testEq09(): Bool = compare(PolyEnum.SingletonCase(Enum.SingletonCase(123)), PolyEnum.SingletonCase(Enum.SingletonCase(123))) == EqualTo

    @Test
    def testEq10(): Bool = compare(PolyEnum.EmptyCase: PolyEnum[PolyEnum[Int32]], PolyEnum.EmptyCase: PolyEnum[PolyEnum[Int32]]) == EqualTo

    @Test
    def testEq11(): Bool = compare(PolyEnum.SingletonCase(PolyEnum.EmptyCase): PolyEnum[PolyEnum[Int32]], PolyEnum.SingletonCase(PolyEnum.EmptyCase): PolyEnum[PolyEnum[Int32]]) == EqualTo

    @Test
    def testEq12(): Bool = compare(PolyEnum.SingletonCase(PolyEnum.SingletonCase(123)), PolyEnum.SingletonCase(PolyEnum.SingletonCase(123))) == EqualTo

    @Test
    def testEq13(): Bool = compare(RecursiveEnum.EmptyCase, RecursiveEnum.EmptyCase) == EqualTo

    @Test
    def testEq14(): Bool = compare(RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase), RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase)) == EqualTo

    @Test
    def testEq15(): Bool = compare(PolyMultiEnum.EmptyCase: PolyMultiEnum[Int32, Bool], PolyMultiEnum.EmptyCase: PolyMultiEnum[Int32, Bool]) == EqualTo

    @Test
    def testEq16(): Bool = compare(PolyMultiEnum.LeftCase(123): PolyMultiEnum[Int32, Bool], PolyMultiEnum.LeftCase(123): PolyMultiEnum[Int32, Bool]) == EqualTo

    @Test
    def testEq17(): Bool = compare(PolyMultiEnum.RightCase(true): PolyMultiEnum[Int32, Bool], PolyMultiEnum.RightCase(true): PolyMultiEnum[Int32, Bool]) == EqualTo

    @Test
    def testEq18(): Bool = compare(PolyMultiEnum.BothCase(123, true), PolyMultiEnum.BothCase(123, true)) == EqualTo

    @Test
    def testEq19(): Bool = compare(PolyBoolEnum.EmptyCase: PolyBoolEnum[Pure], PolyBoolEnum.EmptyCase: PolyBoolEnum[Pure]) == EqualTo

    @Test
    def testEq20(): Bool = compare(PolyBoolEnum.RecursiveCase(PolyBoolEnum.EmptyCase): PolyBoolEnum[Pure], PolyBoolEnum.RecursiveCase(PolyBoolEnum.EmptyCase): PolyBoolEnum[Pure]) == EqualTo

    @Test
    def testEq21(): Bool = compare(MutRecursiveEnum1.EmptyCase, MutRecursiveEnum1.EmptyCase) == EqualTo

    @Test
    def testEq22(): Bool = compare(MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.EmptyCase), MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.EmptyCase)) == EqualTo

    @Test
    def testEq23(): Bool = compare(MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.RecursiveCase(MutRecursiveEnum1.EmptyCase)), MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.RecursiveCase(MutRecursiveEnum1.EmptyCase))) == EqualTo

    @Test
    def testEq24(): Bool = compare(OpaqueType.OpaqueType(123), OpaqueType.OpaqueType(123)) == EqualTo

    @Test
    def testLt01(): Bool = Enum.EmptyCase < Enum.SingletonCase(123)

    @Test
    def testLt02(): Bool = Enum.SingletonCase(123) < Enum.SingletonCase(456)

    @Test
    def testLt03(): Bool = PolyEnum.EmptyCase < PolyEnum.SingletonCase(123)

    @Test
    def testLt04(): Bool = PolyEnum.SingletonCase(123) < PolyEnum.SingletonCase(456)

    @Test
    def testLt05(): Bool = PolyEnum.SingletonCase(Enum.EmptyCase) < PolyEnum.SingletonCase(Enum.SingletonCase(123))

    @Test
    def testLt06(): Bool = RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase) < RecursiveEnum.RecursiveCase(RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase))

    @Test
    def testLt07(): Bool = PolyMultiEnum.LeftCase(123) < PolyMultiEnum.RightCase(123)

    @Test
    def testLt08(): Bool = PolyBoolEnum.EmptyCase: PolyBoolEnum[Pure] < PolyBoolEnum.RecursiveCase(PolyBoolEnum.EmptyCase): PolyBoolEnum[Pure]

    @Test
    def testLt09(): Bool = MutRecursiveEnum1.EmptyCase < MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.EmptyCase)

    @Test
    def testLt10(): Bool = OpaqueType.OpaqueType(123) < OpaqueType.OpaqueType(456)
}

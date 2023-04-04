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

    pub enum PolyBoolEnum[ef: Bool] with Eq, Order {
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

    @test
    def testEq01(): Bool = compare(Enum.EmptyCase, Enum.EmptyCase) == EqualTo

    @test
    def testEq02(): Bool = compare(Enum.SingletonCase(123), Enum.SingletonCase(123)) == EqualTo

    @test
    def testEq03(): Bool = compare(Enum.TupleCase(123, true), Enum.TupleCase(123, true)) == EqualTo

    @test
    def testEq04(): Bool = compare(PolyEnum.EmptyCase: PolyEnum[Int32], PolyEnum.EmptyCase: PolyEnum[Int32]) == EqualTo

    @test
    def testEq05(): Bool = compare(PolyEnum.SingletonCase(123), PolyEnum.SingletonCase(123)) == EqualTo

    @test
    def testEq06(): Bool = compare(PolyEnum.TupleCase(123, 123), PolyEnum.TupleCase(123, 123)) == EqualTo

    @test
    def testEq07(): Bool = compare(PolyEnum.EmptyCase: PolyEnum[Enum], PolyEnum.EmptyCase: PolyEnum[Enum]) == EqualTo

    @test
    def testEq08(): Bool = compare(PolyEnum.SingletonCase(Enum.EmptyCase), PolyEnum.SingletonCase(Enum.EmptyCase)) == EqualTo

    @test
    def testEq09(): Bool = compare(PolyEnum.SingletonCase(Enum.SingletonCase(123)), PolyEnum.SingletonCase(Enum.SingletonCase(123))) == EqualTo

    @test
    def testEq10(): Bool = compare(PolyEnum.EmptyCase: PolyEnum[PolyEnum[Int32]], PolyEnum.EmptyCase: PolyEnum[PolyEnum[Int32]]) == EqualTo

    @test
    def testEq11(): Bool = compare(PolyEnum.SingletonCase(PolyEnum.EmptyCase): PolyEnum[PolyEnum[Int32]], PolyEnum.SingletonCase(PolyEnum.EmptyCase): PolyEnum[PolyEnum[Int32]]) == EqualTo

    @test
    def testEq12(): Bool = compare(PolyEnum.SingletonCase(PolyEnum.SingletonCase(123)), PolyEnum.SingletonCase(PolyEnum.SingletonCase(123))) == EqualTo

    @test
    def testEq13(): Bool = compare(RecursiveEnum.EmptyCase, RecursiveEnum.EmptyCase) == EqualTo

    @test
    def testEq14(): Bool = compare(RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase), RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase)) == EqualTo

    @test
    def testEq15(): Bool = compare(PolyMultiEnum.EmptyCase: PolyMultiEnum[Int32, Bool], PolyMultiEnum.EmptyCase: PolyMultiEnum[Int32, Bool]) == EqualTo

    @test
    def testEq16(): Bool = compare(PolyMultiEnum.LeftCase(123): PolyMultiEnum[Int32, Bool], PolyMultiEnum.LeftCase(123): PolyMultiEnum[Int32, Bool]) == EqualTo

    @test
    def testEq17(): Bool = compare(PolyMultiEnum.RightCase(true): PolyMultiEnum[Int32, Bool], PolyMultiEnum.RightCase(true): PolyMultiEnum[Int32, Bool]) == EqualTo

    @test
    def testEq18(): Bool = compare(PolyMultiEnum.BothCase(123, true), PolyMultiEnum.BothCase(123, true)) == EqualTo

    @test
    def testEq19(): Bool = compare(PolyBoolEnum.EmptyCase: PolyBoolEnum[Pure], PolyBoolEnum.EmptyCase: PolyBoolEnum[Pure]) == EqualTo

    @test
    def testEq20(): Bool = compare(PolyBoolEnum.RecursiveCase(PolyBoolEnum.EmptyCase): PolyBoolEnum[Pure], PolyBoolEnum.RecursiveCase(PolyBoolEnum.EmptyCase): PolyBoolEnum[Pure]) == EqualTo

    @test
    def testEq21(): Bool = compare(MutRecursiveEnum1.EmptyCase, MutRecursiveEnum1.EmptyCase) == EqualTo

    @test
    def testEq22(): Bool = compare(MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.EmptyCase), MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.EmptyCase)) == EqualTo

    @test
    def testEq23(): Bool = compare(MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.RecursiveCase(MutRecursiveEnum1.EmptyCase)), MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.RecursiveCase(MutRecursiveEnum1.EmptyCase))) == EqualTo

    @test
    def testEq24(): Bool = compare(OpaqueType.OpaqueType(123), OpaqueType.OpaqueType(123)) == EqualTo

    @test
    def testLt01(): Bool = Enum.EmptyCase < Enum.SingletonCase(123)

    @test
    def testLt02(): Bool = Enum.SingletonCase(123) < Enum.SingletonCase(456)

    @test
    def testLt03(): Bool = PolyEnum.EmptyCase < PolyEnum.SingletonCase(123)

    @test
    def testLt04(): Bool = PolyEnum.SingletonCase(123) < PolyEnum.SingletonCase(456)

    @test
    def testLt05(): Bool = PolyEnum.SingletonCase(Enum.EmptyCase) < PolyEnum.SingletonCase(Enum.SingletonCase(123))

    @test
    def testLt06(): Bool = RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase) < RecursiveEnum.RecursiveCase(RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase))

    @test
    def testLt07(): Bool = PolyMultiEnum.LeftCase(123) < PolyMultiEnum.RightCase(123)

    @test
    def testLt08(): Bool = PolyBoolEnum.EmptyCase: PolyBoolEnum[Pure] < PolyBoolEnum.RecursiveCase(PolyBoolEnum.EmptyCase): PolyBoolEnum[Pure]

    @test
    def testLt09(): Bool = MutRecursiveEnum1.EmptyCase < MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.EmptyCase)

    @test
    def testLt10(): Bool = OpaqueType.OpaqueType(123) < OpaqueType.OpaqueType(456)
}

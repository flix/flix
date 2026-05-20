mod Test.Derives.Order {

    use Assert.{assertEq, assertTrue}
    use Order.compare
    use Comparison.{EqualTo, LessThan, GreaterThan}

    pub enum Enum with Eq, Order, ToString {
      case EmptyCase
      case SingletonCase(Int32)
      case TupleCase(Int32, Bool)
    }

    pub enum PolyEnum[a] with Eq, Order, ToString {
        case EmptyCase
        case SingletonCase(a)
        case TupleCase(a, a)
    }

    pub enum RecursiveEnum with Eq, Order, ToString {
        case EmptyCase
        case RecursiveCase(RecursiveEnum)
    }

    pub enum PolyMultiEnum[a, b] with Eq, Order, ToString {
        case EmptyCase
        case LeftCase(a)
        case RightCase(b)
        case BothCase(a, b)
    }

    pub enum PolyBoolEnum[ef: Eff] with Eq, Order, ToString {
        case EmptyCase
        case RecursiveCase(PolyBoolEnum[ef])
    }

    pub enum MutRecursiveEnum1 with Eq, Order, ToString {
        case EmptyCase
        case RecursiveCase(MutRecursiveEnum2)
    }

    pub enum MutRecursiveEnum2 with Eq, Order, ToString {
        case EmptyCase
        case RecursiveCase(MutRecursiveEnum1)
    }

    pub enum OpaqueType(Int32) with Eq, Order, ToString

    /// All-nullary enum (no data-carrying variants).
    pub enum AllNullaryEnum with Eq, Order, ToString {
        case A
        case B
        case C
    }

    /// Many nullary variants with a single data-carrying variant.
    pub enum MostlyNullaryEnum with Eq, Order, ToString {
        case N1
        case N2
        case N3
        case N4
        case N5
        case Data(Int32)
    }

    /// All data-carrying variants (no nullary).
    pub enum AllDataEnum with Eq, Order, ToString {
        case First(Int32)
        case Second(Bool)
    }

    @Test
    def testEq01(): Unit \ Assert =
        assertEq(expected = EqualTo, compare(Enum.EmptyCase, Enum.EmptyCase))

    @Test
    def testEq02(): Unit \ Assert =
        assertEq(expected = EqualTo, compare(Enum.SingletonCase(123), Enum.SingletonCase(123)))

    @Test
    def testEq03(): Unit \ Assert =
        assertEq(expected = EqualTo, compare(Enum.TupleCase(123, true), Enum.TupleCase(123, true)))

    @Test
    def testEq04(): Unit \ Assert =
        assertEq(expected = EqualTo, compare((PolyEnum.EmptyCase: PolyEnum[Int32]), (PolyEnum.EmptyCase: PolyEnum[Int32])))

    @Test
    def testEq05(): Unit \ Assert =
        assertEq(expected = EqualTo, compare(PolyEnum.SingletonCase(123), PolyEnum.SingletonCase(123)))

    @Test
    def testEq06(): Unit \ Assert =
        assertEq(expected = EqualTo, compare(PolyEnum.TupleCase(123, 123), PolyEnum.TupleCase(123, 123)))

    @Test
    def testEq07(): Unit \ Assert =
        assertEq(expected = EqualTo, compare((PolyEnum.EmptyCase: PolyEnum[Enum]), (PolyEnum.EmptyCase: PolyEnum[Enum])))

    @Test
    def testEq08(): Unit \ Assert =
        assertEq(expected = EqualTo, compare(PolyEnum.SingletonCase(Enum.EmptyCase), PolyEnum.SingletonCase(Enum.EmptyCase)))

    @Test
    def testEq09(): Unit \ Assert =
        assertEq(expected = EqualTo, compare(PolyEnum.SingletonCase(Enum.SingletonCase(123)), PolyEnum.SingletonCase(Enum.SingletonCase(123))))

    @Test
    def testEq10(): Unit \ Assert =
        assertEq(expected = EqualTo, compare((PolyEnum.EmptyCase: PolyEnum[PolyEnum[Int32]]), (PolyEnum.EmptyCase: PolyEnum[PolyEnum[Int32]])))

    @Test
    def testEq11(): Unit \ Assert =
        assertEq(expected = EqualTo, compare((PolyEnum.SingletonCase(PolyEnum.EmptyCase): PolyEnum[PolyEnum[Int32]]), (PolyEnum.SingletonCase(PolyEnum.EmptyCase): PolyEnum[PolyEnum[Int32]])))

    @Test
    def testEq12(): Unit \ Assert =
        assertEq(expected = EqualTo, compare(PolyEnum.SingletonCase(PolyEnum.SingletonCase(123)), PolyEnum.SingletonCase(PolyEnum.SingletonCase(123))))

    @Test
    def testEq13(): Unit \ Assert =
        assertEq(expected = EqualTo, compare(RecursiveEnum.EmptyCase, RecursiveEnum.EmptyCase))

    @Test
    def testEq14(): Unit \ Assert =
        assertEq(expected = EqualTo, compare(RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase), RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase)))

    @Test
    def testEq15(): Unit \ Assert =
        assertEq(expected = EqualTo, compare((PolyMultiEnum.EmptyCase: PolyMultiEnum[Int32, Bool]), (PolyMultiEnum.EmptyCase: PolyMultiEnum[Int32, Bool])))

    @Test
    def testEq16(): Unit \ Assert =
        assertEq(expected = EqualTo, compare((PolyMultiEnum.LeftCase(123): PolyMultiEnum[Int32, Bool]), (PolyMultiEnum.LeftCase(123): PolyMultiEnum[Int32, Bool])))

    @Test
    def testEq17(): Unit \ Assert =
        assertEq(expected = EqualTo, compare((PolyMultiEnum.RightCase(true): PolyMultiEnum[Int32, Bool]), (PolyMultiEnum.RightCase(true): PolyMultiEnum[Int32, Bool])))

    @Test
    def testEq18(): Unit \ Assert =
        assertEq(expected = EqualTo, compare(PolyMultiEnum.BothCase(123, true), PolyMultiEnum.BothCase(123, true)))

    @Test
    def testEq19(): Unit \ Assert =
        assertEq(expected = EqualTo, compare((PolyBoolEnum.EmptyCase: PolyBoolEnum[{}]), (PolyBoolEnum.EmptyCase: PolyBoolEnum[{}])))

    @Test
    def testEq20(): Unit \ Assert =
        assertEq(expected = EqualTo, compare((PolyBoolEnum.RecursiveCase(PolyBoolEnum.EmptyCase): PolyBoolEnum[{}]), (PolyBoolEnum.RecursiveCase(PolyBoolEnum.EmptyCase): PolyBoolEnum[{}])))

    @Test
    def testEq21(): Unit \ Assert =
        assertEq(expected = EqualTo, compare(MutRecursiveEnum1.EmptyCase, MutRecursiveEnum1.EmptyCase))

    @Test
    def testEq22(): Unit \ Assert =
        assertEq(expected = EqualTo, compare(MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.EmptyCase), MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.EmptyCase)))

    @Test
    def testEq23(): Unit \ Assert =
        assertEq(expected = EqualTo, compare(MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.RecursiveCase(MutRecursiveEnum1.EmptyCase)), MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.RecursiveCase(MutRecursiveEnum1.EmptyCase))))

    @Test
    def testEq24(): Unit \ Assert =
        assertEq(expected = EqualTo, compare(OpaqueType.OpaqueType(123), OpaqueType.OpaqueType(123)))

    // All-nullary enum tests
    @Test
    def testEqAllNullary01(): Unit \ Assert =
        assertEq(expected = EqualTo, compare(AllNullaryEnum.A, AllNullaryEnum.A))

    @Test
    def testLtAllNullary01(): Unit \ Assert =
        assertTrue(AllNullaryEnum.A < AllNullaryEnum.B)

    @Test
    def testLtAllNullary02(): Unit \ Assert =
        assertTrue(AllNullaryEnum.B < AllNullaryEnum.C)

    @Test
    def testGtAllNullary01(): Unit \ Assert =
        assertTrue(AllNullaryEnum.C > AllNullaryEnum.A)

    // Mostly-nullary enum tests
    @Test
    def testEqMostlyNullary01(): Unit \ Assert =
        assertEq(expected = EqualTo, compare(MostlyNullaryEnum.N1, MostlyNullaryEnum.N1))

    @Test
    def testEqMostlyNullary02(): Unit \ Assert =
        assertEq(expected = EqualTo, compare(MostlyNullaryEnum.Data(42), MostlyNullaryEnum.Data(42)))

    @Test
    def testLtMostlyNullary01(): Unit \ Assert =
        assertTrue(MostlyNullaryEnum.N1 < MostlyNullaryEnum.N2)

    @Test
    def testLtMostlyNullary02(): Unit \ Assert =
        assertTrue(MostlyNullaryEnum.N5 < MostlyNullaryEnum.Data(1))

    @Test
    def testLtMostlyNullary03(): Unit \ Assert =
        assertTrue(MostlyNullaryEnum.Data(1) < MostlyNullaryEnum.Data(2))

    // All-data enum tests
    @Test
    def testEqAllData01(): Unit \ Assert =
        assertEq(expected = EqualTo, compare(AllDataEnum.First(123), AllDataEnum.First(123)))

    @Test
    def testLtAllData01(): Unit \ Assert =
        assertTrue(AllDataEnum.First(1) < AllDataEnum.First(2))

    @Test
    def testLtAllData02(): Unit \ Assert =
        assertTrue(AllDataEnum.First(1) < AllDataEnum.Second(true))

    @Test
    def testLt01(): Unit \ Assert =
        assertTrue(Enum.EmptyCase < Enum.SingletonCase(123))

    @Test
    def testLt02(): Unit \ Assert =
        assertTrue(Enum.SingletonCase(123) < Enum.SingletonCase(456))

    @Test
    def testLt03(): Unit \ Assert =
        assertTrue(PolyEnum.EmptyCase < PolyEnum.SingletonCase(123))

    @Test
    def testLt04(): Unit \ Assert =
        assertTrue(PolyEnum.SingletonCase(123) < PolyEnum.SingletonCase(456))

    @Test
    def testLt05(): Unit \ Assert =
        assertTrue(PolyEnum.SingletonCase(Enum.EmptyCase) < PolyEnum.SingletonCase(Enum.SingletonCase(123)))

    @Test
    def testLt06(): Unit \ Assert =
        assertTrue(RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase) < RecursiveEnum.RecursiveCase(RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase)))

    @Test
    def testLt07(): Unit \ Assert =
        assertTrue(PolyMultiEnum.LeftCase(123) < PolyMultiEnum.RightCase(123))

    @Test
    def testLt08(): Unit \ Assert =
        assertTrue((PolyBoolEnum.EmptyCase: PolyBoolEnum[{}]) < (PolyBoolEnum.RecursiveCase(PolyBoolEnum.EmptyCase): PolyBoolEnum[{}]))

    @Test
    def testLt09(): Unit \ Assert =
        assertTrue(MutRecursiveEnum1.EmptyCase < MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.EmptyCase))

    @Test
    def testLt10(): Unit \ Assert =
        assertTrue(OpaqueType.OpaqueType(123) < OpaqueType.OpaqueType(456))
}

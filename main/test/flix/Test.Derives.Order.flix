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

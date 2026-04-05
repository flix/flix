mod Test.Derives.Eq {

    use Assert.{assertEq, assertNeq}

    pub enum Enum with Eq, ToString {
      case EmptyCase
      case SingletonCase(Int32)
      case TupleCase(Int32, Bool)
    }

    pub enum PolyEnum[a] with Eq, ToString {
        case EmptyCase
        case SingletonCase(a)
        case TupleCase(a, a)
    }

    pub enum RecursiveEnum with Eq, ToString {
        case EmptyCase
        case RecursiveCase(RecursiveEnum)
    }

    pub enum PolyMultiEnum[a, b] with Eq, ToString {
        case EmptyCase
        case LeftCase(a)
        case RightCase(b)
        case BothCase(a, b)
    }

    pub enum PolyBoolEnum[ef: Eff] with Eq, ToString {
        case EmptyCase
        case RecursiveCase(PolyBoolEnum[ef])
    }

    pub enum MutRecursiveEnum1 with Eq, ToString {
        case EmptyCase
        case RecursiveCase(MutRecursiveEnum2)
    }

    pub enum MutRecursiveEnum2 with Eq, ToString {
        case EmptyCase
        case RecursiveCase(MutRecursiveEnum1)
    }

    pub enum OpaqueType(Int32) with Eq, ToString

    /// All-nullary enum (no data-carrying variants).
    pub enum AllNullaryEnum with Eq, ToString {
        case A
        case B
        case C
    }

    /// Many nullary variants with a single data-carrying variant.
    pub enum MostlyNullaryEnum with Eq, ToString {
        case N1
        case N2
        case N3
        case N4
        case N5
        case Data(Int32)
    }

    /// All data-carrying variants (no nullary).
    pub enum AllDataEnum with Eq, ToString {
        case First(Int32)
        case Second(Bool)
    }

    @Test
    def testEq01(): Unit \ Assert =
        assertEq(expected = Enum.EmptyCase, Enum.EmptyCase)

    @Test
    def testEq02(): Unit \ Assert =
        assertEq(expected = Enum.SingletonCase(123), Enum.SingletonCase(123))

    @Test
    def testEq03(): Unit \ Assert =
        assertEq(expected = Enum.TupleCase(123, true), Enum.TupleCase(123, true))

    @Test
    def testEq04(): Unit \ Assert =
        assertEq(expected = (PolyEnum.EmptyCase: PolyEnum[Int32]), (PolyEnum.EmptyCase: PolyEnum[Int32]))

    @Test
    def testEq05(): Unit \ Assert =
        assertEq(expected = PolyEnum.SingletonCase(123), PolyEnum.SingletonCase(123))

    @Test
    def testEq06(): Unit \ Assert =
        assertEq(expected = PolyEnum.TupleCase(123, 123), PolyEnum.TupleCase(123, 123))

    @Test
    def testEq07(): Unit \ Assert =
        assertEq(expected = (PolyEnum.EmptyCase: PolyEnum[Enum]), (PolyEnum.EmptyCase: PolyEnum[Enum]))

    @Test
    def testEq08(): Unit \ Assert =
        assertEq(expected = PolyEnum.SingletonCase(Enum.EmptyCase), PolyEnum.SingletonCase(Enum.EmptyCase))

    @Test
    def testEq09(): Unit \ Assert =
        assertEq(expected = PolyEnum.SingletonCase(Enum.SingletonCase(123)), PolyEnum.SingletonCase(Enum.SingletonCase(123)))

    @Test
    def testEq10(): Unit \ Assert =
        assertEq(expected = (PolyEnum.EmptyCase: PolyEnum[PolyEnum[Int32]]), (PolyEnum.EmptyCase: PolyEnum[PolyEnum[Int32]]))

    @Test
    def testEq11(): Unit \ Assert =
        assertEq(expected = (PolyEnum.SingletonCase(PolyEnum.EmptyCase): PolyEnum[PolyEnum[Int32]]), (PolyEnum.SingletonCase(PolyEnum.EmptyCase): PolyEnum[PolyEnum[Int32]]))

    @Test
    def testEq12(): Unit \ Assert =
        assertEq(expected = PolyEnum.SingletonCase(PolyEnum.SingletonCase(123)), PolyEnum.SingletonCase(PolyEnum.SingletonCase(123)))

    @Test
    def testEq13(): Unit \ Assert =
        assertEq(expected = RecursiveEnum.EmptyCase, RecursiveEnum.EmptyCase)

    @Test
    def testEq14(): Unit \ Assert =
        assertEq(expected = RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase), RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase))

    @Test
    def testEq15(): Unit \ Assert =
        assertEq(expected = (PolyMultiEnum.EmptyCase: PolyMultiEnum[Int32, Bool]), (PolyMultiEnum.EmptyCase: PolyMultiEnum[Int32, Bool]))

    @Test
    def testEq16(): Unit \ Assert =
        assertEq(expected = (PolyMultiEnum.LeftCase(123): PolyMultiEnum[Int32, Bool]), (PolyMultiEnum.LeftCase(123): PolyMultiEnum[Int32, Bool]))

    @Test
    def testEq17(): Unit \ Assert =
        assertEq(expected = (PolyMultiEnum.RightCase(true): PolyMultiEnum[Int32, Bool]), (PolyMultiEnum.RightCase(true): PolyMultiEnum[Int32, Bool]))

    @Test
    def testEq18(): Unit \ Assert =
        assertEq(expected = PolyMultiEnum.BothCase(123, true), PolyMultiEnum.BothCase(123, true))

    @Test
    def testEq19(): Unit \ Assert =
        assertEq(expected = (PolyBoolEnum.EmptyCase: PolyBoolEnum[{}]), (PolyBoolEnum.EmptyCase: PolyBoolEnum[{}]))

    @Test
    def testEq20(): Unit \ Assert =
        assertEq(expected = (PolyBoolEnum.RecursiveCase(PolyBoolEnum.EmptyCase): PolyBoolEnum[{}]), (PolyBoolEnum.RecursiveCase(PolyBoolEnum.EmptyCase): PolyBoolEnum[{}]))

    @Test
    def testEq21(): Unit \ Assert =
        assertEq(expected = MutRecursiveEnum1.EmptyCase, MutRecursiveEnum1.EmptyCase)

    @Test
    def testEq22(): Unit \ Assert =
        assertEq(expected = MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.EmptyCase), MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.EmptyCase))

    @Test
    def testEq23(): Unit \ Assert =
        assertEq(expected = MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.RecursiveCase(MutRecursiveEnum1.EmptyCase)), MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.RecursiveCase(MutRecursiveEnum1.EmptyCase)))

    @Test
    def testEq24(): Unit \ Assert =
        assertEq(expected = OpaqueType.OpaqueType(123), OpaqueType.OpaqueType(123))

    // All-nullary enum tests
    @Test
    def testEqAllNullary01(): Unit \ Assert =
        assertEq(expected = AllNullaryEnum.A, AllNullaryEnum.A)

    @Test
    def testEqAllNullary02(): Unit \ Assert =
        assertEq(expected = AllNullaryEnum.B, AllNullaryEnum.B)

    @Test
    def testNeqAllNullary01(): Unit \ Assert =
        assertNeq(unexpected = AllNullaryEnum.A, AllNullaryEnum.B)

    @Test
    def testNeqAllNullary02(): Unit \ Assert =
        assertNeq(unexpected = AllNullaryEnum.A, AllNullaryEnum.C)

    // Mostly-nullary enum tests
    @Test
    def testEqMostlyNullary01(): Unit \ Assert =
        assertEq(expected = MostlyNullaryEnum.N1, MostlyNullaryEnum.N1)

    @Test
    def testEqMostlyNullary02(): Unit \ Assert =
        assertEq(expected = MostlyNullaryEnum.Data(42), MostlyNullaryEnum.Data(42))

    @Test
    def testNeqMostlyNullary01(): Unit \ Assert =
        assertNeq(unexpected = MostlyNullaryEnum.N1, MostlyNullaryEnum.N2)

    @Test
    def testNeqMostlyNullary02(): Unit \ Assert =
        assertNeq(unexpected = MostlyNullaryEnum.Data(1), MostlyNullaryEnum.Data(2))

    @Test
    def testNeqMostlyNullary03(): Unit \ Assert =
        assertNeq(unexpected = MostlyNullaryEnum.N1, MostlyNullaryEnum.Data(1))

    // All-data enum tests
    @Test
    def testEqAllData01(): Unit \ Assert =
        assertEq(expected = AllDataEnum.First(123), AllDataEnum.First(123))

    @Test
    def testEqAllData02(): Unit \ Assert =
        assertEq(expected = AllDataEnum.Second(true), AllDataEnum.Second(true))

    @Test
    def testNeqAllData01(): Unit \ Assert =
        assertNeq(unexpected = AllDataEnum.First(1), AllDataEnum.First(2))

    @Test
    def testNeqAllData02(): Unit \ Assert =
        assertNeq(unexpected = AllDataEnum.First(1), AllDataEnum.Second(true))

    @Test
    def testNeq01(): Unit \ Assert =
        assertNeq(unexpected = Enum.EmptyCase, Enum.SingletonCase(123))

    @Test
    def testNeq02(): Unit \ Assert =
        assertNeq(unexpected = Enum.SingletonCase(123), Enum.SingletonCase(456))

    @Test
    def testNeq03(): Unit \ Assert =
        assertNeq(unexpected = PolyEnum.EmptyCase, PolyEnum.SingletonCase(123))

    @Test
    def testNeq04(): Unit \ Assert =
        assertNeq(unexpected = PolyEnum.SingletonCase(123), PolyEnum.SingletonCase(456))

    @Test
    def testNeq05(): Unit \ Assert =
        assertNeq(unexpected = PolyEnum.SingletonCase(Enum.EmptyCase), PolyEnum.SingletonCase(Enum.SingletonCase(123)))

    @Test
    def testNeq06(): Unit \ Assert =
        assertNeq(unexpected = RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase), RecursiveEnum.RecursiveCase(RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase)))

    @Test
    def testNeq07(): Unit \ Assert =
        assertNeq(unexpected = PolyMultiEnum.LeftCase(123), PolyMultiEnum.RightCase(123))

    @Test
    def testNeq08(): Unit \ Assert =
        assertNeq(unexpected = (PolyBoolEnum.EmptyCase: PolyBoolEnum[{}]), (PolyBoolEnum.RecursiveCase(PolyBoolEnum.EmptyCase): PolyBoolEnum[{}]))

    @Test
    def testNeq09(): Unit \ Assert =
        assertNeq(unexpected = MutRecursiveEnum1.EmptyCase, MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.EmptyCase))

    @Test
    def testNeq10(): Unit \ Assert =
        assertNeq(unexpected = OpaqueType.OpaqueType(123), OpaqueType.OpaqueType(456))
}

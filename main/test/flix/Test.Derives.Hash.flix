mod Test.Derives.Hash {

    use Assert.{assertEq, assertNeq}
    use Hash.hash

    pub enum Enum with Hash, ToString {
      case EmptyCase
      case SingletonCase(Int32)
      case TupleCase(Int32, Bool)
    }

    pub enum PolyEnum[a] with Hash, ToString {
        case EmptyCase
        case SingletonCase(a)
        case TupleCase(a, a)
    }

    pub enum RecursiveEnum with Hash, ToString {
        case EmptyCase
        case RecursiveCase(RecursiveEnum)
    }

    pub enum PolyMultiEnum[a, b] with Hash, ToString {
        case EmptyCase
        case LeftCase(a)
        case RightCase(b)
        case BothCase(a, b)
    }

    pub enum PolyBoolEnum[ef: Eff] with Hash, ToString {
        case EmptyCase
        case RecursiveCase(PolyBoolEnum[ef])
    }

    pub enum MutRecursiveEnum1 with Hash, ToString {
        case EmptyCase
        case RecursiveCase(MutRecursiveEnum2)
    }

    pub enum MutRecursiveEnum2 with Hash, ToString {
        case EmptyCase
        case RecursiveCase(MutRecursiveEnum1)
    }

    pub enum OpaqueType(Int32) with Hash, ToString

    @Test
    def testHashEq01(): Unit \ Assert =
        assertEq(expected = hash(Enum.EmptyCase), hash(Enum.EmptyCase))

    @Test
    def testHashEq02(): Unit \ Assert =
        assertEq(expected = hash(Enum.SingletonCase(123)), hash(Enum.SingletonCase(123)))

    @Test
    def testHashEq03(): Unit \ Assert =
        assertEq(expected = hash(Enum.TupleCase(123, true)), hash(Enum.TupleCase(123, true)))

    @Test
    def testHashEq04(): Unit \ Assert =
        assertEq(expected = hash((PolyEnum.EmptyCase: PolyEnum[Int32])), hash((PolyEnum.EmptyCase: PolyEnum[Int32])))

    @Test
    def testHashEq05(): Unit \ Assert =
        assertEq(expected = hash(PolyEnum.SingletonCase(123)), hash(PolyEnum.SingletonCase(123)))

    @Test
    def testHashEq06(): Unit \ Assert =
        assertEq(expected = hash(PolyEnum.TupleCase(123, 123)), hash(PolyEnum.TupleCase(123, 123)))

    @Test
    def testHashEq07(): Unit \ Assert =
        assertEq(expected = hash((PolyEnum.EmptyCase: PolyEnum[Enum])), hash((PolyEnum.EmptyCase: PolyEnum[Enum])))

    @Test
    def testHashEq08(): Unit \ Assert =
        assertEq(expected = hash(PolyEnum.SingletonCase(Enum.EmptyCase)), hash(PolyEnum.SingletonCase(Enum.EmptyCase)))

    @Test
    def testHashEq09(): Unit \ Assert =
        assertEq(expected = hash(PolyEnum.SingletonCase(Enum.SingletonCase(123))), hash(PolyEnum.SingletonCase(Enum.SingletonCase(123))))

    @Test
    def testHashEq10(): Unit \ Assert =
        assertEq(expected = hash((PolyEnum.EmptyCase: PolyEnum[PolyEnum[Int32]])), hash((PolyEnum.EmptyCase: PolyEnum[PolyEnum[Int32]])))

    @Test
    def testHashEq11(): Unit \ Assert =
        assertEq(expected = hash((PolyEnum.SingletonCase(PolyEnum.EmptyCase): PolyEnum[PolyEnum[Int32]])), hash((PolyEnum.SingletonCase(PolyEnum.EmptyCase): PolyEnum[PolyEnum[Int32]])))

    @Test
    def testHashEq12(): Unit \ Assert =
        assertEq(expected = hash(PolyEnum.SingletonCase(PolyEnum.SingletonCase(123))), hash(PolyEnum.SingletonCase(PolyEnum.SingletonCase(123))))

    @Test
    def testHashEq13(): Unit \ Assert =
        assertEq(expected = hash(RecursiveEnum.EmptyCase), hash(RecursiveEnum.EmptyCase))

    @Test
    def testHashEq14(): Unit \ Assert =
        assertEq(expected = hash(RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase)), hash(RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase)))

    @Test
    def testHashEq15(): Unit \ Assert =
        assertEq(expected = hash((PolyMultiEnum.EmptyCase: PolyMultiEnum[Int32, Bool])), hash((PolyMultiEnum.EmptyCase: PolyMultiEnum[Int32, Bool])))

    @Test
    def testHashEq16(): Unit \ Assert =
        assertEq(expected = hash((PolyMultiEnum.LeftCase(123): PolyMultiEnum[Int32, Bool])), hash((PolyMultiEnum.LeftCase(123): PolyMultiEnum[Int32, Bool])))

    @Test
    def testHashEq17(): Unit \ Assert =
        assertEq(expected = hash((PolyMultiEnum.RightCase(true): PolyMultiEnum[Int32, Bool])), hash((PolyMultiEnum.RightCase(true): PolyMultiEnum[Int32, Bool])))

    @Test
    def testHashEq18(): Unit \ Assert =
        assertEq(expected = hash(PolyMultiEnum.BothCase(123, true)), hash(PolyMultiEnum.BothCase(123, true)))

    @Test
    def testHashEq19(): Unit \ Assert =
        assertEq(expected = hash((PolyBoolEnum.EmptyCase: PolyBoolEnum[{}])), hash((PolyBoolEnum.EmptyCase: PolyBoolEnum[{}])))

    @Test
    def testHashEq20(): Unit \ Assert =
        assertEq(expected = hash((PolyBoolEnum.RecursiveCase(PolyBoolEnum.EmptyCase): PolyBoolEnum[{}])), hash((PolyBoolEnum.RecursiveCase(PolyBoolEnum.EmptyCase): PolyBoolEnum[{}])))

    @Test
    def testHashEq21(): Unit \ Assert =
        assertEq(expected = hash(MutRecursiveEnum1.EmptyCase), hash(MutRecursiveEnum1.EmptyCase))

    @Test
    def testHashEq22(): Unit \ Assert =
        assertEq(expected = hash(MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.EmptyCase)), hash(MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.EmptyCase)))

    @Test
    def testHashEq23(): Unit \ Assert =
        assertEq(expected = hash(MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.RecursiveCase(MutRecursiveEnum1.EmptyCase))), hash(MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.RecursiveCase(MutRecursiveEnum1.EmptyCase))))

    @Test
    def testHashEq24(): Unit \ Assert =
        assertEq(expected = hash(OpaqueType.OpaqueType(123)), hash(OpaqueType.OpaqueType(123)))

    @Test
    def testHashNeq01(): Unit \ Assert =
        assertNeq(unexpected = hash(Enum.EmptyCase), hash(Enum.SingletonCase(123)))

    @Test
    def testHashNeq02(): Unit \ Assert =
        assertNeq(unexpected = hash(Enum.SingletonCase(123)), hash(Enum.SingletonCase(456)))

    @Test
    def testHashNeq03(): Unit \ Assert =
        assertNeq(unexpected = hash((PolyEnum.EmptyCase: PolyEnum[Int32])), hash(PolyEnum.SingletonCase(123)))

    @Test
    def testHashNeq04(): Unit \ Assert =
        assertNeq(unexpected = hash(PolyEnum.SingletonCase(123)), hash(PolyEnum.SingletonCase(456)))

    @Test
    def testHashNeq05(): Unit \ Assert =
        assertNeq(unexpected = hash(PolyEnum.SingletonCase(Enum.EmptyCase)), hash(PolyEnum.SingletonCase(Enum.SingletonCase(123))))

    @Test
    def testHashNeq06(): Unit \ Assert =
        assertNeq(unexpected = hash(RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase)), hash(RecursiveEnum.RecursiveCase(RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase))))

    @Test
    def testHashNeq07(): Unit \ Assert =
        assertNeq(unexpected = hash((PolyMultiEnum.LeftCase(123): PolyMultiEnum[Int32, Int32])), hash((PolyMultiEnum.RightCase(123): PolyMultiEnum[Int32, Int32])))

    @Test
    def testHashNeq08(): Unit \ Assert =
        assertNeq(unexpected = hash((PolyBoolEnum.EmptyCase: PolyBoolEnum[{}])), hash((PolyBoolEnum.RecursiveCase(PolyBoolEnum.EmptyCase): PolyBoolEnum[{}])))

    @Test
    def testHashNeq09(): Unit \ Assert =
        assertNeq(unexpected = hash(MutRecursiveEnum1.EmptyCase), hash(MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.EmptyCase)))

    @Test
    def testHashNeq10(): Unit \ Assert =
        assertNeq(unexpected = hash(OpaqueType.OpaqueType(123)), hash(OpaqueType.OpaqueType(456)))
}

mod Test.Derives.ToString {

    use Assert.assertEq

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
    def testToString01(): Unit \ Assert =
        assertEq(expected = "EmptyCase", "${Enum.EmptyCase}")

    @Test
    def testToString02(): Unit \ Assert =
        assertEq(expected = "SingletonCase(123)", "${Enum.SingletonCase(123)}")

    @Test
    def testToString03(): Unit \ Assert =
        assertEq(expected = "TupleCase(123, true)", "${Enum.TupleCase(123, true)}")

    @Test
    def testToString04(): Unit \ Assert =
        assertEq(expected = "EmptyCase", "${(PolyEnum.EmptyCase: PolyEnum[Int32])}")

    @Test
    def testToString05(): Unit \ Assert =
        assertEq(expected = "SingletonCase(123)", "${PolyEnum.SingletonCase(123)}")

    @Test
    def testToString06(): Unit \ Assert =
        assertEq(expected = "TupleCase(123, 123)", "${PolyEnum.TupleCase(123, 123)}")

    @Test
    def testToString07(): Unit \ Assert =
        assertEq(expected = "EmptyCase", "${(PolyEnum.EmptyCase: PolyEnum[Enum])}")

    @Test
    def testToString08(): Unit \ Assert =
        assertEq(expected = "SingletonCase(EmptyCase)", "${PolyEnum.SingletonCase(Enum.EmptyCase)}")

    @Test
    def testToString09(): Unit \ Assert =
        assertEq(expected = "SingletonCase(SingletonCase(123))", "${PolyEnum.SingletonCase(Enum.SingletonCase(123))}")

    @Test
    def testToString10(): Unit \ Assert =
        assertEq(expected = "EmptyCase", "${(PolyEnum.EmptyCase: PolyEnum[PolyEnum[Int32]])}")

    @Test
    def testToString11(): Unit \ Assert =
        assertEq(expected = "SingletonCase(EmptyCase)", "${(PolyEnum.SingletonCase(PolyEnum.EmptyCase): PolyEnum[PolyEnum[Int32]])}")

    @Test
    def testToString12(): Unit \ Assert =
        assertEq(expected = "SingletonCase(SingletonCase(123))", "${PolyEnum.SingletonCase(PolyEnum.SingletonCase(123))}")

    @Test
    def testToString13(): Unit \ Assert =
        assertEq(expected = "EmptyCase", "${RecursiveEnum.EmptyCase}")

    @Test
    def testToString14(): Unit \ Assert =
        assertEq(expected = "RecursiveCase(EmptyCase)", "${RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase)}")

    @Test
    def testToString15(): Unit \ Assert =
        assertEq(expected = "EmptyCase", "${(PolyMultiEnum.EmptyCase: PolyMultiEnum[Int32, Bool])}")

    @Test
    def testToString16(): Unit \ Assert =
        assertEq(expected = "LeftCase(123)", "${(PolyMultiEnum.LeftCase(123): PolyMultiEnum[Int32, Bool])}")

    @Test
    def testToString17(): Unit \ Assert =
        assertEq(expected = "RightCase(true)", "${(PolyMultiEnum.RightCase(true): PolyMultiEnum[Int32, Bool])}")

    @Test
    def testToString18(): Unit \ Assert =
        assertEq(expected = "BothCase(123, true)", "${PolyMultiEnum.BothCase(123, true)}")

    @Test
    def testToString19(): Unit \ Assert =
        assertEq(expected = "EmptyCase", "${(PolyBoolEnum.EmptyCase: PolyBoolEnum[{}])}")

    @Test
    def testToString20(): Unit \ Assert =
        assertEq(expected = "RecursiveCase(EmptyCase)", "${(PolyBoolEnum.RecursiveCase(PolyBoolEnum.EmptyCase): PolyBoolEnum[{}])}")

    @Test
    def testToString21(): Unit \ Assert =
        assertEq(expected = "EmptyCase", "${MutRecursiveEnum1.EmptyCase}")

    @Test
    def testToString22(): Unit \ Assert =
        assertEq(expected = "RecursiveCase(EmptyCase)", "${MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.EmptyCase)}")

    @Test
    def testToString23(): Unit \ Assert =
        assertEq(expected = "RecursiveCase(RecursiveCase(EmptyCase))", "${MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.RecursiveCase(MutRecursiveEnum1.EmptyCase))}")

    @Test
    def testToString24(): Unit \ Assert =
        assertEq(expected = "OpaqueType(123)", "${OpaqueType.OpaqueType(123)}")
}

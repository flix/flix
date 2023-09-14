mod Test.Derives.Hash {
    use Hash.hash

    pub enum Enum with Hash {
      case EmptyCase
      case SingletonCase(Int32)
      case TupleCase(Int32, Bool)
    }

    pub enum PolyEnum[a] with Hash {
        case EmptyCase
        case SingletonCase(a)
        case TupleCase(a, a)
    }

    pub enum RecursiveEnum with Hash {
        case EmptyCase
        case RecursiveCase(RecursiveEnum)
    }

    pub enum PolyMultiEnum[a, b] with Hash {
        case EmptyCase
        case LeftCase(a)
        case RightCase(b)
        case BothCase(a, b)
    }

    pub enum PolyBoolEnum[ef: Eff] with Hash {
        case EmptyCase
        case RecursiveCase(PolyBoolEnum[ef])
    }

    pub enum MutRecursiveEnum1 with Hash {
        case EmptyCase
        case RecursiveCase(MutRecursiveEnum2)
    }

    pub enum MutRecursiveEnum2 with Hash {
        case EmptyCase
        case RecursiveCase(MutRecursiveEnum1)
    }

    pub enum OpaqueType(Int32) with Hash

    @Test
    def testHashEq01(): Bool = hash(Enum.EmptyCase) == hash(Enum.EmptyCase)

    @Test
    def testHashEq02(): Bool = hash(Enum.SingletonCase(123)) == hash(Enum.SingletonCase(123))

    @Test
    def testHashEq03(): Bool = hash(Enum.TupleCase(123, true)) == hash(Enum.TupleCase(123, true))

    @Test
    def testHashEq04(): Bool = hash(PolyEnum.EmptyCase: PolyEnum[Int32]) == hash(PolyEnum.EmptyCase: PolyEnum[Int32])

    @Test
    def testHashEq05(): Bool = hash(PolyEnum.SingletonCase(123)) == hash(PolyEnum.SingletonCase(123))

    @Test
    def testHashEq06(): Bool = hash(PolyEnum.TupleCase(123, 123)) == hash(PolyEnum.TupleCase(123, 123))

    @Test
    def testHashEq07(): Bool = hash(PolyEnum.EmptyCase: PolyEnum[Enum]) == hash(PolyEnum.EmptyCase: PolyEnum[Enum])

    @Test
    def testHashEq08(): Bool = hash(PolyEnum.SingletonCase(Enum.EmptyCase)) == hash(PolyEnum.SingletonCase(Enum.EmptyCase))

    @Test
    def testHashEq09(): Bool = hash(PolyEnum.SingletonCase(Enum.SingletonCase(123))) == hash(PolyEnum.SingletonCase(Enum.SingletonCase(123)))

    @Test
    def testHashEq10(): Bool = hash(PolyEnum.EmptyCase: PolyEnum[PolyEnum[Int32]]) == hash(PolyEnum.EmptyCase: PolyEnum[PolyEnum[Int32]])

    @Test
    def testHashEq11(): Bool = hash(PolyEnum.SingletonCase(PolyEnum.EmptyCase): PolyEnum[PolyEnum[Int32]]) == hash(PolyEnum.SingletonCase(PolyEnum.EmptyCase): PolyEnum[PolyEnum[Int32]])

    @Test
    def testHashEq12(): Bool = hash(PolyEnum.SingletonCase(PolyEnum.SingletonCase(123))) == hash(PolyEnum.SingletonCase(PolyEnum.SingletonCase(123)))

    @Test
    def testHashEq13(): Bool = hash(RecursiveEnum.EmptyCase) == hash(RecursiveEnum.EmptyCase)

    @Test
    def testHashEq14(): Bool = hash(RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase)) == hash(RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase))

    @Test
    def testHashEq15(): Bool = hash(PolyMultiEnum.EmptyCase: PolyMultiEnum[Int32, Bool]) == hash(PolyMultiEnum.EmptyCase: PolyMultiEnum[Int32, Bool])

    @Test
    def testHashEq16(): Bool = hash(PolyMultiEnum.LeftCase(123): PolyMultiEnum[Int32, Bool]) == hash(PolyMultiEnum.LeftCase(123): PolyMultiEnum[Int32, Bool])

    @Test
    def testHashEq17(): Bool = hash(PolyMultiEnum.RightCase(true): PolyMultiEnum[Int32, Bool]) == hash(PolyMultiEnum.RightCase(true): PolyMultiEnum[Int32, Bool])

    @Test
    def testHashEq18(): Bool = hash(PolyMultiEnum.BothCase(123, true)) == hash(PolyMultiEnum.BothCase(123, true))

    @Test
    def testHashEq19(): Bool = hash(PolyBoolEnum.EmptyCase: PolyBoolEnum[Pure]) == hash(PolyBoolEnum.EmptyCase: PolyBoolEnum[Pure])

    @Test
    def testHashEq20(): Bool = hash(PolyBoolEnum.RecursiveCase(PolyBoolEnum.EmptyCase): PolyBoolEnum[Pure]) == hash(PolyBoolEnum.RecursiveCase(PolyBoolEnum.EmptyCase): PolyBoolEnum[Pure])

    @Test
    def testHashEq21(): Bool = hash(MutRecursiveEnum1.EmptyCase) == hash(MutRecursiveEnum1.EmptyCase)

    @Test
    def testHashEq22(): Bool = hash(MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.EmptyCase)) == hash(MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.EmptyCase))

    @Test
    def testHashEq23(): Bool = hash(MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.RecursiveCase(MutRecursiveEnum1.EmptyCase))) == hash(MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.RecursiveCase(MutRecursiveEnum1.EmptyCase)))

    @Test
    def testHashEq24(): Bool = hash(OpaqueType.OpaqueType(123)) == hash(OpaqueType.OpaqueType(123))

    @Test
    def testHashNeq01(): Bool = hash(Enum.EmptyCase) != hash(Enum.SingletonCase(123))

    @Test
    def testHashNeq02(): Bool = hash(Enum.SingletonCase(123)) != hash(Enum.SingletonCase(456))

    @Test
    def testHashNeq03(): Bool = hash(PolyEnum.EmptyCase: PolyEnum[Int32]) != hash(PolyEnum.SingletonCase(123))

    @Test
    def testHashNeq04(): Bool = hash(PolyEnum.SingletonCase(123)) != hash(PolyEnum.SingletonCase(456))

    @Test
    def testHashNeq05(): Bool = hash(PolyEnum.SingletonCase(Enum.EmptyCase)) != hash(PolyEnum.SingletonCase(Enum.SingletonCase(123)))

    @Test
    def testHashNeq06(): Bool = hash(RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase)) != hash(RecursiveEnum.RecursiveCase(RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase)))

    @Test
    def testHashNeq07(): Bool = hash(PolyMultiEnum.LeftCase(123): PolyMultiEnum[Int32, Int32]) != hash(PolyMultiEnum.RightCase(123): PolyMultiEnum[Int32, Int32])

    @Test
    def testHashNeq08(): Bool = hash(PolyBoolEnum.EmptyCase: PolyBoolEnum[Pure]) != hash(PolyBoolEnum.RecursiveCase(PolyBoolEnum.EmptyCase): PolyBoolEnum[Pure])

    @Test
    def testHashNeq09(): Bool = hash(MutRecursiveEnum1.EmptyCase) != hash(MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.EmptyCase))

    @Test
    def testHashNeq10(): Bool = hash(OpaqueType.OpaqueType(123)) != hash(OpaqueType.OpaqueType(456))
}

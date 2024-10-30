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

    @test
    def testEq01(): Bool = Enum.EmptyCase == Enum.EmptyCase

    @test
    def testEq02(): Bool = Enum.SingletonCase(123) == Enum.SingletonCase(123)

    @test
    def testEq03(): Bool = Enum.TupleCase(123, true) == Enum.TupleCase(123, true)

    @test
    def testEq04(): Bool = (PolyEnum.EmptyCase: PolyEnum[Int32]) == (PolyEnum.EmptyCase: PolyEnum[Int32])

    @test
    def testEq05(): Bool = PolyEnum.SingletonCase(123) == PolyEnum.SingletonCase(123)

    @test
    def testEq06(): Bool = PolyEnum.TupleCase(123, 123) == PolyEnum.TupleCase(123, 123)

    @test
    def testEq07(): Bool = (PolyEnum.EmptyCase: PolyEnum[Enum]) == (PolyEnum.EmptyCase: PolyEnum[Enum])

    @test
    def testEq08(): Bool = PolyEnum.SingletonCase(Enum.EmptyCase) == PolyEnum.SingletonCase(Enum.EmptyCase)

    @test
    def testEq09(): Bool = PolyEnum.SingletonCase(Enum.SingletonCase(123)) == PolyEnum.SingletonCase(Enum.SingletonCase(123))

    @test
    def testEq10(): Bool = (PolyEnum.EmptyCase: PolyEnum[PolyEnum[Int32]]) == (PolyEnum.EmptyCase: PolyEnum[PolyEnum[Int32]])

    @test
    def testEq11(): Bool = (PolyEnum.SingletonCase(PolyEnum.EmptyCase): PolyEnum[PolyEnum[Int32]]) == (PolyEnum.SingletonCase(PolyEnum.EmptyCase): PolyEnum[PolyEnum[Int32]])

    @test
    def testEq12(): Bool = PolyEnum.SingletonCase(PolyEnum.SingletonCase(123)) == PolyEnum.SingletonCase(PolyEnum.SingletonCase(123))

    @test
    def testEq13(): Bool = RecursiveEnum.EmptyCase == RecursiveEnum.EmptyCase

    @test
    def testEq14(): Bool = RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase) == RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase)

    @test
    def testEq15(): Bool = (PolyMultiEnum.EmptyCase: PolyMultiEnum[Int32, Bool]) == (PolyMultiEnum.EmptyCase: PolyMultiEnum[Int32, Bool])

    @test
    def testEq16(): Bool = (PolyMultiEnum.LeftCase(123): PolyMultiEnum[Int32, Bool]) == (PolyMultiEnum.LeftCase(123): PolyMultiEnum[Int32, Bool])

    @test
    def testEq17(): Bool = (PolyMultiEnum.RightCase(true): PolyMultiEnum[Int32, Bool]) == (PolyMultiEnum.RightCase(true): PolyMultiEnum[Int32, Bool])

    @test
    def testEq18(): Bool = PolyMultiEnum.BothCase(123, true) == PolyMultiEnum.BothCase(123, true)

    @test
    def testEq19(): Bool = (PolyBoolEnum.EmptyCase: PolyBoolEnum[{}]) == (PolyBoolEnum.EmptyCase: PolyBoolEnum[{}])

    @test
    def testEq20(): Bool = (PolyBoolEnum.RecursiveCase(PolyBoolEnum.EmptyCase): PolyBoolEnum[{}]) == (PolyBoolEnum.RecursiveCase(PolyBoolEnum.EmptyCase): PolyBoolEnum[{}])

    @test
    def testEq21(): Bool = MutRecursiveEnum1.EmptyCase == MutRecursiveEnum1.EmptyCase

    @test
    def testEq22(): Bool = MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.EmptyCase) == MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.EmptyCase)

    @test
    def testEq23(): Bool = MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.RecursiveCase(MutRecursiveEnum1.EmptyCase)) == MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.RecursiveCase(MutRecursiveEnum1.EmptyCase))

    @test
    def testEq24(): Bool = OpaqueType.OpaqueType(123) == OpaqueType.OpaqueType(123)

    @test
    def testNeq01(): Bool = Enum.EmptyCase != Enum.SingletonCase(123)

    @test
    def testNeq02(): Bool = Enum.SingletonCase(123) != Enum.SingletonCase(456)

    @test
    def testNeq03(): Bool = PolyEnum.EmptyCase != PolyEnum.SingletonCase(123)

    @test
    def testNeq04(): Bool = PolyEnum.SingletonCase(123) != PolyEnum.SingletonCase(456)

    @test
    def testNeq05(): Bool = PolyEnum.SingletonCase(Enum.EmptyCase) != PolyEnum.SingletonCase(Enum.SingletonCase(123))

    @test
    def testNeq06(): Bool = RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase) != RecursiveEnum.RecursiveCase(RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase))

    @test
    def testNeq07(): Bool = PolyMultiEnum.LeftCase(123) != PolyMultiEnum.RightCase(123)

    @test
    def testNeq08(): Bool = (PolyBoolEnum.EmptyCase: PolyBoolEnum[{}]) != (PolyBoolEnum.RecursiveCase(PolyBoolEnum.EmptyCase): PolyBoolEnum[{}])

    @test
    def testNeq09(): Bool = MutRecursiveEnum1.EmptyCase != MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.EmptyCase)

    @test
    def testNeq10(): Bool = OpaqueType.OpaqueType(123) != OpaqueType.OpaqueType(456)
}

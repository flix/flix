namespace Test/Derives/Eq {
    pub enum Enum with Eq {
      case EmptyCase
      case SingletonCase(Int)
      case TupleCase(Int, Bool)
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

    pub enum PolyBoolEnum[ef: Bool] with Eq {
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

    @test
    def testEq01(): Bool = Enum.EmptyCase == Enum.EmptyCase

    @test
    def testEq02(): Bool = Enum.SingletonCase(123) == Enum.SingletonCase(123)

    @test
    def testEq03(): Bool = Enum.TupleCase(123, true) == Enum.TupleCase(123, true)

    @test
    def testEq04(): Bool = PolyEnum.EmptyCase: PolyEnum[Int] == PolyEnum.EmptyCase: PolyEnum[Int]

    @test
    def testEq05(): Bool = PolyEnum.SingletonCase(123) == PolyEnum.SingletonCase(123)

    @test
    def testEq06(): Bool = PolyEnum.TupleCase(123, 123) == PolyEnum.TupleCase(123, 123)

    @test
    def testEq07(): Bool = RecursiveEnum.EmptyCase == RecursiveEnum.EmptyCase

    @test
    def testEq08(): Bool = RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase) == RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase)

    @test
    def testEq09(): Bool = PolyMultiEnum.EmptyCase: PolyMultiEnum[Int, Bool] == PolyMultiEnum.EmptyCase: PolyMultiEnum[Int, Bool]

    @test
    def testEq10(): Bool = PolyMultiEnum.LeftCase(123): PolyMultiEnum[Int, Bool] == PolyMultiEnum.LeftCase(123): PolyMultiEnum[Int, Bool]

    @test
    def testEq11(): Bool = PolyMultiEnum.RightCase(true): PolyMultiEnum[Int, Bool] == PolyMultiEnum.RightCase(true): PolyMultiEnum[Int, Bool]

    @test
    def testEq12(): Bool = PolyMultiEnum.BothCase(123, true) == PolyMultiEnum.BothCase(123, true)

    @test
    def testEq13(): Bool = PolyBoolEnum.EmptyCase: PolyBoolEnum[Pure] == PolyBoolEnum.EmptyCase: PolyBoolEnum[Pure]

    @test
    def testEq14(): Bool = PolyBoolEnum.RecursiveCase(PolyBoolEnum.EmptyCase): PolyBoolEnum[Pure] == PolyBoolEnum.RecursiveCase(PolyBoolEnum.EmptyCase): PolyBoolEnum[Pure]

    @test
    def testEq15(): Bool = MutRecursiveEnum1.EmptyCase == MutRecursiveEnum1.EmptyCase

    @test
    def testEq16(): Bool = MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.EmptyCase) == MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.EmptyCase)

    @test
    def testEq17(): Bool = MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.RecursiveCase(MutRecursiveEnum1.EmptyCase)) == MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.RecursiveCase(MutRecursiveEnum1.EmptyCase))

    @test
    def testNeq01(): Bool = Enum.EmptyCase != Enum.SingletonCase(123)

    @test
    def testNeq02(): Bool = Enum.SingletonCase(123) != Enum.SingletonCase(456)

    @test
    def testNeq03(): Bool = PolyEnum.EmptyCase != PolyEnum.SingletonCase(123)

    @test
    def testNeq04(): Bool = PolyEnum.SingletonCase(123) != PolyEnum.SingletonCase(456)

    @test
    def testNeq05(): Bool = RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase) != RecursiveEnum.RecursiveCase(RecursiveEnum.RecursiveCase(RecursiveEnum.EmptyCase))

    @test
    def testNeq06(): Bool = PolyMultiEnum.LeftCase(123) != PolyMultiEnum.RightCase(123)

    @test
    def testNeq07(): Bool = PolyBoolEnum.EmptyCase: PolyBoolEnum[Pure] != PolyBoolEnum.RecursiveCase(PolyBoolEnum.EmptyCase): PolyBoolEnum[Pure]

    @test
    def testNeq08(): Bool = MutRecursiveEnum1.EmptyCase != MutRecursiveEnum1.RecursiveCase(MutRecursiveEnum2.EmptyCase)
}

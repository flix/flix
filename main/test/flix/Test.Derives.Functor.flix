namespace Test/Derives/Functor {

    pub enum PolyEnum[a] with Eq, Functor {
        case EmptyCase
        case SingletonCase(a)
        case TupleCase(a, a)
    }

    pub enum PolyMultiEnum[a, b] with Eq, Functor {
        case EmptyCase
        case LeftCase(a)
        case RightCase(b)
        case BothCase(a, b)
    }

    pub enum OpaqueType[a](a) with Eq, Functor

    pub enum PolyRecursiveEnum[a] with Eq, Functor {
        case EmptyCase
        case SingletonCase(a)
        case RecursiveCase(a, PolyRecursiveEnum[a])
    }

    pub enum FunctorInsideEnum[a] with Eq, Functor {
        case EmptyCase
        case SingletonCase(a)
        case FunctorInsideCase(PolyEnum[a])
    }

    @test
    def testFunctor01(): Bool = Functor.map(i -> i * 2, PolyEnum.EmptyCase) == PolyEnum.EmptyCase

    @test
    def testFunctor02(): Bool = Functor.map(i -> i * 2, PolyEnum.SingletonCase(123)) == PolyEnum.SingletonCase(246)

    @test
    def testFunctor03(): Bool = Functor.map(i -> i * 2, PolyEnum.TupleCase(123, 123)) == PolyEnum.TupleCase(246, 246)

    @test
    def testFunctor04(): Bool = Functor.map(i -> i * 2, PolyEnum.EmptyCase) == PolyEnum.EmptyCase

    @test
    def testFunctor05(): Bool = Functor.map(i -> i * 2, PolyMultiEnum.EmptyCase: PolyMultiEnum[Bool, Int32]) == PolyMultiEnum.EmptyCase: PolyMultiEnum[Bool, Int32]

    @test
    def testFunctor06(): Bool = Functor.map(i -> i * 2, PolyMultiEnum.LeftCase(true): PolyMultiEnum[Bool, Int32]) == PolyMultiEnum.LeftCase(true): PolyMultiEnum[Bool, Int32]

    @test
    def testFunctor07(): Bool = Functor.map(i -> i * 2, PolyMultiEnum.RightCase(123): PolyMultiEnum[Bool, Int32]) == PolyMultiEnum.RightCase(246): PolyMultiEnum[Bool, Int32]

    @test
    def testFunctor08(): Bool = PolyMultiEnum.BothCase(123, true) == PolyMultiEnum.BothCase(123, true)

    @test
    def testFunctor09(): Bool = Functor.map(i -> i * 2, OpaqueType.OpaqueType(123)) == OpaqueType.OpaqueType(246)

    @test
    def testFunctor10(): Bool = Functor.map(i -> i * 2, PolyRecursiveEnum.EmptyCase) == PolyRecursiveEnum.EmptyCase

    @test
    def testFunctor11(): Bool = Functor.map(i -> i * 2, PolyRecursiveEnum.SingletonCase(123)) == PolyRecursiveEnum.SingletonCase(246)

    @test
    def testFunctor12(): Bool = Functor.map(i -> i * 2, PolyRecursiveEnum.RecursiveCase(123, PolyRecursiveEnum.SingletonCase(234))) == PolyRecursiveEnum.RecursiveCase(246, PolyRecursiveEnum.SingletonCase(468))

    @test
    def testFunctor13(): Bool = Functor.map(i -> i * 2, FunctorInsideEnum.EmptyCase) == FunctorInsideEnum.EmptyCase

    @test
    def testFunctor14(): Bool = Functor.map(i -> i * 2, FunctorInsideEnum.SingletonCase(123)) == FunctorInsideEnum.SingletonCase(246)

    @test
    def testFunctor15(): Bool = Functor.map(i -> i * 2, FunctorInsideEnum.FunctorInsideCase(PolyEnum.SingletonCase(123))) == FunctorInsideEnum.FunctorInsideCase(PolyEnum.SingletonCase(246))
}

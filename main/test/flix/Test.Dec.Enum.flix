mod Test.Dec.Enum {

    use Assert.{assertEq, assertTrue, success};

    pub enum Empty

    pub enum EmptyWithBraces {}

    pub enum Singleton(Int32)

    pub enum Singleton2(Int32, String)

    @Test
    def example01(): Unit \ Assert =
        match Singleton2.Singleton2(42, "42") {
            case Singleton2.Singleton2(i, s) =>
                assertEq(expected = Int32.toString(i), s)
        }

    pub enum SingletonTuple((Int32, String))

    @Test
    def example02(): Unit \ Assert =
        let t1 = (42, "42");
        match SingletonTuple.SingletonTuple(t1) {
            case SingletonTuple.SingletonTuple(t2) =>
                assertEq(expected = t1, t2)
        }

    pub enum SingletonPoly[a](a)

    pub enum Poly[a] {
        case Zero
        case One(a)
    }

    pub enum PolyMulti[a, b] {
        case Zero
        case OneA(a)
        case OneB(b)
        case Two(a, b)
        case Tuple((a, b))
    }

    @Test
    def example03(): Unit \ Assert =
        match PolyMulti.Zero {
            case PolyMulti.Two(a, b) => assertTrue(a and b)
            case PolyMulti.Tuple(t) => assertTrue(fst(t) and snd(t))
            case _ => success("ok")
        }

    pub enum Recursive {
        case Base
        case Recurse(Recursive)
    }

    pub enum PolyRecursive[a] {
        case Base(a)
        case Recurse(PolyRecursive[a])
    }

    pub enum PolyRecursiveNonRegular[a] {
        case Simple(a)
        case Recurse(PolyRecursiveNonRegular[Poly[a]])
    }
}

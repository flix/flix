mod Test.Dec.Enum {
    pub enum Empty

    pub enum EmptyWithBraces {}

    pub enum Singleton(Int32)

    pub enum Poly[a] {
        case Zero
        case One(a)
    }

    pub enum PolyMulti[a, b] {
        case Zero
        case OneA(a)
        case OneB(b)
        case Two(a, b)
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

mod Test.Dec.Struct {
    pub struct Empty {}

    pub struct Singleton {
        a: Int32
    }

    pub struct Poly[a] {
        zero: Int32,
        one: a
    }

    pub struct PolyMulti[a, b] {
        zero: Int32,
        oneA: a,
        oneB: b,
        two: Map[a, b]
    }

    pub struct Recursive {
        base: Int32,
        recurse: Recursive
    }

    pub struct PolyRecursive[a] {
        base: a,
        recurse: PolyRecursive[a]
    }

    pub struct PolyRecursiveNonRegular[a] {
        simple: a,
        recurse: PolyRecursiveNonRegular[Poly[a]]
    }
}

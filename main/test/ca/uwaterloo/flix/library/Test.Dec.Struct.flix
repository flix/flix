mod Test.Dec.Struct {
    pub struct Empty[r] {}

    pub struct Singleton[r] {
        a: Int32
    }

    pub struct Poly[a, r] {
        zero: Int32,
        one: a
    }

    pub struct PolyMulti[a, b, r] {
        zero: Int32,
        oneA: a,
        oneB: b,
        two: Map[a, b]
    }

    pub struct Recursive[r] {
        base: Int32,
        recurse: Recursive[r]
    }

    pub struct PolyRecursive[a, r] {
        base: a,
        recurse: PolyRecursive[a, r]
    }

    pub struct PolyRecursiveNonRegular[a, r] {
        simple: a,
        recurse: PolyRecursiveNonRegular[Poly[a, r], r]
    }

    pub struct BinaryTree[t, r] {
        l: Option[BinaryTree[t, r]],
        r: Option[BinaryTree[t, r]],
        value: t
    }
}

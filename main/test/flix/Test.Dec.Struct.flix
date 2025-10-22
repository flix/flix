struct S {
    foo: Int32
}

@test
def foo(): Unit = ()

mod Test.Dec.Struct {
    pub struct Empty {}

    pub struct ShortEmpty

    pub struct Singleton {
        singletonfield: Int32
    }

    pub struct Poly[a] {
        zero: Int32,
        one: a
    }

    pub struct PolyMulti[a, b] {
        polymulti_zero: Int32,
        polymulti_oneA: a,
        polymulti_oneB: b,
        polymulti_two: Map[a, b]
    }

    pub struct Recursive[r] {
        base: Int32,
        recurse: Recursive[r]
    }

    pub struct PolyRecursive[a, r] {
        poly_base: a,
        poly_recurse: PolyRecursive[a, r]
    }

    pub struct PolyRecursiveNonRegular[a, r] {
        poly_nonregular_simple: a,
        poly_nonregular_recurse: PolyRecursiveNonRegular[Poly[a], r]
    }

    pub struct BinaryTree[t, r] {
        l: Option[BinaryTree[t, r]],
        r: Option[BinaryTree[t, r]],
        value: t
    }
}

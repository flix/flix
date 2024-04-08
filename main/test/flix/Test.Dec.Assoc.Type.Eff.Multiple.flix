mod Test.Dec.Assoc.Type.Eff.Multiple {

    eff Observe {
        pub def capture(x: Int32): Unit
    }

    enum Maybe[a] {
        case Nothing,
        case Just(a)
    }

    enum Split[e: Eff, a: Type] {
        case Nothing,
        case Value(a),
        case Pending(a -> Unit \ e, a)
    }

    trait Transformer[a] {
        pub type E: Eff
        pub type B: Type
        pub def transform(x: a): Transformer.B[a] \ Transformer.E[a]
    }

    trait Composable[a] {
        pub type E: Eff
        pub type B: Type -> Type
        pub type C: Eff -> Type -> Type
        pub def compose(f: a -> Unit \ Composable.E[a], x: a): Composable.C[a][Composable.E[a], Composable.B[a][a]]
    }

    instance Transformer[Int32] {
        pub type E = Observe
        pub type B = Int64
        pub def transform(x: Int32): Int64 \ Observe =
            do Observe.capture(x); Int32.toInt64(x)
    }

    instance Composable[Int32] {
        pub type E = Observe
        pub type B = Maybe
        pub type C = Split

        pub def compose(f: a -> Unit \ Observe, x: Int32): Split[Observe, Maybe[Int32]] =
            Split.Pending(f, Maybe.Just(x))

    }
}

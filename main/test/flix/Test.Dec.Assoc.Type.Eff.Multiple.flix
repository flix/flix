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
        pub def reduce(x: Composable.C[a][Composable.E[a], Composable.B[a][a]]): Composable.B[a][a] \ Composable.E[a]
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

        pub def compose(f: Int32 -> Unit \ Observe, x: Int32): Split[Observe, Maybe[Int32]] =
            let g = y -> match y {
                case Maybe.Nothing  => ()
                case Maybe.Just(v)  => f(v)
            };
            Split.Pending(g, Maybe.Just(x))

        pub def reduce(x: Split[Observe, Maybe[Int32]]): Maybe[Int32] \ Observe = match x {
            case Split.Nothing              => Maybe.Nothing
            case Split.Value(Maybe.Just(v)) => Maybe.Just(v)
            case Split.Value(Maybe.Nothing) => Maybe.Nothing
            case Split.Pending(f, v)        => f(v); v
        }
    }
}

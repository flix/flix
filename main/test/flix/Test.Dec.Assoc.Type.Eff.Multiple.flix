mod Test.Dec.Assoc.Type.Eff.Multiple {

    ////////////////////////////////////////////////////////
    // Type Definitions                                   //
    ////////////////////////////////////////////////////////

    eff Observe {
        pub def captureInt32(x: Int32): Unit
        pub def captureString(x: String): Unit
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

    trait Transformable[a] {
        pub type E: Eff
        pub type B: Type
        pub def transform(x: a): Transformable.B[a] \ Transformable.E[a]
    }

    trait Composable[a] {
        pub type E: Eff
        pub type B: Type -> Type
        pub type C: Eff -> Type -> Type
        pub def compose(f: a -> Unit \ Composable.E[a], x: a): Composable.C[a][Composable.E[a], Composable.B[a][a]]
        pub def reduce(x: Composable.C[a][Composable.E[a], Composable.B[a][a]]): Composable.B[a][a] \ Composable.E[a]
    }

    instance Transformable[Int32] {
        pub type E = Observe
        pub type B = Int64
        pub def transform(x: Int32): Int64 \ Observe =
            do Observe.captureInt32(x); Int32.toInt64(x)
    }

    instance Transformable[String] {
        pub type E = Observe
        pub type B = Vector[Char]
        pub def transform(x: String): Vector[Char] \ Observe =
            do Observe.captureString(x); String.toVector(x)
    }

    instance Transformable[Vector[a]] with Transformable[a] {
        pub type E = Transformable.E[a]
        pub type B = Vector[Transformable.B[a]]
        pub def transform(x: Vector[a]): Vector[Transformable.B[a]] \ Transformable.E[a] =
            Vector.map(Transformable.transform, x)
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

    instance Composable[String] {
        pub type E = Observe
        pub type B = Maybe
        pub type C = Split

        pub def compose(f: String -> Unit \ Observe, x: String): Split[Observe, Maybe[String]] =
            let g = y -> match y {
                case Maybe.Nothing  => ()
                case Maybe.Just(v)  => f(v)
            };
            Split.Pending(g, Maybe.Just(x))

        pub def reduce(x: Split[Observe, Maybe[String]]): Maybe[String] \ Observe = match x {
            case Split.Nothing              => Maybe.Nothing
            case Split.Value(Maybe.Just(v)) => Maybe.Just(v)
            case Split.Value(Maybe.Nothing) => Maybe.Nothing
            case Split.Pending(f, v)        => f(v); v
        }
    }

    instance Composable[Vector[a]] with Composable[a] {
        pub type E = Composable.E[a]
        pub type B = Vector
        pub type C = Split

        pub def compose(f: Vector[a] -> Unit \ Composable.E[a], x: Vector[a]): Split[Composable.E[a], Vector[Vector[a]]] =
            Split.Pending(Vector.forEach(f), Vector#{x})

        pub def reduce(x: Split[Composable.E[a], Vector[Vector[a]]]): Vector[Vector[a]] \ Composable.E[a] = match x {
            case Split.Nothing       => Vector.empty()
            case Split.Value(v)      => v
            case Split.Pending(f, v) => f(v); v
        }
    }


    ////////////////////////////////////////////////////////
    // Polymorphic functions using Transformable          //
    ////////////////////////////////////////////////////////

    pub def transformAndCheckEq(eq: Transformable.B[a], x: a): Bool \ Transformable.E[a] with Transformable[a], Eq[a] where Transformable.B[a] ~ a =
        Transformable.transform(x) == eq

    pub def composeAndReduceEq(eq: Composable.B[a][a], f: a -> Unit \ Composable.E[a], x: a): Bool \ Composable.E[a] with Composable[a], Eq[a] where Composable.B[a][a] ~ a =
        Composable.reduce(Composable.compose(f, x)) == eq

}

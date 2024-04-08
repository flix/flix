mod Test.Dec.Assoc.Type.Eff.Multiple {

    eff _OutIn32 {
        pub def toStream(x: Int32): Unit
    }

    enum _Maybe[a] {
        case Nothing,
        case Just(a)
    }

    enum _Split[e: Eff, a: Type] {
        case Nothing,
        case Value(a),
        case Pending(a -> Unit \ e, a)
    }

    trait Transformer[a] {
        pub type E: Eff
        pub type B: Type
        pub def transform(x: a): Transformer.B[a] \ Transformer.E[a]
    }

    trait T1[a] {
        pub type E: Eff
        pub type B: Eff -> Type
        pub type C: Type -> Type
        pub def compose(x: a): T1.C[a][T1.B[a][T1.E[a]]]
    }

}

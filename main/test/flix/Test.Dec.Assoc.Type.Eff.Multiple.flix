mod Test.Dec.Assoc.Type.Eff.Multiple {

    trait Transformer[a] {
        pub type E: Eff
        pub type B: Type
        pub def transform(x: a): Transformer.B[a] \ Transformer.E[a]
    }

}

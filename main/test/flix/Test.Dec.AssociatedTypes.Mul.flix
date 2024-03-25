mod Test.Dec.AssociatedTypes.Mul {
    trait Mul[a] {
        pub type Right: Type
        pub type Out: Type
        pub def mul(x: a, y: Mul.Right[a]): Mul.Out[a]
    }
}

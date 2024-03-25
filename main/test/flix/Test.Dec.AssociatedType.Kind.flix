mod Test.Dec.AssociatedTypes.Kind {

    ////////////////////////////////////////////////////////
    // Type Definitions                                   //
    ////////////////////////////////////////////////////////

    trait Foo[t] {
        type K: Type -> Type
        pub def f(x: t): Foo.K[t][String]
    }

}

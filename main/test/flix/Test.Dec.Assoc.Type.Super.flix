mod Test.Dec.Assoc.Type.Super {
    trait C[a] {
        type T
    }

    trait D[a] with C[a] {
    }

    trait E[a] with D[a] {
        pub def foo(x: a): C.T[a]
    }
}

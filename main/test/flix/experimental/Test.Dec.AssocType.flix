mod Test/Dec/AssocType {

    class Coll[a] {
        type Elem[a]: Type

        pub def contains(x: Elem[a], c: a): Bool = ???
    }
}

mod Test/Dec/AssocType {

    class Coll[a] {
        type Elem[a]: Type

        pub def contains(x: Coll.Elem[a], c: a): Bool = ???
    }
}

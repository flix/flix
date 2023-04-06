mod Test.Dec.AssocType {

    class Coll[a] {
        type Elem[a]: Type

        pub def contains(x: Coll.Elem[a], c: a): Bool
    }

    instance Coll[String] {
        type Elem[String] = Char

        pub def contains(x: Char, s: String): Bool = ???
    }

    pub def hasA(s: String): Bool = Coll.contains('A', s)
}

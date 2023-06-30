mod Test.Assoc.Type {

    class Collection[t] {
        type Elm[t]: Type

        pub def isEmpty(x: t): Bool
        pub def contains(x: t, e: Collection.Elm[t]): Bool
        pub def head(x: t): Option[Collection.Elm[t]]
    }

    instance Collection[String] {
        type Elm[String] = Char

        pub def isEmpty(x: String): Bool = ???
        pub def contains(x: String, e: Char): Bool = ???
        pub def head(x: String): Option[Char] = if (String.isEmpty(x)) None else Some(String.charAt(0, x))
    }

}

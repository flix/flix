mod Test.Assoc.Type {

    class Collection[t] {
        type Elm[t]: Type

        pub def isEmpty(x: t): Bool
        pub def contains(x: t, e: Elm[t]): Bool
        pub def head(x: t): Option[Elm[t]]
    }

    instance Collection[String] {
        type Elm[String] = Char

        pub def isEmpty(x: String): Bool = ???
        pub def contains(x: String, e: Char): Bool = ???
        pub def head(x: String): Option[Char] = if (String.isEmpty(x)) None else Some(String.charAt(0, x))
    }

    class Collection2[t] {
        type Elm: Type

        pub def isEmpty(x: t): Bool
        pub def contains(x: t, e: Elm[t]): Bool
        pub def head(x: t): Option[Elm[t]]
    }

    instance Collection2[String] {
        type Elm = Char

        pub def isEmpty(x: String): Bool = ???
        pub def contains(x: String, e: Char): Bool = ???
        pub def head(x: String): Option[Char] = if (String.isEmpty(x)) None else Some(String.charAt(0, x))
    }

    class Collection3[t] {
        type Elm

        pub def isEmpty(x: t): Bool
        pub def contains(x: t, e: Elm[t]): Bool
        pub def head(x: t): Option[Elm[t]]
    }

    instance Collection3[String] {
        type Elm = Char

        pub def isEmpty(x: String): Bool = ???
        pub def contains(x: String, e: Char): Bool = ???
        pub def head(x: String): Option[Char] = if (String.isEmpty(x)) None else Some(String.charAt(0, x))
    }
}

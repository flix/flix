namespace List {

    enum List {
        case Nil
    }

    extern def null(xs: List[A]): Bool

    extern def map(f: A => B, xs: List[A]): List[B]

    extern def range(b: Int, e: Int): List[Int]

    extern def isPrefixOf(xs: List[A], ys: List[B]): Bool

}
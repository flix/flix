mod ShortestDistance {

    // Type aliases for readability
    type alias Source = Int32
    type alias Destination = Int32
    type alias Distance = Int32

    //////////////////////////////////////////
    // Tests                                //
    //////////////////////////////////////////

    ///
    /// Returns the following graph:
    ///
    /// ```
    ///      1---2
    ///     /|\  |\
    ///    0 | \ | 5
    ///     \|  \|/
    ///      3---4
    /// ```
    ///
    /// The edges are directed as follows (ordered from left to right, top to bottom):
    ///
    /// ```
    /// 0 -> 1, distance 2
    /// 0 -> 3, distance 3
    /// 1 -> 3, distance 10
    /// 1 -> 2, distance 8
    /// 1 -> 4, distance 4
    /// 3 -> 4, distance 5
    /// 4 -> 2, distance 3
    /// 2 -> 5, distance 1
    /// 4 -> 5, distance 3
    /// ```
    ///
    /// The shortest route is `0 -> 1 -> 4 -> 2 -> 5` with a total distance of `2 + 4 + 3 + 1 = 10`.
    ///
    pub def exampleGraph01(): Set[(Source, Destination, Distance)] =
        Set#{ (0, 1, 10), (0, 3, 10), (1, 3, 2), (1, 2, 4), (1, 4, 8), (3, 4, 9), (4, 2, 6), (2, 5, 10), (4, 5, 10) }

}

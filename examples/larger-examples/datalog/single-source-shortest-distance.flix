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
        Set#{ (0, 1, 2), (0, 3, 3), (1, 3, 10), (1, 2, 8), (1, 4, 4), (3, 4, 5), (4, 2, 3), (2, 5, 1), (4, 5, 3) }

    ///
    /// Returns the following graph:
    ///
    /// ```
    ///      1---2
    ///     / \   \
    ///    0   \   5
    ///     \   \ /
    ///      3---4
    /// ```
    ///
    /// The edges are directed as follows (ordered from left to right, top to bottom):
    ///
    /// ```
    /// 0 -> 1, distance 1
    /// 0 -> 3, distance 1
    /// 1 -> 2, distance 1
    /// 1 -> 4, distance 1
    /// 3 -> 4, distance 2
    /// 2 -> 5, distance 2
    /// 4 -> 5, distance 1
    /// ```
    ///
    /// The shortest route is `0 -> 1 -> 4 -> 5` with a total distance of `1 + 1 + 1 = 3`.
    ///
    pub def exampleGraph02(): Set[(Source, Destination, Distance)] =
        Set#{ (0, 1, 1), (0, 3, 1), (1, 2, 1), (1, 4, 1), (3, 4, 2), (2, 5, 2), (4, 5, 1) }

    ///
    /// Returns the following graph:
    ///
    /// ```
    ///      1
    ///     /|\
    ///    0 | 3
    ///     \|/
    ///      2
    /// ```
    ///
    /// The edges are directed as follows (ordered from left to right, top to bottom):
    ///
    /// ```
    /// 0 -> 1, distance 10
    /// 0 -> 2, distance 15
    /// 1 -> 2, distance 1
    /// 1 -> 3, distance 15
    /// 2 -> 3, distance 10
    /// ```
    ///
    /// The shortest route is `0 -> 1 -> 2 -> 3` with a total distance of `10 + 1 + 10 = 21`.
    ///
    pub def exampleGraph03(): Set[(Source, Destination, Distance)] =
        Set#{ (0, 1, 10), (0, 2, 15), (1, 2, 1), (1, 3, 15), (2, 3, 10) }

    ///
    /// Returns the following graph:
    ///
    /// ```
    ///       1--2--3
    ///      /|  |\  \
    ///     / |  | \  \
    ///    0--4--5--6--10
    ///     \   /  /| /
    ///      \ /  / |/
    ///       7--8--9
    /// ```
    ///
    /// The edges are directed as follows (ordered from left to right, top to bottom):
    ///
    /// ```
    /// 0 -> 1 , distance 10
    /// 0 -> 4 , distance 10
    /// 0 -> 7 , distance 10
    /// 4 -> 1 , distance 15
    /// 1 -> 2 , distance 20
    /// 4 -> 5 , distance 20
    /// 5 -> 7 , distance 5
    /// 7 -> 8 , distance 5
    /// 2 -> 5 , distance 10
    /// 2 -> 3 , distance 5
    /// 6 -> 2 , distance 5
    /// 5 -> 6 , distance 30
    /// 8 -> 6 , distance 5
    /// 8 -> 9 , distance 10
    /// 3 -> 10, distance 5
    /// 6 -> 10, distance 20
    /// 9 -> 10, distance 30
    /// ```
    ///
    /// The shortest route is `0 -> 7 -> 8 -> 6 -> 2 -> 3 -> 10 with a total distance of `10 + 5 + 5 + 5 + 5 + 5  = 35`.
    ///
    pub def exampleGraph04(): Set[(Source, Destination, Distance)] =
        Set#{   (0, 1, 10), (0, 4, 10), (0, 7, 10), (4, 1, 15), (1, 2, 20),
                (4, 5, 20), (5, 7, 5), (7, 8, 5), (2, 5, 10), (2, 3, 5),
                (6, 2, 5), (5, 6, 30), (8, 6, 5), (8, 9, 10), (3, 10, 5),
                (6, 10, 20), (9, 10, 30)
            }

}

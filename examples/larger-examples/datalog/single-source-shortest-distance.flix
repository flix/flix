mod ShortestDistance {

    use Down.Down;

    ///
    /// Returns the shortest distance from `src` to every other reachable vertex in the
    /// weighted directed graph `g`.
    ///
    pub def sssd(src: t, g: m[(t, Int32, t)]): Map[t, Int32] \ Foldable.Aef[m] with Foldable[m], Order[t] = {
        let edges = inject g into Edge;
        let dists = #{
            Dist(src; Down(0)).
            Dist(y; d + Down(w)) :- Dist(x; d), Edge(x, w, y).
        };
        let res = query edges, dists select (x, coerce(d)) from Dist(x; d);
        res |> Vector.toMap
    }

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
    /// 4 -> 5, distance 10
    /// ```
    ///
    /// The shortest route from 0 to 5 is `0 -> 1 -> 4 -> 2 -> 5` with a total distance of `2 + 4 + 3 + 1 = 10`.
    ///
    pub def exampleGraph01(): Set[(Int32, Int32, Int32)] =
        Set#{ (0, 2, 1), (0, 3, 3), (1, 4, 4), (1, 8, 2), (1, 10, 3), (2, 1, 5), (3, 5, 4), (4, 3, 2), (4, 10, 5) }

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
    /// The shortest route from 0 to 5 is `0 -> 1 -> 4 -> 5` with a total distance of `1 + 1 + 1 = 3`.
    ///
    pub def exampleGraph02(): Set[(Int32, Int32, Int32)] =
        Set#{ (0, 1, 1), (0, 1, 3), (1, 1, 2), (1, 1, 4), (2, 2, 5), (3, 2, 4), (4, 1, 5) }

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
    /// The shortest route from 0 to 3 is `0 -> 1 -> 2 -> 3` with a total distance of `10 + 1 + 10 = 21`.
    ///
    pub def exampleGraph03(): Set[(Int32, Int32, Int32)] =
        Set#{ (0, 10, 1), (0, 15, 2), (1, 1, 2), (1, 15, 3), (2, 10, 3) }

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
    /// 6 -> 9 , distance 2
    /// 3 -> 10, distance 5
    /// 6 -> 10, distance 20
    /// 9 -> 10, distance 35
    /// ```
    ///
    /// The shortest route from 0 to 10 is `0 -> 7 -> 8 -> 6 -> 2 -> 3 -> 10 with a total distance of `10 + 5 + 5 + 5 + 5 + 5  = 35`.
    ///
    pub def exampleGraph04(): Set[(Int32, Int32, Int32)] =
        Set#{  (0, 10, 1), (0, 10, 4 ), (0, 10, 7 ), (1, 20, 2), (2, 5, 3),
               (2, 10, 5), (3, 5 , 10), (4, 15, 1 ), (4, 20, 5), (5, 5, 7),
               (5, 30, 6), (6, 2 , 9 ), (6, 5 , 2 ), (6, 20, 10), (7, 5 , 8),
               (8, 5 , 6), (8, 10, 9 ), (9, 30, 10)
            }

    ///
    /// Returns the following graph:
    ///
    /// ```
    ///      1---2
    ///     / \   \
    ///    0   \   6
    ///     \   \ /
    ///      3---5
    ///      |
    ///      4
    /// ```
    ///
    /// The edges are directed as follows (ordered from left to right, top to bottom):
    ///
    /// ```
    /// 0 -> 1, distance 1
    /// 0 -> 3, distance 1
    /// 1 -> 2, distance 1
    /// 1 -> 5, distance 1
    /// 3 -> 5, distance 2
    /// 3 -> 4, distance 6
    /// 2 -> 6, distance 2
    /// 5 -> 6, distance 1
    /// ```
    ///
    /// The shortest route from 0 to 6 is `0 -> 1 -> 5 -> 6` with a total distance of `1 + 1 + 1 = 3`.
    ///
    pub def exampleGraph05(): Set[(Int32, Int32, Int32)] =
        Set#{ (0, 1, 1), (0, 1, 3), (1, 1, 2), (1, 1, 5), (2, 2, 6), (3, 2, 5), (3, 6, 4), (5, 1, 6) }

    @Test
    def testExampleGraphShortestDistances01(): Bool =
        Assert.eq(Map#{0 => 0, 1 => 2, 2 => 9, 3 => 3, 4 => 6, 5 => 10}, sssd(0, exampleGraph01()))

    @Test
    def testExampleGraphShortestDistances02(): Bool =
        Assert.eq(Map#{0 => 0, 1 => 1, 2 => 2, 3 => 1, 4 => 2, 5 => 3}, sssd(0, exampleGraph02()))

    @Test
    def testExampleGraphShortestDistances03(): Bool =
        Assert.eq(Map#{0 => 0, 1 => 10, 2 => 11, 3 => 21}, sssd(0, exampleGraph03()))

    @Test
    def testExampleGraphShortestDistances04(): Bool =
        Assert.eq(Map#{0 => 0, 1 => 10, 2 => 25, 3 => 30, 4 => 10, 5 => 30, 6 => 20, 7 => 10, 8 => 15, 9 => 22, 10 => 35}, sssd(0, exampleGraph04()))

    @Test
    def testExampleGraphShortestDistances05(): Bool =
        Assert.eq(Map#{0 => 0, 1 => 1, 2 => 2, 3 => 1, 4 => 7, 5 => 2, 6 => 3}, sssd(0, exampleGraph05()))

}

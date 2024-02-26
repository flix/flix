mod ShortestPathN {

    use Path.{Path, Bot};

    ///
    /// Returns the shortest paths from `src` to all of reachable vertices
    /// in the graph `g`.
    ///
    pub def ssspn(src: t, g: m[(t, Int32, t)]): Map[t, Vector[t]] with Foldable[m], Order[t] = {
        let edges = inject g into Edge;
        let rules = #{
            Reach(x, y; init(y, l, x)) :- Edge(x, l, y).
            Reach(x, z; cons(z, l, p)) :- Reach(x, y; p), Edge(y, l, z).
        };
        let res = query edges, rules select (t, p) from Reach(src, t; p);
        res |> Functor.map(match (t, p) -> (t, Foldable.toVector(p) |> Vector.reverse))
            |> Foldable.toMap
    }

    pub enum Path[a] with ToString {
        case Path(List[a], Int32)
        case Bot // Infinitely long path
    }

    instance Eq[Path[a]] {
        pub def eq(x: Path[a], y: Path[a]): Bool = match (x, y) {
            case (Bot, Bot)                 => true
            case (Path(_, l1), Path(_, l2)) => l1 == l2
            case _                          => false
        }
    }

    instance Order[Path[a]] {
        pub def compare(x: Path[a], y: Path[a]): Comparison = match (x, y) {
            case (Bot, Bot)                 => Comparison.EqualTo
            case (Bot, _)                   => Comparison.LessThan
            case (_, Bot)                   => Comparison.GreaterThan
            case (Path(_, l1), Path(_, l2)) => l1 <=> l2
        }
    }

    instance LowerBound[Path[a]] {
        // The longest list
        pub def minValue(): Path[a] = Bot
    }

    instance PartialOrder[Path[a]] {
        pub def lessEqual(x: Path[a], y: Path[a]): Bool = match (x, y) {
            case (Bot, _)                   => true
            case (Path(_, l1), Path(_, l2)) => l1 >= l2
            case _                          => false
        }
    }

    instance JoinLattice[Path[a]] {
        pub def leastUpperBound(x: Path[a], y: Path[a]): Path[a] = match (x, y) {
            case (Bot, p)                   => p
            case (p, Bot)                   => p
            case (Path(_, l1), Path(_, l2)) => if (l1 <= l2) x else y
        }
    }

    instance MeetLattice[Path[a]] {
        pub def greatestLowerBound(x: Path[a], y: Path[a]): Path[a] = match (x, y) {
            case (Bot, _)                   => Bot
            case (_, Bot)                   => Bot
            case (Path(_, l1), Path(_, l2)) => if (l1 > l2) x else y
        }
    }

    instance Foldable[Path] {
        pub def foldLeft(f: b -> (a -> b \ ef), s: b, t: Path[a]): b \ ef = match t {
            case Bot     => s
            case Path(p, _) => Foldable.foldLeft(f, s, p)
        }

        pub def foldRight(f: a -> (b -> b \ ef), s: b, t: Path[a]): b \ ef = match t {
            case Bot     => s
            case Path(p, _) => Foldable.foldRight(f, s, p)
        }

        pub def foldRightWithCont(f: a -> ((Unit -> b \ ef) -> b \ ef), s: b, t: Path[a]): b \ ef = match t {
            case Bot     => s
            case Path(p, _) => Foldable.foldRightWithCont(f, s, p)
        }
    }

    ///
    /// Returns a `Path` from `x` to `y` of length `l`.
    ///
    pub def init(y: a,  l: Int32, x: a): Path[a] =
        Path(y :: x :: Nil, l)

    ///
    /// Extends the `Path` `p` with `z` of length `l`.
    ///
    pub def cons(z: a, l: Int32, p: Path[a]): Path[a] = match (p) {
        case Bot          => Bot
        case Path(xs, l1) => Path(z :: xs, l1 + l)
    }

    ///
    /// Returns the index of `a` in the `Path` `p`.
    /// Note that a `Path` is sorted in descending order.
    ///
    pub def indexOf(x: a, p: Path[a]): Option[Int32] with Eq[a] = match p {
        case Bot         => None
        case Path(xs, _) => List.indexOf(x, xs)
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
    /// 0 -> 1
    /// 0 -> 3
    /// 1 -> 3
    /// 1 -> 2
    /// 1 -> 4
    /// 3 -> 4
    /// 4 -> 2
    /// 2 -> 5
    /// 4 -> 5
    /// ```
    ///
    /// The shortest path from 0 to 5 is `0 -> 3 -> 4 -> 5`.
    ///
    pub def exampleGraph01(): Set[(Int32, Int32, Int32)] =
        Set#{ (0, 1, 1), (0, 1, 3), (1, 1, 4), (1, 1, 2), (1, 1, 3), (2, 1, 5), (3, 1, 4), (4, 1, 2), (4, 1, 5) }

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
    /// 0 -> 1
    /// 0 -> 3
    /// 1 -> 2
    /// 1 -> 4
    /// 3 -> 4
    /// 2 -> 5
    /// 4 -> 5
    /// ```
    ///
    /// The shortest path from 0 to 5 is `0 -> 3 -> 4 -> 5`.
    ///
    pub def exampleGraph02(): Set[(Int32, Int32, Int32)] =
        Set#{ (0, 1, 1), (0, 1, 3), (1, 1, 2), (1, 1, 4), (2, 1, 5), (3, 1, 4), (4, 1, 5) }

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
    /// 0 -> 1
    /// 0 -> 2
    /// 1 -> 2
    /// 1 -> 3
    /// 2 -> 3
    /// ```
    ///
    /// The shortest path from 0 to 3 is `0 -> 2 -> 3`.
    ///
    pub def exampleGraph03(): Set[(Int32, Int32, Int32)] =
        Set#{ (0, 1, 1), (0, 1, 2), (1, 1, 2), (1, 1, 3), (2, 1, 3) }

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
    /// 0 -> 1
    /// 0 -> 4
    /// 0 -> 7
    /// 4 -> 1
    /// 1 -> 2
    /// 4 -> 5
    /// 5 -> 7
    /// 7 -> 8
    /// 2 -> 5
    /// 2 -> 3
    /// 6 -> 2
    /// 5 -> 6
    /// 8 -> 6
    /// 8 -> 9
    /// 6 -> 9
    /// 3 -> 10
    /// 6 -> 10
    /// 9 -> 10
    /// ```
    ///
    /// The shortest path from 0 to 10 is `0 -> 7 -> 8 -> 9 -> 10`.
    ///
    pub def exampleGraph04(): Set[(Int32, Int32, Int32)] =
        Set#{  (0, 1, 1), (0, 1, 4 ), (0, 1, 7 ), (1, 1, 2 ), (2, 1, 3),
               (2, 1, 5), (3, 1, 10), (4, 1, 1 ), (4, 1, 5 ), (5, 1, 7),
               (5, 1, 6), (6, 1, 9 ), (6, 1, 2 ), (6, 1, 10), (7, 1, 8),
               (8, 1, 6), (8, 1, 9 ), (9, 1, 10)
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
    /// 0 -> 1
    /// 0 -> 3
    /// 1 -> 2
    /// 1 -> 5
    /// 3 -> 5
    /// 3 -> 4
    /// 2 -> 6
    /// 5 -> 6
    /// ```
    ///
    /// The shortest path from 0 to 6 is `0 -> 3 -> 5 -> 6`.
    ///
    pub def exampleGraph05(): Set[(Int32, Int32, Int32)] =
        Set#{ (0, 1, 1), (0, 1, 3), (1, 1, 2), (1, 1, 5), (2, 1, 6), (3, 1, 5), (3, 1, 4), (5, 1, 6) }

    @Test
    def testExampleGraphShortestPaths01(): Bool =
        Assert.eq(Map#{1 => Vector#{0, 1}, 2 => Vector#{0, 1, 2}, 3 => Vector#{0, 3}, 4 => Vector#{0, 3, 4}, 5 => Vector#{0, 3, 4, 5}}, ssspn(0, exampleGraph01()))

    @Test
    def testExampleGraphShortestPaths02(): Bool =
        Assert.eq(Map#{1 => Vector#{0, 1}, 2 => Vector#{0, 1, 2}, 3 => Vector#{0, 3}, 4 => Vector#{0, 3, 4}, 5 => Vector#{0, 3, 4, 5}}, ssspn(0, exampleGraph02()))

    @Test
    def testExampleGraphShortestPaths03(): Bool =
        Assert.eq(Map#{1 => Vector#{0, 1}, 2 => Vector#{0, 2}, 3 => Vector#{0, 2, 3}}, ssspn(0, exampleGraph03()))

    @Test
    def testExampleGraphShortestPaths04(): Bool =
        Assert.eq(Map#{1 => Vector#{0 ,1}, 2 => Vector#{0, 1, 2}, 3 => Vector#{0, 1, 2, 3}, 4 => Vector#{0, 4},
            5 => Vector#{0, 4, 5}, 6 => Vector#{0, 7, 8, 6}, 7 => Vector#{0, 7}, 8 => Vector#{0, 7, 8},
            9 => Vector#{0, 7, 8, 9}, 10 => Vector#{0, 7, 8, 9, 10}}, ssspn(0, exampleGraph04()))

    @Test
    def testExampleGraphShortestPaths05(): Bool =
        Assert.eq(Map#{1 => Vector#{0, 1}, 2 => Vector#{0, 1, 2}, 3 => Vector#{0, 3}, 4 => Vector#{0, 3, 4}, 5 => Vector#{0, 3, 5}, 6 => Vector#{0, 3, 5, 6}}, ssspn(0, exampleGraph05()))

}

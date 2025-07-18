mod ShortestPath {

    use Path.{Path, Bot};

    ///
    /// Returns the shortest paths from `src` to all of reachable vertices
    /// in the graph `g`.
    ///
    pub def sssp(src: t, g: m[(t, t)]): Map[t, Vector[t]] \ Foldable.Aef[m] with Foldable[m], Order[t] = {
        let edges = inject g into Edge/2;
        let rules = #{
            Reach(x, y; init(y, x)) :- Edge(x, y).
            Reach(x, z; cons(z, p)) :- Reach(x, y; p), Edge(y, z).
        };
        let res = query edges, rules select (t, p) from Reach(src, t; p);
        res |> Functor.map(match (t, p) -> (t, Foldable.toVector(p) |> Vector.reverse))
            |> Foldable.toMap
    }

    pub enum Path[a] with ToString {
        case Path(List[a])
        case Bot // Infinitely long path
    }

    instance Eq[Path[a]] {
        pub def eq(x: Path[a], y: Path[a]): Bool = match (x, y) {
            case (Bot, Bot)           => true
            case (Path(xs), Path(ys)) => List.length(xs) == List.length(ys)
            case _                    => false
        }
    }

    instance Order[Path[a]] with Order[a] {
        pub def compare(x: Path[a], y: Path[a]): Comparison = match (x, y) {
            case (Bot, Bot)                 => Comparison.EqualTo
            case (Bot, _)                   => Comparison.LessThan
            case (_, Bot)                   => Comparison.GreaterThan
            case (Path(list1), Path(list2)) => list1 <=> list2
        }
    }

    instance LowerBound[Path[a]] {
        // The longest list
        pub def minValue(): Path[a] = Bot
    }

    instance PartialOrder[Path[a]] {
        pub def lessEqual(x: Path[a], y: Path[a]): Bool = match (x, y) {
            case (Bot, _)             => true
            case (Path(xs), Path(ys)) => List.length(xs) >= List.length(ys)
            case _                    => false
        }
    }

    instance JoinLattice[Path[a]] {
        pub def leastUpperBound(x: Path[a], y: Path[a]): Path[a] = match (x, y) {
            case (Bot, p)             => p
            case (p, Bot)             => p
            case (Path(xs), Path(ys)) => if (List.length(xs) <= List.length(ys)) x else y
        }
    }

    instance MeetLattice[Path[a]] {
        pub def greatestLowerBound(x: Path[a], y: Path[a]): Path[a] = match (x, y) {
            case (Bot, _)             => Bot
            case (_, Bot)             => Bot
            case (Path(xs), Path(ys)) => if (List.length(xs) > List.length(ys)) x else y
        }
    }

    instance Foldable[Path] {
        pub def foldLeft(f: b -> (a -> b \ ef), s: b, t: Path[a]): b \ ef = match t {
            case Bot     => s
            case Path(p) => Foldable.foldLeft(f, s, p)
        }

        pub def foldRight(f: a -> (b -> b \ ef), s: b, t: Path[a]): b \ ef = match t {
            case Bot     => s
            case Path(p) => Foldable.foldRight(f, s, p)
        }

        pub def foldRightWithCont(f: a -> ((Unit -> b \ ef) -> b \ ef), s: b, t: Path[a]): b \ ef = match t {
            case Bot     => s
            case Path(p) => Foldable.foldRightWithCont(f, s, p)
        }
    }

    ///
    /// Returns a `Path` from `x` to `y`.
    ///
    pub def init(y: a, x: a): Path[a] =
        Path(y :: x :: Nil)

    ///
    /// Extends the `Path` `p` with `z`.
    ///
    pub def cons(z: a, p: Path[a]): Path[a] = match p {
        case Bot      => Bot
        case Path(xs) => Path(z :: xs)
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
    pub def exampleGraph01(): Set[(Int32, Int32)] =
        Set#{ (0, 1), (0, 3), (1, 4), (1, 2), (1, 3), (2, 5), (3, 4), (4, 2), (4, 5) }

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
    pub def exampleGraph02(): Set[(Int32, Int32)] =
        Set#{ (0, 1), (0, 3), (1, 2), (1, 4), (2, 5), (3, 4), (4, 5) }

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
    pub def exampleGraph03(): Set[(Int32, Int32)] =
        Set#{ (0, 1), (0, 2), (1, 2), (1, 3), (2, 3) }

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
    pub def exampleGraph04(): Set[(Int32, Int32)] =
        Set#{  (0, 1), (0, 4 ), (0, 7 ), (1, 2 ), (2, 3),
               (2, 5), (3, 10), (4, 1 ), (4, 5 ), (5, 7),
               (5, 6), (6, 9 ), (6, 2 ), (6, 10), (7, 8),
               (8, 6), (8, 9 ), (9, 10)
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
    pub def exampleGraph05(): Set[(Int32, Int32)] =
        Set#{ (0, 1), (0, 3), (1, 2), (1, 5), (2, 6), (3, 5), (3, 4), (5, 6) }

    @Test
    def testExampleGraphShortestPaths01(): Bool =
        sssp(0, exampleGraph01()) |> 
        Map.forAll(k -> v -> match k {
            case 1 => Assert.eq(Vector#{0, 1}, v)
            case 2 => Assert.eq(Vector#{0, 1, 2}, v)
            case 3 => Assert.eq(Vector#{0, 3}, v)
            case 4 =>
                let shortestPathFound = v == Vector#{0, 3, 4} or v == Vector#{0, 1, 4};
                Assert.eq(true, shortestPathFound)
            case 5 =>
                let shortestPathFound = v == Vector#{0, 1, 2, 5} or v == Vector#{0, 3, 4, 5} or v == Vector#{0, 1, 4, 5};
                Assert.eq(true, shortestPathFound)
            case _ => unreachable!()
        })

    @Test
    def testExampleGraphShortestPaths02(): Bool =
        sssp(0, exampleGraph02()) |>
        Map.forAll(k -> v -> match k {
            case 1 => Assert.eq(Vector#{0, 1}, v)
            case 2 => Assert.eq(Vector#{0, 1, 2}, v)
            case 3 => Assert.eq(Vector#{0, 3}, v)
            case 4 =>
                let shortestPathFound = v == Vector#{0, 1, 4} or v == Vector#{0, 3, 4};
                Assert.eq(true, shortestPathFound)
            case 5 =>
                let shortestPathFound = v == Vector#{0, 1, 4, 5} or v == Vector#{0, 1, 2, 5} or v == Vector#{0, 3, 4, 5};
                Assert.eq(true, shortestPathFound)
            case _ => unreachable!()
        })

    @Test
    def testExampleGraphShortestPaths03(): Bool =
        sssp(0, exampleGraph03()) |>
            Map.forAll(k -> v -> match k {
                case 1 => Assert.eq(Vector#{0, 1}, v)
                case 2 => Assert.eq(Vector#{0, 2}, v)
                case 3 =>
                    let shortestPathFound = v == Vector#{0, 2, 3} or v == Vector#{0, 1, 3};
                    Assert.eq(true, shortestPathFound)
                case _ => unreachable!()                
            })

    @Test
    def testExampleGraphShortestPaths04(): Bool =
        sssp(0, exampleGraph04()) |>
        Map.forAll(k -> v -> match k {
            case 1 => Assert.eq(Vector#{0, 1}, v)
            case 2 => Assert.eq(Vector#{0, 1, 2}, v)
            case 3 => Assert.eq(Vector#{0, 1, 2, 3}, v)
            case 4 => Assert.eq(Vector#{0, 4}, v)
            case 5 => Assert.eq(Vector#{0, 4, 5}, v)
            case 6 =>
                let shortestPathFound = v == 
                Vector#{0, 4, 5, 6} or
                v == Vector#{0, 1, 2, 6} or
                v == Vector#{0, 7, 8, 6} or
                v == Vector#{0, 7, 5, 6};
                Assert.eq(true, shortestPathFound)
            case 7 => Assert.eq(Vector#{0, 7}, v)
            case 8 => Assert.eq(Vector#{0, 7, 8}, v)
            case 9 => Assert.eq(Vector#{0, 7, 8, 9}, v)
            case 10 =>
                let shortestPathFound = 
                    v == Vector#{0, 1, 2, 3, 10} or
                    v == Vector#{0, 1, 2, 6, 10} or
                    v == Vector#{0, 4, 5, 6, 10} or
                    v == Vector#{0, 7, 5, 6, 10} or
                    v == Vector#{0, 7, 8, 6, 10} or
                    v == Vector#{0, 7, 8, 9, 10};
                Assert.eq(true, shortestPathFound)
            case _ => unreachable!()
        })

    @Test
    def testExampleGraphShortestPaths05(): Bool =
        sssp(0, exampleGraph05()) |>
        Map.forAll(k -> v -> match k {
            case 1 => Assert.eq(Vector#{0, 1}, v)
            case 2 => Assert.eq(Vector#{0, 1, 2}, v)
            case 3 => Assert.eq(Vector#{0, 3}, v)
            case 4 => Assert.eq(Vector#{0, 3, 4}, v)
            case 5 =>
                let shortestPathFound = v == Vector#{0, 1, 5} or v == Vector#{0, 3, 5};
                Assert.eq(true, shortestPathFound)
            case 6 =>
                let shortestPathFound = v == Vector#{0, 1, 2, 5} or v == Vector#{0, 1, 5, 6} or v == Vector#{0, 3, 5, 6};
                Assert.eq(true, shortestPathFound)
            case _ => unreachable!()
        })
}

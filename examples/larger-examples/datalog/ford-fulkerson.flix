///
/// The Ford-Fulkerson algorithm finds the maximum flow of a flow network.
/// Here it is implemented using a combination of functional programming
/// and datalog.
///
pub def main(): Unit \ IO =
    let g1 = FordFulkerson.exampleGraph01() |> FordFulkerson.maxFlow(0, 5);
    let g2 = FordFulkerson.exampleGraph02() |> FordFulkerson.maxFlow(0, 5);
    let g3 = FordFulkerson.exampleGraph03() |> FordFulkerson.maxFlow(0, 3);
    let g4 = FordFulkerson.exampleGraph04() |> FordFulkerson.maxFlow(0, 10);
    println("Expected maximum flow 19, got ${g1}");
    println("Expected maximum flow 20, got ${g2}");
    println("Expected maximum flow 2000, got ${g3}");
    println("Expected maximum flow 20, got ${g4}")

mod FordFulkerson {

    use Path.{Path, Bot};

    ///
    /// Returns the maximum flow from `src` to `dst` in the flow network `g`.
    /// N.B.: `g` is a directed graph with upper bounds / capacity on the edges.
    /// No pre-assigned flow is allowed.
    ///
    /// The following assumptions also apply:
    /// - `src` and `dst` is connected in `g`
    /// - `g` contains no negative cycles
    /// - `g` is labeled in ascending order from `src` to `sink`
    /// - The label of `src` has the lowest value in the graph
    /// - The label of `dst` has the highest value in the graph
    ///
    pub def maxFlow(src: t, dst: t, g: m[(t, Int32, t)]): Int32 \ Foldable.Aef[m] with Foldable[m], Order[t] =
        def fordFulkerson(flowNetwork) = match augmentingPath(src, dst, flowNetwork) {
            case None       => getMaxFlow(dst, flowNetwork)
            case Some(path) =>
                let incr = minCapacity(path, flowNetwork);
                let updatedNetwork = increaseFlow(path, incr, flowNetwork);
                fordFulkerson(updatedNetwork)
        };
        // Init with 0 flow
        fordFulkerson(zeroFlow(g))

    ///
    /// Returns a flow network with zero flow.
    ///
    def zeroFlow(g: m[(t, Int32, t)]): Vector[(t, Int32, Int32, t)] \ Foldable.Aef[m] with Foldable[m], Order[t] =
        Foldable.toVector(g) |> Vector.map(match (x, y, z) -> (x, y, 0, z))

    ///
    /// Returns the sum of the flows on all directly ingoing edges to `dst`.
    ///
    def getMaxFlow(dst: t, g: m[(t, Int32, Int32, t)]): Int32 \ Foldable.Aef[m] with Foldable[m], Order[t] =
        g
        |> Foldable.toVector
        |> Vector.filterMap(match (_, _, f, d) -> if (d == dst) Some(f) else None)
        |> Vector.sum

    ///
    /// Returns an augmenting path if one exists.
    ///
    /// An edge is in an augmenting path if its flow can be increased, i.e., the flow is strictly less than the capacity,
    /// or if it has non-zero flow.
    ///
    def augmentingPath(src: t, dst: t, g: m[(t, Int32, Int32, t)]): Option[Path[t]] \ Foldable.Aef[m] with Foldable[m], Order[t] =
        let edges = inject g into Edge;
        let rules = #{
            Reach(x, y; init(y, x)) :- Edge(x, u, f, y),                 if (u - f) > 0. // Forward edge
            Reach(x, z; cons(z, p)) :- Reach(x, y; p), Edge(y, u, f, z), if (u - f) > 0. // Forward edge
            Reach(x, y; init(y, x)) :- Edge(y, u, f, x),                 if f > 0.       // Back edge
            Reach(x, z; cons(z, p)) :- Reach(x, y; p), Edge(z, u, f, y), if f > 0.       // Back edge
        };
        let result = query edges, rules select fn from Reach(src, dst; fn);
        Vector.head(result)

    ///
    /// Returns the most constraining capacity of `g` on the `Path` `p`.
    ///
    def minCapacity(p: Path[t], g: m[(t, Int32, Int32, t)]): Int32 \ Foldable.Aef[m] with Foldable[m], Order[t] =
        let onPath = (s, d) -> isForwardEdge(s, d, p) or isBackEdge(s, d, p);
        let optMin = g |> Foldable.filter(match (s, _, _, d) -> onPath(s, d))
            |> List.map(match (_, u, f, _) -> u - f)
            |> List.minimum;
        match optMin {
            case Some(u) => u
            case None    => unreachable!() // This function is only called by `maxFlow` if an augmenting path was found
        }

    ///
    /// Returns a new flow network where the edges in `g` on the `Path` `p` has been adjusted by `incr`.
    ///
    def increaseFlow(p: Path[t], incr: Int32, g: m[(t, Int32, Int32, t)]): Vector[(t, Int32, Int32, t)] \ Foldable.Aef[m] with Foldable[m], Order[t] =
        g
        |> Foldable.toVector
        |> Vector.map(match (s, u, f, d) ->
            if (isForwardEdge(s, d, p))
                (s, u, f + incr, d)
            else if (isBackEdge(s, d, p))
                (s, u, f - incr, d)
            else
                (s, u, f, d)
        )

    ///
    /// Returns true if `src` is an edge pointing to `dst` on the `Path` `p`.
    ///
    def isForwardEdge(src: t, dst: t, p: Path[t]): Bool with Eq[t] =
        match (indexOf(src, p), indexOf(dst, p)) { // A path is sorted in reverse order
            case (Some(si), Some(di)) if di + 1 == si => true
            case _ => false
        }

    ///
    /// Returns true if `dst` is an edge pointing to `src` on the `Path` `p`.
    ///
    def isBackEdge(src: t, dst: t, p: Path[t]): Bool with Eq[t] =
        match (indexOf(src, p), indexOf(dst, p)) { // A path is sorted in reverse order
            case (Some(si), Some(di)) if si + 1 == di => true
            case _ => false
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

    instance Order[Path[a]] {
        pub def compare(x: Path[a], y: Path[a]): Comparison = match (x, y) {
            case (Bot, Bot)           => Comparison.EqualTo
            case (Bot, _)             => Comparison.LessThan
            case (_, Bot)             => Comparison.GreaterThan
            case (Path(xs), Path(ys)) => List.length(xs) <=> List.length(ys)
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

    ///
    /// Returns the index of `a` in the `Path` `p`.
    /// Note that a `Path` is sorted in descending order.
    ///
    pub def indexOf(x: a, p: Path[a]): Option[Int32] with Eq[a] = match p {
        case Bot      => None
        case Path(xs) => List.indexOf(x, xs)
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
    /// 0 -> 1, capacity 10
    /// 0 -> 3, capacity 10
    /// 1 -> 3, capacity 2
    /// 1 -> 2, capacity 4
    /// 1 -> 4, capacity 8
    /// 3 -> 4, capacity 9
    /// 4 -> 2, capacity 6
    /// 2 -> 5, capacity 10
    /// 4 -> 5, capacity 10
    /// ```
    ///
    /// The maximum flow is `19`.
    ///
    pub def exampleGraph01(): Set[(Int32, Int32, Int32)] =
        Set#{ (0, 10, 1), (0, 10, 3), (1, 2, 3), (1, 4, 2), (1, 8, 4), (2, 10, 5), (3, 9, 4), (4, 6, 2), (4, 10, 5) }

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
    /// 0 -> 1, capacity 10
    /// 0 -> 3, capacity 10
    /// 1 -> 2, capacity 25
    /// 4 -> 1, capacity 6
    /// 3 -> 4, capacity 15
    /// 2 -> 5, capacity 10
    /// 4 -> 5, capacity 10
    /// ```
    ///
    /// The maximum flow is `20`.
    ///
    pub def exampleGraph02(): Set[(Int32, Int32, Int32)] =
        Set#{ (0, 10, 1), (0, 10, 3), (1, 25, 2), (2, 10, 5), (3, 15, 4), (4, 6, 1), (4, 10, 5) }

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
    /// 0 -> 1, capacity 1000
    /// 0 -> 2, capacity 1000
    /// 1 -> 2, capacity 1
    /// 1 -> 3, capacity 1000
    /// 2 -> 3, capacity 1000
    /// ```
    ///
    /// The maximum flow is `2000`.
    ///
    pub def exampleGraph03(): Set[(Int32, Int32, Int32)] =
        Set#{ (0, 1000, 1), (0, 1000, 2), (1, 1, 2), (1, 1000, 3), (2, 1000, 3) }

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
    /// 0 -> 1 , capacity 5
    /// 0 -> 4 , capacity 10
    /// 0 -> 7 , capacity 5
    /// 4 -> 1 , capacity 15
    /// 1 -> 2 , capacity 10
    /// 4 -> 5 , capacity 20
    /// 5 -> 7 , capacity 5
    /// 7 -> 8 , capacity 10
    /// 2 -> 5 , capacity 25
    /// 2 -> 3 , capacity 10
    /// 6 -> 2 , capacity 15
    /// 5 -> 6 , capacity 30
    /// 8 -> 6 , capacity 5
    /// 8 -> 9 , capacity 10
    /// 6 -> 9 , capacity 5
    /// 3 -> 10, capacity 5
    /// 6 -> 10, capacity 15
    /// 9 -> 10, capacity 10
    /// ```
    ///
    /// The maximum flow is `20`.
    ///
    pub def exampleGraph04(): Set[(Int32, Int32, Int32)] =
        Set#{ (0, 5 , 1), (0, 5 , 7 ), (0, 10, 4 ), (1, 10, 2 ), (2, 10, 3),
              (2, 25, 5), (3, 5 , 10), (4, 15, 1 ), (4, 20, 5 ), (5, 5 , 7),
              (5, 30, 6), (6, 5 , 9 ), (6, 15, 2 ), (6, 15, 10), (7, 10, 8),
              (8, 5 , 6), (8, 10, 9 ), (9, 10, 10)
            }

    @Test
    def testAugmentingPath01(): Bool =
        let s = 1;
        let t = 5;
        let g = Set#{ (1, 1, 0, 2), (2, 1, 0, 3), (3, 1, 0, 4), (3, 1, 0, 8), (4, 1, 0, 5), (3, 1, 0, 5) };
        let p = augmentingPath(s, t, g) |> Option.getWithDefault(Path(Nil));
        Assert.eq(Path(5 :: 3 :: 2 :: 1 :: Nil), p)

    @Test
    def testAugmentingPath02(): Bool =
        let s = 0;
        let t = 5;
        let g = exampleGraph01() |> zeroFlow;
        let p = augmentingPath(s, t, g) |> Option.getWithDefault(Path(Nil));
        Assert.eq(Path(5 :: 4 :: 3 :: 0 :: Nil), p) // Shortest path in reverse order

    @Test
    def testAugmentingPath03(): Bool =
        let s = 0;
        let t = 5;
        let g = exampleGraph01() |> Set.map(x -> match x {
            case (0, 10, 3) => (0, 10, 9, 3)
            case (3, 9 , 4) => (3, 9 , 9, 4)
            case (4, 10, 5) => (4, 10, 9, 5)
            case (a, u, b)  => (a, u , 0, b)
        });
        let p = augmentingPath(s, t, g) |> Option.getWithDefault(Path(Nil));
        Assert.eq(Path(5 :: 4 :: 1 :: 0 :: Nil), p)

    @Test
    def testAugmentingPath04(): Bool =
        let s = 0;
        let t = 5;
        let g = exampleGraph01() |> Set.map(x -> match x {
            case (0, 10, 1) => (0, 10, 1 , 1)
            case (0, 10, 3) => (0, 10, 9 , 3)
            case (1, 8, 4)  => (1, 8 , 1 , 4)
            case (3, 9, 4)  => (3, 9 , 9 , 4)
            case (4, 10, 5) => (4, 10, 10, 5)
            case (a, u, b)  => (a, u , 0 , b)
        });
        let p = augmentingPath(s, t, g) |> Option.getWithDefault(Path(Nil));
        Assert.eq(Path(5 :: 2 :: 1 :: 0 :: Nil), p)

    @Test
    def testAugmentingPath05(): Bool =
        let s = 0;
        let t = 5;
        let g = exampleGraph01() |> Set.map(x -> match x {
            case (0, 10, 1) => (0, 10, 5 , 1)
            case (0, 10, 3) => (0, 10, 9 , 3)
            case (1, 8 , 4) => (1, 8 , 1 , 4)
            case (1, 4 , 2) => (1, 4 , 4 , 2)
            case (3, 9 , 4) => (3, 9 , 9 , 4)
            case (2, 10, 5) => (2, 10, 4 , 5)
            case (4, 10, 5) => (4, 10, 10, 5)
            case (a, u , b) => (a, u , 0 , b)
        });
        let p = augmentingPath(s, t, g) |> Option.getWithDefault(Path(Nil));
        Assert.eq(Path(5 :: 2 :: 4 :: 1 :: 0 :: Nil), p)

    @Test
    def testAugmentingPath06(): Bool =
        let s = 0;
        let t = 5;
        let g = zeroFlow(exampleGraph02());
        let p = augmentingPath(s, t, g) |> Option.getWithDefault(Path(Nil));
        Assert.eq(Path(5 :: 4 :: 3 :: 0 :: Nil), p)

    @Test
    def testAugmentingPath07(): Bool =
        let s = 0;
        let t = 5;
        let g = exampleGraph02() |> zeroFlow |> Functor.map(x -> match x {
            case (0, 10, 0, 3) => (0, 10, 10, 3)
            case (3, 15, 0, 4) => (3, 15, 10, 4)
            case (4, 10, 0, 5) => (4, 10, 10, 5)
            case _             => x
        });
        let p = augmentingPath(s, t, g) |> Option.getWithDefault(Path(Nil));
        Assert.eq(Path(5 :: 2 :: 1 :: 0 :: Nil), p)

    @Test
    def testAugmentingPath08(): Bool =
        let s = 0;
        let t = 5;
        let g = exampleGraph02() |> zeroFlow |> Functor.map(x -> match x {
            case (0, 10, 0, 1) => (0, 10, 10, 1)
            case (0, 10, 0, 3) => (0, 10, 10, 3)
            case (1, 25, 0, 2) => (1, 25, 10, 2)
            case (3, 15, 0, 4) => (3, 15, 10, 4)
            case (2, 10, 0, 5) => (2, 10, 10, 5)
            case (4, 10, 0, 5) => (4, 10, 10, 5)
            case _             => x
        });
        Assert.eq(None, augmentingPath(s, t, g))

    @Test
    def testMinCapacity01(): Bool =
        let p = Path(5 :: 4 :: 3 :: 2 :: 1 :: Nil);
        let g = Set#{(1, 3, 0, 2), (1, 4, 0, 3), (2, 1, 0, 3), (3, 5, 0, 4), (4, 10, 0, 5)};
        Assert.eq(1, minCapacity(p, g))

    @Test
    def testMinCapacity02(): Bool =
        let p = Path(5 :: 4 :: 3 :: 2 :: 1 :: Nil);
        let g = Set#{(1, 3, 0, 2), (1, 4, 0, 3), (2, 3, 0, 3), (3, 5, 0, 4), (4, 10, 0, 5), (6, 1, 0, 7)};
        Assert.eq(3, minCapacity(p, g))

    @Test
    def testMinCapacity03(): Bool =
        let s = 0;
        let t = 5;
        let g = exampleGraph01() |> zeroFlow;
        let p = augmentingPath(s, t, g) |> Option.getWithDefault(Path(Nil));
        Assert.eq(9, minCapacity(p, g)) // The path is picked as in testAugmentingPath02

    @Test
    def testMinCapacity04(): Bool =
        let s = 0;
        let t = 5;
        let g = exampleGraph01() |> Set.map(x -> match x {
            case (0, 10, 3) => (0, 10, 9, 3)
            case (3, 9 , 4) => (3, 9 , 9, 4)
            case (4, 10, 5) => (4, 10, 9, 5)
            case (a, u , b) => (a, u , 0, b)
        });
        let p = augmentingPath(s, t, g) |> Option.getWithDefault(Path(Nil));
        Assert.eq(1, minCapacity(p, g)) // The path is picked as in testAugmentingPath03

    @Test
    def testMinCapacity05(): Bool =
        let s = 0;
        let t = 5;
        let g = exampleGraph01() |> Set.map(x -> match x {
            case (0, 10, 1) => (0, 10, 1 , 1)
            case (0, 10, 3) => (0, 10, 9 , 3)
            case (1, 8 , 4) => (1, 8 , 1 , 4)
            case (3, 9 , 4) => (3, 9 , 9 , 4)
            case (4, 10, 5) => (4, 10, 10, 5)
            case (a, u , b) => (a, u , 0 , b)
        });
        let p = augmentingPath(s, t, g) |> Option.getWithDefault(Path(Nil));
        Assert.eq(4, minCapacity(p, g)) // The path is picked as in testAugmentingPath04

    @Test
    def testMinCapacity06(): Bool =
        let s = 0;
        let t = 5;
        let g = exampleGraph01() |> Set.map(x -> match x {
            case (0, 10, 1) => (0, 10, 5 , 1)
            case (0, 10, 3) => (0, 10, 9 , 3)
            case (1, 8 , 4) => (1, 8 , 1 , 4)
            case (1, 4 , 2) => (1, 4 , 4 , 2)
            case (3, 9 , 4) => (3, 9 , 9 , 4)
            case (2, 10, 5) => (2, 10, 4 , 5)
            case (4, 10, 5) => (4, 10, 10, 5)
            case (a, u , b) => (a, u , 0 , b)
        });
        let p = augmentingPath(s, t, g) |> Option.getWithDefault(Path(Nil));
        Assert.eq(5, minCapacity(p, g)) // The path is picked as in testAugmentingPath05

    @Test
    def testMinCapacity07(): Bool =
        let s = 0;
        let t = 5;
        let g = zeroFlow(exampleGraph02());
        let p = augmentingPath(s, t, g) |> Option.getWithDefault(Path(Nil));
        Assert.eq(10, minCapacity(p, g)) // The path is picked as in testAugmentingPath06

    @Test
    def testMinCapacity08(): Bool =
        let s = 0;
        let t = 5;
        let g = exampleGraph02() |> zeroFlow |> Functor.map(x -> match x {
            case (0, 10, 0, 3) => (0, 10, 10, 3)
            case (3, 15, 0, 4) => (3, 15, 10, 4)
            case (4, 10, 0, 5) => (4, 10, 10, 5)
            case _             => x
        });
        let p = augmentingPath(s, t, g) |> Option.getWithDefault(Path(Nil));
        Assert.eq(10, minCapacity(p, g)) // The path is picked as in testAugmentingPath07

    @Test
    def testIncreaseFlow01(): Bool =
        let s = 0;
        let t = 5;
        let g = exampleGraph01() |> zeroFlow;
        let p = augmentingPath(s, t, g) |> Option.getWithDefault(Path(Nil));
        let incr = minCapacity(p, g); // Is 9, see testMinCapacity03
        let g1 = g |> Vector.map(x -> match x {
            case (0, 10, 0, 3) => (0, 10, 9, 3)
            case (3, 9 , 0, 4) => (3, 9 , 9, 4)
            case (4, 10, 0, 5) => (4, 10, 9, 5)
            case  _            => x
        });
        Assert.eq(g1, increaseFlow(p, incr, g))

    @Test
    def testIncreaseFlow02(): Bool =
        let s = 0;
        let t = 5;
        let g = exampleGraph01() |> Set.map(x -> match x {
            case (0, 10, 3) => (0, 10, 9, 3)
            case (3, 9 , 4) => (3, 9 , 9, 4)
            case (4, 10, 5) => (4, 10, 9, 5)
            case (a, u , b) => (a, u , 0, b)
        });
        let p = augmentingPath(s, t, g) |> Option.getWithDefault(Path(Nil));
        let incr = minCapacity(p, g); // Is 1, see testMinCapacity04
        let g1 = g |> Set.map(x -> match x {
            case (0, 10, 0, 1) => (0, 10, 1 , 1)
            case (1, 8 , 0, 4) => (1, 8 , 1 , 4)
            case (4, 10, 9, 5) => (4, 10, 10, 5)
            case _             => x
        }) |> Foldable.toVector;
        Assert.eq(g1, increaseFlow(p, incr, g))

    @Test
    def testIncreaseFlow03(): Bool =
        let s = 0;
        let t = 5;
        let g = exampleGraph01() |> Set.map(x -> match x {
            case (0, 10, 1) => (0, 10, 1 , 1)
            case (0, 10, 3) => (0, 10, 9 , 3)
            case (1, 8 , 4) => (1, 8 , 1 , 4)
            case (3, 9 , 4) => (3, 9 , 9 , 4)
            case (4, 10, 5) => (4, 10, 10, 5)
            case (a, u , b) => (a, u , 0 , b)
        });
        let p = augmentingPath(s, t, g) |> Option.getWithDefault(Path(Nil));
        let incr = minCapacity(p, g); // Is 4, see testMinCapacity05
        let g1 = g |> Set.map(x -> match x {
            case (0, 10, 1, 1) => (0, 10, 5, 1)
            case (1, 4 , 0, 2) => (1, 4 , 4, 2)
            case (2, 10, 0, 5) => (2, 10, 4, 5)
            case _             => x
        }) |> Foldable.toVector;
        Assert.eq(g1, increaseFlow(p, incr, g))

    @Test
    def testIncreaseFlow04(): Bool =
        let s = 0;
        let t = 5;
        let g = zeroFlow(exampleGraph02());
        let p = augmentingPath(s, t, g) |> Option.getWithDefault(Path(Nil));
        let incr = minCapacity(p, g); // Is 10, see testMinCapacity07
        let g1 = g |> Vector.map(x -> match x {
            case (0, 10, 0, 3) => (0, 10, 10, 3)
            case (3, 15, 0, 4) => (3, 15, 10, 4)
            case (4, 10, 0, 5) => (4, 10, 10, 5)
            case _ => x
        }) |> Foldable.toVector;
        Assert.eq(g1, increaseFlow(p, incr, g))

    @Test
    def testIncreaseFlow05(): Bool =
        let s = 0;
        let t = 5;
        let g = exampleGraph02() |> zeroFlow |> Functor.map(x -> match x {
            case (0, 10, 0, 3) => (0, 10, 10, 3)
            case (3, 15, 0, 4) => (3, 15, 10, 4)
            case (4, 10, 0, 5) => (4, 10, 10, 5)
            case _             => x
        });
        let p = augmentingPath(s, t, g) |> Option.getWithDefault(Path(Nil));
        let incr = minCapacity(p, g); // Is 10, see testMinCapacity08
        let g1 = g |> Functor.map(x -> match x {
            case (0, 10, 0, 1) => (0, 10, 10, 1)
            case (1, 25, 0, 2) => (1, 25, 10, 2)
            case (2, 10, 0, 5) => (2, 10, 10, 5)
            case _             => x
        });
        Assert.eq(g1, increaseFlow(p, incr, g))

    @Test
    def testGetMaxFlow01(): Bool =
        let t = 5;
        let g = exampleGraph02() |> zeroFlow |> Functor.map(x -> match x {
            case (0, 10, 0, 1) => (0, 10, 10, 1)
            case (0, 10, 0, 3) => (0, 10, 10, 3)
            case (1, 25, 0, 2) => (1, 25, 10, 2)
            case (3, 15, 0, 4) => (3, 15, 10, 4)
            case (2, 10, 0, 5) => (2, 10, 10, 5)
            case (4, 10, 0, 5) => (4, 10, 10, 5)
            case _             => x
        });
        Assert.eq(20, getMaxFlow(t, g))

    @Test
    def testExampleGraph01(): Bool =
        Assert.eq(19, maxFlow(0, 5, exampleGraph01()))

    @Test
    def testExampleGraph02(): Bool =
        Assert.eq(20, maxFlow(0, 5, exampleGraph02()))

    @Test
    def testExampleGraph03(): Bool =
        Assert.eq(2000, maxFlow(0, 3, exampleGraph03()))

    @Test
    def testExampleGraph04(): Bool =
        Assert.eq(20, maxFlow(0, 10, exampleGraph04()))

}

pub def main(): Unit \ IO =
    let g1 = FordFulkerson.exampleGraph01() |> FordFulkerson.maxFlow(src = 0, dst = 5);
    let g2 = FordFulkerson.exampleGraph02() |> FordFulkerson.maxFlow(src = 0, dst = 5);
    let g3 = FordFulkerson.exampleGraph03() |> FordFulkerson.maxFlow(src = 0, dst = 3);
    let g3 = FordFulkerson.exampleGraph04() |> FordFulkerson.maxFlow(src = 0, dst = 10);
    println("Expected maximum flow 19, got ${g1}");
    println("Expected maximum flow 20, got ${g2}");
    println("Expected maximum flow 2000, got ${g3}");
    println("Expected maximum flow 20, got ${g4}")

mod FordFulkerson {

    use Path.Path;

    enum Path with ToString {
        case Path(List[Int32])
    }

    instance Eq[Path] {
        pub def eq(x: Path, y: Path): Bool =
            let Path(xs) = x;
            let Path(ys) = y;
            List.length(xs) == List.length(ys)
    }

    instance Order[Path] {
        pub def compare(x: Path, y: Path): Comparison =
            let Path(xs) = x;
            let Path(ys) = y;
            if (List.length(xs) ==  List.length(ys))
                Comparison.EqualTo
            else if (List.length(xs) <= List.length(ys))
                Comparison.GreaterThan
            else
                Comparison.LessThan
    }

    instance LowerBound[Path] {
        // The longest list
        pub def minValue(): Path = Path(List.range(1, 100))
    }

    instance PartialOrder[Path] {
        pub def lessEqual(x: Path, y: Path): Bool =
            let Path(xs) = x;
            let Path(ys) = y;
            List.length(xs) >= List.length(ys)
    }

    instance JoinLattice[Path] {
        pub def leastUpperBound(x: Path, y: Path): Path =
            let Path(xs) = x;
            let Path(ys) = y;
            if (List.length(xs) <= List.length(ys)) x else y
    }

    instance MeetLattice[Path] {
        pub def greatestLowerBound(x: Path, y: Path): Path = ???
    }


    pub def init(y: Int32, x: Int32): Path =
        Path(y :: x :: Nil)

    pub def cons(z: Int32, p: Path): Path = match p {
        case Path(xs) => Path(z :: xs)
    }

    type alias From = Int32
    type alias To = Int32
    type alias UpperCapacity = Int32
    type alias Flow = Int32
    type alias FlowNetwork = (From, To, UpperCapacity, Flow)

    pub def maxFlow(src: { src = From }, dst: { dst = To }, g: m[(From, To, UpperCapacity)]): Flow with Foldable[m] =
        def fordFulkerson(flowNetwork) = match augmentingPath(src, dst, flowNetwork) {
            case None       => getMaxFlow(dst.dst, flowNetwork)
            case Some(path) =>
                let incr = pathMinCapacity(path, flowNetwork);
                let updatedNetwork = increaseFlow(path, incr, flowNetwork);
                fordFulkerson(updatedNetwork)
        };
        // Init with 0 flow
        fordFulkerson(zeroFlow(g))

    def zeroFlow(g: m[(From, To, UpperCapacity)]): Vector[FlowNetwork] with Foldable[m] =
        Foldable.toVector(g) |> Vector.map(match (x, y, z) -> (x, y, z, 0))

    def getMaxFlow(dst: To, g: m[FlowNetwork]): Flow with Foldable[m] =
        g
        |> Foldable.toVector
        |> Vector.filter(match (_, d, _, _) -> d == dst)
        |> Vector.map(match (_, _, _, f) -> f)
        |> Vector.sum

    def augmentingPath(src: { src = From }, dst: { dst = To }, g: m[FlowNetwork]): Option[Path] with Foldable[m] =
        let edges = inject g into Edge;
        let rules = #{
            Reach(x, y; init(y, x)) :- Edge(x, y, u, f),                 if (u - f) > 0. // Forward edge
            Reach(x, z; cons(z, p)) :- Reach(x, y; p), Edge(y, z, u, f), if (u - f) > 0. // Forward edge
            Reach(x, y; init(y, x)) :- Edge(y, x, u, f),                 if f > 0.       // Back edge
            Reach(x, z; cons(z, p)) :- Reach(x, y; p), Edge(z, y, u, f), if f > 0.       // Back edge
        };
        let s = src.src;
        let t = dst.dst;
        let result = query edges, rules select fn from Reach(s, t; fn);
        Vector.head(result) |> Option.flatMap(match Path(xs) -> if (List.isEmpty(xs)) None else Some(Path(xs)))

    def pathMinCapacity(p: Path, g: m[FlowNetwork]): Int32 with Foldable[m] =
        let onPath = (s, d) -> isForwardEdge(src = s, dst = d, p) or isBackEdge(src = s, dst = d, p);
        let optMin = g |> Foldable.filter(match (s, d, _, _) -> onPath(s, d))
            |> List.map(match (_, _, u, f) -> u - f)
            |> List.minimum;
        match optMin {
            case Some(u) => u
            case None    => unreachable!() // Handle this better somehow
        }

    def increaseFlow(p: Path, incr: Flow, g: m[FlowNetwork]): Vector[FlowNetwork] with Foldable[m] =
        g
        |> Foldable.toVector
        |> Vector.map(match (s, d, u, f) ->
            if (isForwardEdge(src = s, dst = d, p))
                (s, d, u, f + incr)
            else if (isBackEdge(src = s, dst = d, p))
                (s, d, u, f - incr)
            else
                (s, d, u, f)
        )

    def isForwardEdge(src: { src = From }, dst: { dst = To }, p: Path): Bool =
        match (indexOf(src.src, p), indexOf(dst.dst, p)) { // A path is sorted in reverse order
            case (Some(si), Some(di)) if di + 1 == si => true
            case _ => false
        }

    def isBackEdge(src: { src = From }, dst: { dst = To }, p: Path): Bool =
        match (indexOf(src.src, p), indexOf(dst.dst, p)) { // A path is sorted in reverse order
            case (Some(si), Some(di)) if si + 1 == di => true
            case _ => false
        }

    def indexOf(a: Int32, p: Path): Option[Int32] =
        let Path(xs) = p;
        List.indexOf(a, xs)

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
    pub def exampleGraph01(): Set[(From, To, UpperCapacity)] =
        Set#{ (0, 1, 10), (0, 3, 10), (1, 3, 2), (1, 2, 4), (1, 4, 8), (3, 4, 9), (4, 2, 6), (2, 5, 10), (4, 5, 10) }

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
    pub def exampleGraph02(): Set[(From, To, UpperCapacity)] =
        Set#{ (0, 1, 10), (0, 3, 10), (1, 2, 25), (4, 1, 6), (3, 4, 15), (2, 5, 10), (4, 5, 10) }

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
    pub def exampleGraph03(): Set[(From, To, UpperCapacity)] =
        Set#{ (0, 1, 1000), (0, 2, 1000), (1, 2, 1), (1, 3, 1000), (2, 3, 1000) }

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
    /// 3 -> 10, capacity 5
    /// 6 -> 10, capacity 15
    /// 9 -> 10, capacity 10
    /// ```
    ///
    /// The maximum flow is `20`.
    ///
    pub def exampleGraph04(): Set[(From, To, UpperCapacity)] =
        Set#{   (0, 1, 5), (0, 4, 10), (0, 7, 5), (4, 1, 15), (1, 2, 10),
                (4, 5, 20), (5, 7, 5), (7, 8, 10), (2, 5, 25), (2, 3, 10),
                (6, 2, 15), (5, 6, 30), (8, 6, 5), (8, 9, 10), (3, 10, 5),
                (6, 10, 15), (9, 10, 10)
            }

    @Test
    def testAugmentingPath01(): Bool =
        let s = 1;
        let t = 5;
        let g = Set#{ (1, 2, 1, 0), (2, 3, 1, 0), (3, 4, 1, 0), (3, 8, 1, 0), (4, 5, 1, 0), (3, 5, 1, 0) };
        let Path(xs) = augmentingPath(src = s, dst = t, g) |> Option.getWithDefault(Path(Nil));
        Assert.eq(5 :: 3 :: 2 :: 1 :: Nil, xs)

    @Test
    def testAugmentingPath02(): Bool =
        let s = 0;
        let t = 5;
        let g = exampleGraph01() |> Set.map(match (a, b, u) -> (a, b, u, 0));
        let Path(xs) = augmentingPath(src = s, dst = t, g) |> Option.getWithDefault(Path(Nil));
        Assert.eq(5 :: 4 :: 3 :: 0 :: Nil, xs) // Shortest path in reverse order

    @Test
    def testAugmentingPath03(): Bool =
        let s = 0;
        let t = 5;
        let g = exampleGraph01() |> Set.map(x -> match x {
            case (0, 3, 10) => (0, 3, 10, 9)
            case (3, 4, 9)  => (3, 4, 9 , 9)
            case (4, 5, 10) => (4, 5, 10, 9)
            case (a, b, u)  => (a, b, u , 0)
        });
        let Path(xs) = augmentingPath(src = s, dst = t, g) |> Option.getWithDefault(Path(Nil));
        Assert.eq(5 :: 4 :: 1 :: 0 :: Nil, xs)

    @Test
    def testAugmentingPath04(): Bool =
        let s = 0;
        let t = 5;
        let g = exampleGraph01() |> Set.map(x -> match x {
            case (0, 1, 10) => (0, 1, 10, 1)
            case (0, 3, 10) => (0, 3, 10, 9)
            case (1, 4, 8)  => (1, 4, 8 , 1)
            case (3, 4, 9)  => (3, 4, 9 , 9)
            case (4, 5, 10) => (4, 5, 10, 10)
            case (a, b, u)  => (a, b, u , 0)
        });
        let Path(xs) = augmentingPath(src = s, dst = t, g) |> Option.getWithDefault(Path(Nil));
        Assert.eq(5 :: 2 :: 1 :: 0 :: Nil, xs)

    @Test
    def testAugmentingPath05(): Bool =
        let s = 0;
        let t = 5;
        let g = exampleGraph01() |> Set.map(x -> match x {
            case (0, 1, 10) => (0, 1, 10, 5)
            case (0, 3, 10) => (0, 3, 10, 9)
            case (1, 4, 8)  => (1, 4, 8 , 1)
            case (1, 2, 4)  => (1, 2, 4 , 4)
            case (3, 4, 9)  => (3, 4, 9 , 9)
            case (2, 5, 10) => (2, 5, 10, 4)
            case (4, 5, 10) => (4, 5, 10, 10)
            case (a, b, u)  => (a, b, u , 0)
        });
        let Path(xs) = augmentingPath(src = s, dst = t, g) |> Option.getWithDefault(Path(Nil));
        Assert.eq(5 :: 2 :: 4 :: 1 :: 0 :: Nil, xs)

    @Test
    def testAugmentingPath06(): Bool =
        let s = 0;
        let t = 5;
        let g = zeroFlow(exampleGraph02());
        let Path(xs) = augmentingPath(src = s, dst = t, g) |> Option.getWithDefault(Path(Nil));
        Assert.eq(5 :: 4 :: 3 :: 0 :: Nil, xs)

    @Test
    def testAugmentingPath07(): Bool =
        let s = 0;
        let t = 5;
        let g = exampleGraph02() |> zeroFlow |> Functor.map(x -> match x {
            case (0, 3, 10, 0) => (0, 3, 10, 10)
            case (3, 4, 15, 0) => (3, 4, 15, 10)
            case (4, 5, 10, 0) => (4, 5, 10, 10)
            case _             => x
        });
        let Path(xs) = augmentingPath(src = s, dst = t, g) |> Option.getWithDefault(Path(Nil));
        Assert.eq(5 :: 2 :: 1 :: 0 :: Nil, xs)

    @Test
    def testAugmentingPath08(): Bool =
        let s = 0;
        let t = 5;
        let g = exampleGraph02() |> zeroFlow |> Functor.map(x -> match x {
            case (0, 1, 10, 0) => (0, 1, 10, 10)
            case (0, 3, 10, 0) => (0, 3, 10, 10)
            case (1, 2, 25, 0) => (1, 2, 25, 10)
            case (3, 4, 15, 0) => (3, 4, 15, 10)
            case (2, 5, 10, 0) => (2, 5, 10, 10)
            case (4, 5, 10, 0) => (4, 5, 10, 10)
            case _             => x
        });
        Assert.eq(None, augmentingPath(src = s, dst = t, g))

    @Test
    def testPathMinCapacity01(): Bool =
        let p = Path(5 :: 4 :: 3 :: 2 :: 1 :: Nil);
        let g = Set#{(1, 2, 3, 0), (1, 3, 4, 0), (2, 3, 1, 0), (3, 4, 5, 0), (4, 5, 10, 0)};
        Assert.eq(1, pathMinCapacity(p, g))

    @Test
    def testPathMinCapacity02(): Bool =
        let p = Path(5 :: 4 :: 3 :: 2 :: 1 :: Nil);
        let g = Set#{(1, 2, 3, 0), (1, 3, 4, 0), (2, 3, 3, 0), (3, 4, 5, 0), (4, 5, 10, 0), (6, 7, 1, 0)};
        Assert.eq(3, pathMinCapacity(p, g))

    @Test
    def testPathMinCapacity03(): Bool =
        let s = 0;
        let t = 5;
        let g = exampleGraph01() |> Set.map(match (a, b, u) -> (a, b, u, 0));
        let p = augmentingPath(src = s, dst = t, g) |> Option.getWithDefault(Path(Nil));
        Assert.eq(9, pathMinCapacity(p, g)) // The path is picked as in testAugmentingPath02

    @Test
    def testPathMinCapacity04(): Bool =
        let s = 0;
        let t = 5;
        let g = exampleGraph01() |> Set.map(x -> match x {
            case (0, 3, 10) => (0, 3, 10, 9)
            case (3, 4, 9)  => (3, 4, 9 , 9)
            case (4, 5, 10) => (4, 5, 10, 9)
            case (a, b, u)  => (a, b, u , 0)
        });
        let p = augmentingPath(src = s, dst = t, g) |> Option.getWithDefault(Path(Nil));
        Assert.eq(1, pathMinCapacity(p, g)) // The path is picked as in testAugmentingPath03

    @Test
    def testPathMinCapacity05(): Bool =
        let s = 0;
        let t = 5;
        let g = exampleGraph01() |> Set.map(x -> match x {
            case (0, 1, 10) => (0, 1, 10, 1)
            case (0, 3, 10) => (0, 3, 10, 9)
            case (1, 4, 8)  => (1, 4, 8 , 1)
            case (3, 4, 9)  => (3, 4, 9 , 9)
            case (4, 5, 10) => (4, 5, 10, 10)
            case (a, b, u)  => (a, b, u , 0)
        });
        let p = augmentingPath(src = s, dst = t, g) |> Option.getWithDefault(Path(Nil));
        Assert.eq(4, pathMinCapacity(p, g)) // The path is picked as in testAugmentingPath04

    @Test
    def testPathMinCapacity06(): Bool =
        let s = 0;
        let t = 5;
        let g = exampleGraph01() |> Set.map(x -> match x {
            case (0, 1, 10) => (0, 1, 10, 5)
            case (0, 3, 10) => (0, 3, 10, 9)
            case (1, 4, 8)  => (1, 4, 8 , 1)
            case (1, 2, 4)  => (1, 2, 4 , 4)
            case (3, 4, 9)  => (3, 4, 9 , 9)
            case (2, 5, 10) => (2, 5, 10, 4)
            case (4, 5, 10) => (4, 5, 10, 10)
            case (a, b, u)  => (a, b, u , 0)
        });
        let p = augmentingPath(src = s, dst = t, g) |> Option.getWithDefault(Path(Nil));
        Assert.eq(5, pathMinCapacity(p, g)) // The path is picked as in testAugmentingPath05

    @Test
    def testPathMinCapacity07(): Bool =
        let s = 0;
        let t = 5;
        let g = zeroFlow(exampleGraph02());
        let p = augmentingPath(src = s, dst = t, g) |> Option.getWithDefault(Path(Nil));
        Assert.eq(10, pathMinCapacity(p, g)) // The path is picked as in testAugmentingPath06

    @Test
    def testPathMinCapacity08(): Bool =
        let s = 0;
        let t = 5;
        let g = exampleGraph02() |> zeroFlow |> Functor.map(x -> match x {
            case (0, 3, 10, 0) => (0, 3, 10, 10)
            case (3, 4, 15, 0) => (3, 4, 15, 10)
            case (4, 5, 10, 0) => (4, 5, 10, 10)
            case _             => x
        });
        let p = augmentingPath(src = s, dst = t, g) |> Option.getWithDefault(Path(Nil));
        Assert.eq(10, pathMinCapacity(p, g)) // The path is picked as in testAugmentingPath07

    @Test
    def testIncreaseFlow01(): Bool =
        let s = 0;
        let t = 5;
        let g = exampleGraph01() |> Set.map(match (a, b, u) -> (a, b, u, 0));
        let p = augmentingPath(src = s, dst = t, g) |> Option.getWithDefault(Path(Nil));
        let incr = pathMinCapacity(p, g); // Is 9, see testPathMinCapacity03
        let g1 = g |> Set.map(x -> match x {
            case (0, 3, 10, 0) => (0, 3, 10, 9)
            case (3, 4, 9 , 0) => (3, 4, 9 , 9)
            case (4, 5, 10, 0) => (4, 5, 10, 9)
            case _             => x
        }) |> Foldable.toVector;
        Assert.eq(g1, increaseFlow(p, incr, g))

    @Test
    def testIncreaseFlow02(): Bool =
        let s = 0;
        let t = 5;
        let g = exampleGraph01() |> Set.map(x -> match x {
            case (0, 3, 10) => (0, 3, 10, 9)
            case (3, 4, 9)  => (3, 4, 9 , 9)
            case (4, 5, 10) => (4, 5, 10, 9)
            case (a, b, u)  => (a, b, u , 0)
        });
        let p = augmentingPath(src = s, dst = t, g) |> Option.getWithDefault(Path(Nil));
        let incr = pathMinCapacity(p, g); // Is 1, see testPathMinCapacity04
        let g1 = g |> Set.map(x -> match x {
            case (0, 1, 10, 0) => (0, 1, 10, 1)
            case (1, 4, 8 , 0) => (1, 4, 8 , 1)
            case (4, 5, 10, 9) => (4, 5, 10, 10)
            case _             => x
        }) |> Foldable.toVector;
        Assert.eq(g1, increaseFlow(p, incr, g))

    @Test
    def testIncreaseFlow03(): Bool =
        let s = 0;
        let t = 5;
        let g = exampleGraph01() |> Set.map(x -> match x {
            case (0, 1, 10) => (0, 1, 10, 1)
            case (0, 3, 10) => (0, 3, 10, 9)
            case (1, 4, 8)  => (1, 4, 8 , 1)
            case (3, 4, 9)  => (3, 4, 9 , 9)
            case (4, 5, 10) => (4, 5, 10, 10)
            case (a, b, u)  => (a, b, u , 0)
        });
        let p = augmentingPath(src = s, dst = t, g) |> Option.getWithDefault(Path(Nil));
        let incr = pathMinCapacity(p, g); // Is 4, see testPathMinCapacity05
        let g1 = g |> Set.map(x -> match x {
            case (0, 1, 10, 1) => (0, 1, 10, 5)
            case (1, 2, 4 , 0) => (1, 2, 4 , 4)
            case (2, 5, 10, 0) => (2, 5, 10, 4)
            case _             => x
        }) |> Foldable.toVector;
        Assert.eq(g1, increaseFlow(p, incr, g))

    @Test
    def testIncreaseFlow04(): Bool =
        let s = 0;
        let t = 5;
        let g = zeroFlow(exampleGraph02());
        let p = augmentingPath(src = s, dst = t, g) |> Option.getWithDefault(Path(Nil));
        let incr = pathMinCapacity(p, g); // Is 10, see testPathMinCapacity07
        let g1 = g |> Vector.map(x -> match x {
            case (0, 3, 10, 0) => (0, 3, 10, 10)
            case (3, 4, 15, 0) => (3, 4, 15, 10)
            case (4, 5, 10, 0) => (4, 5, 10, 10)
            case _ => x
        }) |> Foldable.toVector;
        Assert.eq(g1, increaseFlow(p, incr, g))

    @Test
    def testIncreaseFlow05(): Bool =
        let s = 0;
        let t = 5;
        let g = exampleGraph02() |> zeroFlow |> Functor.map(x -> match x {
            case (0, 3, 10, 0) => (0, 3, 10, 10)
            case (3, 4, 15, 0) => (3, 4, 15, 10)
            case (4, 5, 10, 0) => (4, 5, 10, 10)
            case _             => x
        });
        let p = augmentingPath(src = s, dst = t, g) |> Option.getWithDefault(Path(Nil));
        let incr = pathMinCapacity(p, g); // Is 10, see testPathMinCapacity08
        let g1 = g |> Functor.map(x -> match x {
            case (0, 1, 10, 0) => (0, 1, 10, 10)
            case (1, 2, 25, 0) => (1, 2, 25, 10)
            case (2, 5, 10, 0) => (2, 5, 10, 10)
            case _             => x
        });
        Assert.eq(g1, increaseFlow(p, incr, g))

    @Test
    def testGetMaxFlow01(): Bool =
        let t = 5;
        let g = exampleGraph02() |> zeroFlow |> Functor.map(x -> match x {
            case (0, 1, 10, 0) => (0, 1, 10, 10)
            case (0, 3, 10, 0) => (0, 3, 10, 10)
            case (1, 2, 25, 0) => (1, 2, 25, 10)
            case (3, 4, 15, 0) => (3, 4, 15, 10)
            case (2, 5, 10, 0) => (2, 5, 10, 10)
            case (4, 5, 10, 0) => (4, 5, 10, 10)
            case _             => x
        });
        Assert.eq(20, getMaxFlow(t, g))

    @Test
    def testExampleGraph01(): Bool =
        Assert.eq(19, maxFlow(src = 0, dst = 5, exampleGraph01()))

    @Test
    def testExampleGraph02(): Bool =
        Assert.eq(20, maxFlow(src = 0, dst = 5, exampleGraph02()))

    @Test
    def testExampleGraph03(): Bool =
        Assert.eq(2000, maxFlow(src = 0, dst = 3, exampleGraph03()))

    @Test
    def testExampleGraph04(): Bool =
        Assert.eq(20, maxFlow(src = 0, dst = 10, exampleGraph04()))

}

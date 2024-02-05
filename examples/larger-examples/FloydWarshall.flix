// This is a Flix implementation of a Floyd-Warshall-like algorithm
// for finding shortest paths between all pairs of nodes in a graph.

// Elements of the distance lattice
enum Dist with Eq, Order, ToString {
  case Bot,        // Not reachable
  case Dst(Int32), // Reachable with some minimum distance
  case Top         // Reachable from a negative-cost cycle
}

instance LowerBound[Dist] {
    pub def minValue(): Dist = Dist.Bot
}

instance PartialOrder[Dist] {
    pub def lessEqual(x: Dist, y: Dist): Bool = match (x, y) {
        case (_, Dist.Top)                => true
        case (Dist.Bot, _)                => true
        case (Dist.Dst(n1), Dist.Dst(n2)) => n1 >= n2
        case _                            => false
    }
}

instance JoinLattice[Dist] {
    pub def leastUpperBound(x: Dist, y: Dist): Dist = match (x, y) {
        case (Dist.Bot, _)                => y
        case (_, Dist.Bot)                => x
        case (Dist.Dst(n1), Dist.Dst(n2)) => Dist.Dst(Int32.min(n1, n2))
        case _                            => Dist.Top
    }
}

instance MeetLattice[Dist] {
    pub def greatestLowerBound(x: Dist, y: Dist): Dist = match (x, y) {
        case (Dist.Top, z)                => z
        case (z, Dist.Top)                => z
        case (Dist.Dst(n1), Dist.Dst(n2)) => Dist.Dst(Int32.max(n1, n2))
        case _                            => Dist.Bot
    }
}

///
/// Transfer function.
/// Returns the sum of two given distances.
///
def sum(e1: Dist, e2: Dist): Dist = match (e1, e2) {
  case (Dist.Top, _)                => Dist.Top
  case (_, Dist.Top)                => Dist.Top
  case (Dist.Dst(n1), Dist.Dst(n2)) => Dist.Dst(n1 + n2)
  case _                            => Dist.Bot
}

///
/// Filter function.
/// Returns `true` if the given distance `d` is negative.
///
def negativeDist(d: Dist): Bool = match d {
  case Dist.Top    => true
  case Dist.Dst(x) => x < 0
  case _           => false
}

def main(): Unit \ IO =
    let p = #{
        // Copy input relation into the ShortestDist lattice
        ShortestDist(a, b; Dist.Dst(d)) :- Edge(a, b, d).

        // If a->b->c, try using that to get a->c
        ShortestDist(a, c; sum(d1, d2)) :- ShortestDist(a, b; d1), ShortestDist(b, c; d2).

        // If you can get from a->a with negative distance, then it is part of a negative cycle
        ShortestDist(a, a; Dist.Top) :- ShortestDist(a, a; d), if negativeDist(d).

        // Example input data
        Edge("a", "b", 1).
        Edge("b", "c", 2).
        Edge("c", "a", 3).
        Edge("c", "d", 4).
        Edge("d", "e", 7).
        Edge("d", "f", 11).
        Edge("f", "e", 23).

        // Negative cycle
        Edge("1", "2", -3).
        Edge("2", "3", 1).
        Edge("3", "1", 1).
        Edge("3", "4", 30).
    };
    let res = query p select (x, y, z) from ShortestDist(x, y; z);
    println(res)

// Expected output
// ShortestDist
// +-----+-----+--------------+
// | a   | b   | d            |
// +-----+-----+--------------+
// | "1" | "1" | Dist.Top     |
// | "1" | "4" | Dist.Top     |
// | "1" | "2" | Dist.Top     |
// | "1" | "3" | Dist.Top     |
// | "2" | "4" | Dist.Top     |
// | "2" | "2" | Dist.Top     |
// | "2" | "3" | Dist.Top     |
// | "2" | "1" | Dist.Top     |
// | "3" | "2" | Dist.Top     |
// | "3" | "3" | Dist.Top     |
// | "3" | "1" | Dist.Top     |
// | "3" | "4" | Dist.Top     |
// | "a" | "b" | Dist.Dst(1)  |
// | "a" | "e" | Dist.Dst(14) |
// | "a" | "c" | Dist.Dst(3)  |
// | "a" | "f" | Dist.Dst(18) |
// | "a" | "d" | Dist.Dst(7)  |
// | "a" | "a" | Dist.Dst(6)  |
// | "b" | "b" | Dist.Dst(6)  |
// | "b" | "e" | Dist.Dst(13) |
// | "b" | "f" | Dist.Dst(17) |
// | "b" | "c" | Dist.Dst(2)  |
// | "b" | "a" | Dist.Dst(5)  |
// | "b" | "d" | Dist.Dst(6)  |
// | "c" | "c" | Dist.Dst(6)  |
// | "c" | "f" | Dist.Dst(15) |
// | "c" | "a" | Dist.Dst(3)  |
// | "c" | "d" | Dist.Dst(4)  |
// | "c" | "e" | Dist.Dst(11) |
// | "c" | "b" | Dist.Dst(4)  |
// | "d" | "e" | Dist.Dst(7)  |
// | "d" | "f" | Dist.Dst(11) |
// | "f" | "e" | Dist.Dst(23) |
// +-----+-----+--------------+

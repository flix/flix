// This is a Flix implementation of a Floyd-Warshall-like algorithm
// for finding shortest paths between all pairs of nodes in a graph.

// Elements of the distance lattice
enum Dist with Eq, Order, ToString {
  case Bot,      // Not reachable
  case Dst(Int32), // Reachable with some minimum distance
  case Top       // Reachable from a negative-cost cycle
}

instance LowerBound[Dist] {
    pub def minValue(): Dist = Dist.Bot
}

instance PartialOrder[Dist] {
    pub def lessEqual(x: Dist, y: Dist): Bool = match (x, y) {
        case (_, Top) => true
        case (Bot, _) => true
        case (Dst(n1), Dst(n2)) => n1 >= n2
        case _ => false
    }
}

instance JoinLattice[Dist] {
    pub def leastUpperBound(x: Dist, y: Dist): Dist = match (x, y) {
        case (Bot, _)           => y
        case (_, Bot)           => x
        case (Dst(n1), Dst(n2)) => Dst(Int32.min(n1, n2))
        case _                  => Top
    }
}

instance MeetLattice[Dist] {
    pub def greatestLowerBound(x: Dist, y: Dist): Dist = match (x, y) {
        case (Top, z) => z
        case (z, Top) => z
        case (Dst(n1), Dst(n2)) => Dst(Int32.max(n1, n2))
        case _ => Bot
    }
}

///
/// Transfer function.
/// Returns the sum of two given distances.
///
def sum(e1: Dist, e2: Dist): Dist = match (e1, e2) {
  case (Top, _) => Top
  case (_, Top) => Top
  case (Dst(n1), Dst(n2)) => Dst(n1 + n2)
  case _ => Bot
}

///
/// Filter function.
/// Returns `true` if the given distance `d` is negative.
///
def negativeDist(d: Dist): Bool = match d {
  case Top => true
  case Dst(x) => x < 0
  case _ => false
}

def main(): Unit \ IO =
    let p = #{
        // Copy input relation into the ShortestDist lattice
        ShortestDist(a, b; Dst(d)) :- Edge(a, b, d).

        // If a->b->c, try using that to get a->c
        ShortestDist(a, c; sum(d1, d2)) :- ShortestDist(a, b; d1), ShortestDist(b, c; d2).

        // If you can get from a->a with negative distance, then it is part of a negative cycle
        ShortestDist(a, a; Top) :- ShortestDist(a, a; d), if negativeDist(d).

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
    let m = solve p;
    Fixpoint.unsafePrint(m);
    ()

// Expected output
// ShortestDist
// +-----+-----+---------+
// | a   | b   | d       |
// +-----+-----+---------+
// | "1" | "1" | Top     |
// | "1" | "4" | Top     |
// | "1" | "2" | Top     |
// | "1" | "3" | Top     |
// | "2" | "4" | Top     |
// | "2" | "2" | Top     |
// | "2" | "3" | Top     |
// | "2" | "1" | Top     |
// | "3" | "2" | Top     |
// | "3" | "3" | Top     |
// | "3" | "1" | Top     |
// | "3" | "4" | Top     |
// | "a" | "b" | Dst(1)  |
// | "a" | "e" | Dst(14) |
// | "a" | "c" | Dst(3)  |
// | "a" | "f" | Dst(18) |
// | "a" | "d" | Dst(7)  |
// | "a" | "a" | Dst(6)  |
// | "b" | "b" | Dst(6)  |
// | "b" | "e" | Dst(13) |
// | "b" | "f" | Dst(17) |
// | "b" | "c" | Dst(2)  |
// | "b" | "a" | Dst(5)  |
// | "b" | "d" | Dst(6)  |
// | "c" | "c" | Dst(6)  |
// | "c" | "f" | Dst(15) |
// | "c" | "a" | Dst(3)  |
// | "c" | "d" | Dst(4)  |
// | "c" | "e" | Dst(11) |
// | "c" | "b" | Dst(4)  |
// | "d" | "e" | Dst(7)  |
// | "d" | "f" | Dst(11) |
// | "f" | "e" | Dst(23) |
// +-----+-----+---------+

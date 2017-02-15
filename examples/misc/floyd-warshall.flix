// This is a Flix implementation of a Floyd-Warshall-like algorithm
// for finding shortest paths between all pairs of points.

// Inputs: Directed distances d between nodes a and b
rel Edge(a: Str, b: Str, d: Int)

// Outputs: Shortest distance d from a to b
lat ShortestDist(a: Str, b: Str, d: Dist)

// Elements of the distance lattice
enum Dist {
  case Top,      // Reachable from a negative-cost cycle
  case Dst(Int), // Reachable with some minimum distance
  case Bot       // Not reachable
}

// Lattice operations
// Shorter distances are "better"
def leq(e1: Dist, e2: Dist): Bool = match (e1, e2) with {
  case (_, Top) => true
  case (Bot, _) => true
  case (Dst(n1), Dst(n2)) => n1 >= n2
  case _ => false
}
// LUB of two distances is the minimum
def lub(e1: Dist, e2: Dist): Dist = match (e1, e2) with {
  case (Bot, x) => x
  case (x, Bot) => x
  case (Dst(n1), Dst(n2)) => if (n1 < n2) Dst(n1) else Dst(n2)
  case _ => Top
}
// GLB of two distances is the maximum
def glb(e1: Dist, e2: Dist): Dist = match (e1, e2) with {
  case (Top, x) => x
  case (x, Top) => x
  case (Dst(n1), Dst(n2)) => if (n1 < n2) Dst(n2) else Dst(n1)
  case _ => Bot
}

let Dist<> = (Bot, Top, leq, lub, glb)

// Copy input relation into the ShortestDist lattice
ShortestDist(a, b, Dst(d)) :- Edge(a, b, d).

// If a->b->c, try using that to get a->c
ShortestDist(a, c, sumDist(d1, d2)) :- ShortestDist(a, b, d1), ShortestDist(b, c, d2).

// If you can get from a->a with negative distance, then it is part of a negative cycle
ShortestDist(a, a, Top) :- ShortestDist(a, a, d), negativeDist(d).

// Transfer function that gets the sum of two distances
def sumDist(e1: Dist, e2: Dist): Dist = match (e1, e2) with {
  case (Top, _) => Top
  case (_, Top) => Top
  case (Dst(n1), Dst(n2)) => Dst(n1 + n2)
  case _ => Bot
}

// Filter function that gets whether or not a distance is negative
def negativeDist(d: Dist): Bool = match d with {
  case Dst(x) => x < 0
  case x => false
}

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

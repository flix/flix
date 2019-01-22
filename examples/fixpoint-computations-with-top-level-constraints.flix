/// We can use Flix as an ordinary Datalog solver.

/// Declare two predicate symbols.
rel Edge(x: Int, y: Int)
rel Path(x: Int, y: Int)

/// Declare some edge facts.
Edge(1, 2).
Edge(2, 3).
Edge(2, 4).
Edge(3, 5).

// Declare some constraints.
Path(x, y) :- Edge(x, y).
Path(x, z) :- Path(x, y), Edge(y, z).

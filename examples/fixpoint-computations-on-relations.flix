///// We can use Flix as an ordinary Datalog solver.
//
///// Declare some edge facts.
//DirectedEdge(1, 2).
//DirectedEdge(2, 3).
//DirectedEdge(2, 4).
//DirectedEdge(3, 5).
//
//// Declare some constraints.
//Connected(x, y) :- DirectedEdge(x, y).
//Connected(x, z) :- Connected(x, y), DirectedEdge(y, z).

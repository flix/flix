namespace Cycles {

    rel Edge(src: Int, dst: Int);
    Edge(1, 2).
    Edge(2, 3).
    Edge(3, 1).
    Edge(1, 5).

    rel Reachable(src: Int, dst: Int);
    Reachable(x, y) :- Edge(x, y).
    Reachable(x, z) :- Reachable(x, y), Edge(y, z).

    rel Cyclic(vertex: Int);
    Cyclic(x) :- Reachable(x, x).

    // TODO: add assertion
    // assert Cylic(1).
    // refute Cyclic(5).

}
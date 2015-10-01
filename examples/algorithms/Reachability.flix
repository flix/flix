namespace Reachability {

    rel Edge(src: Int, dst: Int);
    Edge(1, 2).
    Edge(2, 3).
    Edge(3, 1).
    Edge(1, 5).
    Edge(3, 5).
    Edge(7, 8).
    Edge(8, 7).

    rel Reachable(src: Int, dst: Int);
    Reachable(x, y) :- Edge(x, y).
    Reachable(x, z) :- Reachable(x, y), Edge(y, z).

    // TODO: add assertion
    // assert Reachable(5, 7).
    // assert Reachable(1, y), Reachable(y, 3).
    // refute Reachable(1, 9).

};
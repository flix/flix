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
    // assert Reachable(5, 7). // there must be a path from 5 to 7.

    // assert false <= Reachable(5, 7). // there can never be path from 5 to 7.
    // assert Err <= Reachable(5, 7). // there can never be path from 5 to 7.

    // assert Reachable(x, y) <= Reachable(x, y), Reachable(y, z). // for every x, y and y z that are connected there must be a x,z reachable.
    // assert false <= Reachable(x, y), Reachable(y, z). // for any x,y connected and y,z connected, there can never be a x, z reachable.

    // assert Err(x, y) <= Reachable(x, y), Reachable(y, z). // for any x,y connected and y,z connected, there can never be a x, z reachable.


};
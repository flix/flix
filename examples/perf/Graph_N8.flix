namespace Graph_N8 {

    // N = 8, P = 0.125

    rel Edge(x: Int, y: Int);
    Edge(y, x) :- Edge(x, y).
    Edge(x, z) :- Edge(x, y), Edge(y, z).

    Edge(0, 0).
    Edge(0, 3).
    Edge(0, 5).
    Edge(1, 1).
    Edge(5, 3).
    Edge(6, 6).

}
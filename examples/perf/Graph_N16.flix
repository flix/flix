namespace Graph_N16 {

    // N = 16, P = 0.0625

    rel Edge(x: Int, y: Int);
    Edge(y, x) :- Edge(x, y).
    Edge(x, z) :- Edge(x, y), Edge(y, z).

    Edge(0, 4).
    Edge(0, 7).
    Edge(3, 13).
    Edge(4, 15).
    Edge(6, 1).
    Edge(7, 0).
    Edge(7, 13).
    Edge(7, 14).
    Edge(8, 4).
    Edge(8, 5).
    Edge(9, 8).
    Edge(9, 9).
    Edge(10, 6).
    Edge(10, 7).
    Edge(12, 2).
    Edge(13, 2).
    Edge(13, 10).
    Edge(13, 11).
    Edge(14, 2).
    Edge(15, 13).

}
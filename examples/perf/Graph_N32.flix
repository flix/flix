namespace Graph_N32 {

    // N = 32, P = 0.03125

    rel Edge(x: Int, y: Int);
    Edge(y, x) :- Edge(x, y).
    Edge(x, z) :- Edge(x, y), Edge(y, z).

    Edge(1, 20).
    Edge(1, 21).
    Edge(1, 29).
    Edge(2, 7).
    Edge(2, 11).
    Edge(3, 14).
    Edge(3, 24).
    Edge(3, 28).
    Edge(4, 10).
    Edge(4, 21).
    Edge(5, 28).
    Edge(7, 11).
    Edge(8, 22).
    Edge(8, 27).
    Edge(10, 6).
    Edge(10, 17).
    Edge(13, 3).
    Edge(13, 16).
    Edge(14, 10).
    Edge(14, 29).
    Edge(14, 30).
    Edge(15, 7).
    Edge(16, 25).
    Edge(16, 29).
    Edge(18, 10).
    Edge(19, 1).
    Edge(19, 14).
    Edge(19, 16).
    Edge(19, 24).
    Edge(21, 13).
    Edge(21, 27).
    Edge(22, 11).
    Edge(22, 24).
    Edge(24, 27).
    Edge(25, 16).
    Edge(25, 20).
    Edge(25, 21).
    Edge(27, 28).
    Edge(28, 16).
    Edge(29, 18).
    Edge(31, 10).

}
namespace Graph_N8 {

    // N = 8, P = 0.125

    rel Edge(x: Int, y: Int);
    rel Reach(x: Int, y: Int);

    index Edge({x}, {x, y});
    index Reach({y}, {x, y});

    Reach(x, y) :- Edge(x, y).
    Reach(x, y) :- Edge(y, x).
    Reach(x, z) :- Reach(x, y), Edge(y, z).


    Edge(0, 0).
    Edge(0, 3).
    Edge(0, 5).
    Edge(1, 1).
    Edge(5, 3).
    Edge(6, 6).

}
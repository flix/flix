namespace Graph_N64 {

    // N = 64, P = 0.015625

    rel Edge(x: Int, y: Int);
    Edge(y, x) :- Edge(x, y).
    Edge(x, z) :- Edge(x, y), Edge(y, z).

    Edge(0, 0).
    Edge(2, 8).
    Edge(5, 13).
    Edge(5, 62).
    Edge(9, 16).
    Edge(9, 42).
    Edge(10, 3).
    Edge(10, 44).
    Edge(11, 22).
    Edge(11, 23).
    Edge(11, 37).
    Edge(11, 60).
    Edge(13, 29).
    Edge(14, 63).
    Edge(17, 3).
    Edge(17, 29).
    Edge(18, 7).
    Edge(18, 60).
    Edge(19, 14).
    Edge(20, 36).
    Edge(20, 43).
    Edge(22, 1).
    Edge(22, 51).
    Edge(23, 50).
    Edge(24, 29).
    Edge(25, 56).
    Edge(26, 42).
    Edge(26, 51).
    Edge(27, 26).
    Edge(29, 3).
    Edge(29, 9).
    Edge(32, 29).
    Edge(37, 15).
    Edge(39, 13).
    Edge(40, 13).
    Edge(40, 36).
    Edge(41, 58).
    Edge(42, 48).
    Edge(43, 28).
    Edge(44, 31).
    Edge(45, 4).
    Edge(45, 57).
    Edge(47, 49).
    Edge(48, 62).
    Edge(49, 36).
    Edge(49, 46).
    Edge(50, 55).
    Edge(51, 43).
    Edge(52, 32).
    Edge(52, 41).
    Edge(53, 10).
    Edge(54, 8).
    Edge(54, 26).
    Edge(55, 8).
    Edge(56, 57).
    Edge(58, 11).
    Edge(58, 53).
    Edge(59, 16).
    Edge(59, 47).
    Edge(60, 13).
    Edge(62, 36).
    Edge(63, 1).
    Edge(63, 26).
    Edge(63, 45).
    Edge(63, 62).

}
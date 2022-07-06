namespace TestGraph {


    def graph01(): Set[(Int32, Int32)] =
        let _graphString =
            " ┌─┐   ┌─┐ " ::
            " │1├─→─┤2│ " ::
            " └─┘   └─┘ " ::
            Nil;
        Set#{(1, 2)}

    def graph02(): Set[(Int32, Int32)] =
        let _graphString =
            " ┌─┐   ┌─┐ " ::
            " │1├─→─┤2│ " ::
            " └─┘   └─┘ " ::
            " ┌─┐   ┌─┐ " ::
            " │3├─→─┤4│ " ::
            " └─┘   └─┘ " ::
            Nil;
        Set#{(1, 2), (3, 4)}

    def graph03(): Set[(Int32, Int32)] =
        let _graphString =
            " ┌─┐   ┌─┐ " ::
            " │1├─→─┤2│ " ::
            " └─┘   └┬┘ " ::
            "        ↓  " ::
            "       ┌┴┐ " ::
            "       │3│ " ::
            "       └─┘ " ::
            Nil;
        Set#{(1, 2), (2, 3)}

    def graph04(): Set[(Int32, Int32)] =
        let _graphString =
            " ┌─┐   ┌─┐ " ::
            " │1├─→─┤2│ " ::
            " └─┘   └┬┘ " ::
            "        ↓  " ::
            " ┌─┐   ┌┴┐ " ::
            " │4├─←─┤3│ " ::
            " └─┘   └─┘ " ::
            Nil;
        Set#{(1, 2), (2, 3), (3, 4)}



    /////////////////////////////////////////////////////////////////////////////
    // closure                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def closure01(): Bool =
        let s1 = graph01();
        let s2 = Set#{(1, 1), (1, 2), (2, 2)};
        Graph.closure(s1) == s2

    @test
    def closure02(): Bool =
        let s1 = graph02();
        let s2 = Set#{(1, 1), (1, 2), (2, 2), (3, 3), (3, 4), (4, 4)};
        Graph.closure(s1) == s2

    @test
    def closure03(): Bool =
        let s1 = graph03();
        let s2 = Set#{(1, 1), (1, 2), (1, 3), (2, 2), (2, 3), (3, 3)};
        Graph.closure(s1) == s2

    @test
    def closure04(): Bool =
        let s1 = graph04();
        let s2 = Set#{(1, 1), (1, 2), (1, 3), (1, 4),
                      (2, 2), (2, 3), (2, 4),
                      (3, 3), (3, 4),
                      (4, 4)};
        Graph.closure(s1) == s2


    /////////////////////////////////////////////////////////////////////////////
    // reachable                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reachable01(): Bool =
        let s1 = graph01();
        let s2 = Set#{1, 2};
        Graph.reachable(1, s1) == s2

    @test
    def reachable02(): Bool =
        let s1 = graph02();
        let s2 = Set#{1, 2};
        Graph.reachable(1, s1) == s2

    @test
    def reachable03(): Bool =
        let s1 = graph03();
        let s2 = Set#{1, 2, 3};
        Graph.reachable(1, s1) == s2

    @test
    def reachable04(): Bool =
        let s1 = graph04();
        let s2 = Set#{1, 2, 3, 4};
        Graph.reachable(1, s1) == s2

    @test
    def reachable05(): Bool =
        let s1 = graph04();
        let s2 = Set#{3, 4};
        Graph.reachable(3, s1) == s2

    /////////////////////////////////////////////////////////////////////////////
    // stronglyConnectedComponents01                                           //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def stronglyConnectedComponents01(): Bool =
        let g = graph01();
        let s = Nil;
        Graph.stronglyConnectedComponents(g) == s

    @test
    def stronglyConnectedComponents02(): Bool =
        let g = graph02();
        let s = Set#{2} :: Set#{1} :: Nil;
        Graph.stronglyConnectedComponents(g) == s

    @test
    def stronglyConnectedComponents03(): Bool =
        let g = graph03();
        let s = Set#{4} :: Set#{3} :: Set#{2} :: Set#{1} :: Nil;
        Graph.stronglyConnectedComponents(g) == s

    @test
    def stronglyConnectedComponents04(): Bool =
        let g = graph04();
        let s = Set#{3} :: Set#{2} :: Set#{1} :: Nil;
        Graph.stronglyConnectedComponents(g) == s

    @test
    def stronglyConnectedComponents05(): Bool =
        let g = graph05();
        let s = Set#{4} :: Set#{3} :: Set#{2} :: Set#{1} :: Nil;
        Graph.stronglyConnectedComponents(g) == s

    @test
    def stronglyConnectedComponents06(): Bool =
        let g = graph06();
        let s = Set#{1, 2, 3, 4} :: Nil;
        Graph.stronglyConnectedComponents(g) == s

    @test
    def stronglyConnectedComponents07(): Bool =
        let g = graph07();
        let s = Set#{5, 6} :: Set#{1, 2, 3, 4} :: Nil;
        Graph.stronglyConnectedComponents(g) == s

}

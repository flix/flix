namespace TestGraph {

    def graph01(): Set[(Int32, Int32)] =
        Set#{}

    def graph02(): Set[(Int32, Int32)] =
        let _graphString =
            " ┌─┐   ┌─┐ " ::
            " │1├─→─┤2│ " ::
            " └─┘   └─┘ " ::
            Nil;
        Set#{(1, 2)}

    def graph03(): Set[(Int32, Int32)] =
        let _graphString =
            " ┌─┐   ┌─┐ " ::
            " │1├─→─┤2│ " ::
            " └─┘   └─┘ " ::
            " ┌─┐   ┌─┐ " ::
            " │3├─→─┤4│ " ::
            " └─┘   └─┘ " ::
            Nil;
        Set#{(1, 2), (3, 4)}

    def graph04(): Set[(Int32, Int32)] =
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

    def graph05(): Set[(Int32, Int32)] =
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

    def graph06(): Set[(Int32, Int32)] =
        let _graphString =
            " ┌─┐   ┌─┐ " ::
            " │1├─→─┤2│ " ::
            " └┬┘   └┬┘ " ::
            "  ↑     ↓  " ::
            " ┌┴┐   ┌┴┐ " ::
            " │4├─←─┤3│ " ::
            " └─┘   └─┘ " ::
            Nil;
        Set#{(1, 2), (2, 3), (3, 4), (4, 1)}

    def graph07(): Set[(Int32, Int32)] =
        let _graphString =
            " ┌─┐   ┌─┐   ┌─┐ " ::
            " │1├─→─┤2├─→─┤5│ " ::
            " └┬┘   └┬┘   └┬┘ " ::
            "  ↑     ↓     ↕  " ::
            " ┌┴┐   ┌┴┐   ┌┴┐ " ::
            " │4├─←─┤3│   │6│ " ::
            " └─┘   └─┘   └─┘ " ::
            Nil;
        Set#{(1, 2), (2, 3), (3, 4), (4, 1), (5, 6), (6, 5)}


    /////////////////////////////////////////////////////////////////////////////
    // closure                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def closure01(): Bool =
        let g = graph01();
        let s = Set#{};
        Graph.closure(g) == s

    @test
    def closure02(): Bool =
        let g = graph02();
        let s = Set#{(1, 1), (1, 2), (2, 2)};
        Graph.closure(g) == s

    @test
    def closure03(): Bool =
        let g = graph03();
        let s = Set#{(1, 1), (1, 2), (2, 2), (3, 3), (3, 4), (4, 4)};
        Graph.closure(g) == s

    @test
    def closure04(): Bool =
        let g = graph04();
        let s = Set#{(1, 1), (1, 2), (1, 3), (2, 2), (2, 3), (3, 3)};
        Graph.closure(g) == s

    @test
    def closure05(): Bool =
        let g = graph05();
        let s = Set#{(1, 1), (1, 2), (1, 3), (1, 4),
                      (2, 2), (2, 3), (2, 4),
                      (3, 3), (3, 4),
                      (4, 4)};
        Graph.closure(g) == s


    /////////////////////////////////////////////////////////////////////////////
    // reachable                                                               //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def reachable01(): Bool =
        let g = graph01();
        Graph.reachable(1, g) == Set#{1}

    @test
    def reachable02(): Bool =
        let g = graph02();
        Graph.reachable(1, g) == Set#{1, 2}

    @test
    def reachable03(): Bool =
        let g = graph03();
        Graph.reachable(1, g) == Set#{1, 2}

    @test
    def reachable04(): Bool =
        let g = graph04();
        Graph.reachable(1, g) == Set#{1, 2, 3}

    @test
    def reachable05(): Bool =
        let g = graph05();
        Graph.reachable(1, g) == Set#{1, 2, 3, 4}

    @test
    def reachable06(): Bool =
        let g = graph05();
        Graph.reachable(3, g) == Set#{3, 4}

    @test
    def reachable07(): Bool =
        let g = graph05();
        Graph.reachable(5, g) == Set#{5}


    /////////////////////////////////////////////////////////////////////////////
    // unreachable                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def unreachable01(): Bool =
        let g = graph01();
        Graph.unreachable(1, g) == Set#{}

    @test
    def unreachable02(): Bool =
        let g = graph02();
        Graph.unreachable(1, g) == Set#{}

    @test
    def unreachable03(): Bool =
        let g = graph02();
        Graph.unreachable(2, g) == Set#{1}

    @test
    def unreachable04(): Bool =
        let g = graph03();
        Graph.unreachable(1, g) == Set#{3, 4}

    @test
    def unreachable05(): Bool =
        let g = graph04();
        Graph.unreachable(2, g) == Set#{1}

    @test
    def unreachable06(): Bool =
        let g = graph05();
        Graph.unreachable(3, g) == Set#{1, 2}

    @test
    def unreachable07(): Bool =
        let g = graph05();
        Graph.unreachable(5, g) == Set#{1, 2, 3, 4}


    /////////////////////////////////////////////////////////////////////////////
    // isConnected                                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def isConnected01(): Bool =
        let g = graph01();
        Graph.isConnected(src = 1, dst = 2, g) == false

    @test
    def isConnected02(): Bool =
        let g = graph02();
        Graph.isConnected(src = 1, dst = 2, g)

    @test
    def isConnected03(): Bool =
        let g = graph02();
        Graph.isConnected(src = 2, dst = 1, g) == false

    @test
    def isConnected04(): Bool =
        let g = graph03();
        Graph.isConnected(src = 1, dst = 3, g) == false

    @test
    def isConnected05(): Bool =
        let g = graph04();
        Graph.isConnected(src = 1, dst = 3, g)

    @test
    def isConnected06(): Bool =
        let g = graph05();
        Graph.isConnected(src = 2, dst = 4, g)

    @test
    def isConnected07(): Bool =
        let g = graph05();
        Graph.isConnected(src = 4, dst = 1, g) == false

    @test
    def isConnected08(): Bool =
        let g = graph05();
        Graph.isConnected(src = 5, dst = 6, g) == false


    /////////////////////////////////////////////////////////////////////////////
    // stronglyConnectedComponents                                             //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def stronglyConnectedComponents01(): Bool =
        let g = graph01();
        let s = Set#{};
        Graph.stronglyConnectedComponents(g) == s

    @test
    def stronglyConnectedComponents02(): Bool =
        let g = graph02();
        let s = Set#{Set#{1}, Set#{2}};
        Graph.stronglyConnectedComponents(g) == s

    @test
    def stronglyConnectedComponents03(): Bool =
        let g = graph03();
        let s = Set#{Set#{1}, Set#{2}, Set#{3}, Set#{4}};
        Graph.stronglyConnectedComponents(g) == s

    @test
    def stronglyConnectedComponents04(): Bool =
        let g = graph04();
        let s = Set#{Set#{1}, Set#{2}, Set#{3}};
        Graph.stronglyConnectedComponents(g) == s

    @test
    def stronglyConnectedComponents05(): Bool =
        let g = graph05();
        let s = Set#{Set#{1}, Set#{2}, Set#{3}, Set#{4}};
        Graph.stronglyConnectedComponents(g) == s

    @test
    def stronglyConnectedComponents06(): Bool =
        let g = graph06();
        let s = Set#{Set#{1, 2, 3, 4}};
        Graph.stronglyConnectedComponents(g) == s

    @test
    def stronglyConnectedComponents07(): Bool =
        let g = graph07();
        let s = Set#{Set#{1, 2, 3, 4}, Set#{5, 6}};
        Graph.stronglyConnectedComponents(g) == s

}

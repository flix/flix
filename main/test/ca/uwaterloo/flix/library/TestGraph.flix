namespace TestGraph {


    /////////////////////////////////////////////////////////////////////////////
    // closure                                                                 //
    /////////////////////////////////////////////////////////////////////////////

    @test
    def closure01(): Bool =
        let s1 = Set#{(1, 2)};
        let _graphString =
            " ┌─┐   ┌─┐ " ::
            " │1├─>─┤2│ " ::
            " └─┘   └─┘ " ::
            Nil;
        let s2 = Set#{(1, 1), (1, 2), (2, 2)};
        Graph.closure(s1) == s2

    @test
    def closure02(): Bool =
        let s1 = Set#{(1, 2), (3, 4)};
        let _graphString =
            " ┌─┐   ┌─┐ " ::
            " │1├─→─┤2│ " ::
            " └─┘   └─┘ " ::
            " ┌─┐   ┌─┐ " ::
            " │3├─→─┤4│ " ::
            " └─┘   └─┘ " ::
            Nil;
        let s2 = Set#{(1, 1), (1, 2), (2, 2), (3, 3), (3, 4), (4, 4)};
        Graph.closure(s1) == s2

    @test
    def closure03(): Bool =
        let s1 = Set#{(1, 2), (2, 3)};
        let _graphString =
            " ┌─┐   ┌─┐ " ::
            " │1├─→─┤2│ " ::
            " └─┘   └┬┘ " ::
            "        ↓  " ::
            "       ┌┴┐ " ::
            "       │3│ " ::
            "       └─┘ " ::
            Nil;
        let s2 = Set#{(1, 1), (1, 2), (1, 3), (2, 2), (2, 3), (3, 3)};
        Graph.closure(s1) == s2

    @test
    def closure04(): Bool =
        let s1 = Set#{(1, 2), (2, 3), (3, 4)};
        let _graphString =
            " ┌─┐   ┌─┐ " ::
            " │1├─→─┤2│ " ::
            " └─┘   └┬┘ " ::
            "        ↓  " ::
            " ┌─┐   ┌┴┐ " ::
            " │4├─←─┤3│ " ::
            " └─┘   └─┘ " ::
            Nil;
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
        let s1 = Set#{(1, 2)};
        let _graphString =
            " ┌─┐   ┌─┐ " ::
            " │1├─>─┤2│ " ::
            " └─┘   └─┘ " ::
            Nil;
        let s2 = Set#{1, 2};
        Graph.reachable(1, s1) == s2

    @test
    def reachable02(): Bool =
        let s1 = Set#{(1, 2), (3, 4)};
        let _graphString =
            " ┌─┐   ┌─┐ " ::
            " │1├─→─┤2│ " ::
            " └─┘   └─┘ " ::
            " ┌─┐   ┌─┐ " ::
            " │3├─→─┤4│ " ::
            " └─┘   └─┘ " ::
            Nil;
        let s2 = Set#{1, 2};
        Graph.reachable(1, s1) == s2

    @test
    def reachable03(): Bool =
        let s1 = Set#{(1, 2), (2, 3)};
        let _graphString =
            " ┌─┐   ┌─┐ " ::
            " │1├─→─┤2│ " ::
            " └─┘   └┬┘ " ::
            "        ↓  " ::
            "       ┌┴┐ " ::
            "       │3│ " ::
            "       └─┘ " ::
            Nil;
        let s2 = Set#{1, 2, 3};
        Graph.reachable(1, s1) == s2

    @test
    def reachable04(): Bool =
        let s1 = Set#{(1, 2), (2, 3), (3, 4)};
        let _graphString =
            " ┌─┐   ┌─┐ " ::
            " │1├─→─┤2│ " ::
            " └─┘   └┬┘ " ::
            "        ↓  " ::
            " ┌─┐   ┌┴┐ " ::
            " │4├─←─┤3│ " ::
            " └─┘   └─┘ " ::
            Nil;
        let s2 = Set#{1, 2, 3, 4};
        Graph.reachable(1, s1) == s2

    @test
    def reachable05(): Bool =
        let s1 = Set#{(1, 2), (2, 3), (3, 4)};
        let _graphString =
            " ┌─┐   ┌─┐ " ::
            " │1├─→─┤2│ " ::
            " └─┘   └┬┘ " ::
            "        ↓  " ::
            " ┌─┐   ┌┴┐ " ::
            " │4├─←─┤3│ " ::
            " └─┘   └─┘ " ::
            Nil;
        let s2 = Set#{3, 4};
        Graph.reachable(3, s1) == s2


}

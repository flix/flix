mod TestGraph {

    use Assert.{assertEq, assertTrue, assertFalse, assertSome, assertNone};

    ////////////////////////////////////////////////////////////////////////////
    // directed graphs                                                        //
    ////////////////////////////////////////////////////////////////////////////

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
        Set#{(1, 2), (2, 3), (2, 5), (3, 4), (4, 1), (5, 6), (6, 5)}


    ////////////////////////////////////////////////////////////////////////////
    // weighted directed graph                                                //
    ////////////////////////////////////////////////////////////////////////////

    def graphWithDist01(): Set[(Int32, Int32, Int32)] =
        Set#{}

    def graphWithDist02(): Set[(Int32, Int32, Int32)] =
        let _graphString =
            " ┌─┐    ┌─┐ " ::
            " │1├─4→─┤2│ " ::
            " └─┘    └─┘ " ::
            Nil;
        Set#{(1, 4, 2)}

    def graphWithDist03(): Set[(Int32, Int32, Int32)] =
        let _graphString =
            " ┌─┐    ┌─┐ " ::
            " │1├─4→─┤2│ " ::
            " └─┘    └─┘ " ::
            " ┌─┐    ┌─┐ " ::
            " │3├─7→─┤4│ " ::
            " └─┘    └─┘ " ::
            Nil;
        Set#{(1, 4, 2), (3, 7, 4)}

    def graphWithDist04(): Set[(Int32, Int32, Int32)] =
        let _graphString =
            " ┌─┐    ┌─┐ " ::
            " │1├─4→─┤2│ " ::
            " └─┘    └┬┘ " ::
            "         5  " ::
            "         ↓  " ::
            " ┌─┐    ┌┴┐ " ::
            " │4├─←7─┤3│ " ::
            " └─┘    └─┘ " ::
            Nil;
        Set#{(1, 4, 2), (2, 5, 3), (3, 7, 4)}

    def graphWithDist05(): Set[(Int32, Int32, Int32)] =
        let _graphString =
            " ┌─┐    ┌─┐ " ::
            " │1├─4→─┤2│ " ::
            " └┬┘    └┬┘ " ::
            "  ↑      2  " ::
            "  5      ↓  " ::
            " ┌┴┐    ┌┴┐ " ::
            " │4├─←1─┤3│ " ::
            " └─┘    └─┘ " ::
            Nil;
        Set#{(1, 4, 2), (2, 2, 3), (3, 1, 4), (4, 5, 1)}

    def graphWithDist06(): Set[(Int32, Int32, Int32)] =
        let _graphString =
            " ┌─┐               " ::
            " │1├─5→──┐         " ::
            " └┬┘     │         " ::
            "  2      │         " ::
            "  ↓      ↓         " ::
            " ┌┴┐    ┌┴┐    ┌─┐ " ::
            " │2├─←7─┤4├─2→─┤5│ " ::
            " └┬┘    └┬┘    └┬┘ " ::
            "  10     4      1  " ::
            "  ↓      ↓      ↓  " ::
            " ┌┴┐    ┌┴┐     │  " ::
            " │3│    │6├─←───┘  " ::
            " └─┘    └─┘        " ::
            Nil;
        Set#{
            (1, 2, 2), (1, 5, 4),
            (2, 10, 3), (4, 7, 2),
            (4, 2, 5), (4, 4, 6),
            (5, 1, 6)
        }


    ////////////////////////////////////////////////////////////////////////////
    // closure                                                                //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def closure01(): Unit \ Assert = {
        let g = graph01();
        let s = Set#{};
        assertEq(expected = s, Graph.closure(g))
    }

    @Test
    def closure02(): Unit \ Assert = {
        let g = graph02();
        let s = Set#{(1, 1), (1, 2), (2, 2)};
        assertEq(expected = s, Graph.closure(g))
    }

    @Test
    def closure03(): Unit \ Assert = {
        let g = graph03();
        let s = Set#{(1, 1), (1, 2), (2, 2), (3, 3), (3, 4), (4, 4)};
        assertEq(expected = s, Graph.closure(g))
    }

    @Test
    def closure04(): Unit \ Assert = {
        let g = graph04();
        let s = Set#{(1, 1), (1, 2), (1, 3), (2, 2), (2, 3), (3, 3)};
        assertEq(expected = s, Graph.closure(g))
    }

    @Test
    def closure05(): Unit \ Assert = {
        let g = graph05();
        let s = Set#{(1, 1), (1, 2), (1, 3), (1, 4),
                     (2, 2), (2, 3), (2, 4),
                     (3, 3), (3, 4),
                     (4, 4)};
        assertEq(expected = s, Graph.closure(g))
    }


    ////////////////////////////////////////////////////////////////////////////
    // reachableFrom                                                          //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def reachable01(): Unit \ Assert = {
        let g = graph01();
        assertEq(expected = Set#{1}, Graph.reachableFrom(1, g))
    }

    @Test
    def reachable02(): Unit \ Assert = {
        let g = graph02();
        assertEq(expected = Set#{1, 2}, Graph.reachableFrom(1, g))
    }

    @Test
    def reachable03(): Unit \ Assert = {
        let g = graph03();
        assertEq(expected = Set#{1, 2}, Graph.reachableFrom(1, g))
    }

    @Test
    def reachable04(): Unit \ Assert = {
        let g = graph04();
        assertEq(expected = Set#{1, 2, 3}, Graph.reachableFrom(1, g))
    }

    @Test
    def reachable05(): Unit \ Assert = {
        let g = graph05();
        assertEq(expected = Set#{1, 2, 3, 4}, Graph.reachableFrom(1, g))
    }

    @Test
    def reachable06(): Unit \ Assert = {
        let g = graph05();
        assertEq(expected = Set#{3, 4}, Graph.reachableFrom(3, g))
    }

    @Test
    def reachable07(): Unit \ Assert = {
        let g = graph05();
        assertEq(expected = Set#{5}, Graph.reachableFrom(5, g))
    }


    ////////////////////////////////////////////////////////////////////////////
    // unreachableFrom                                                        //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def unreachable01(): Unit \ Assert = {
        let g = graph01();
        assertEq(expected = Set#{}, Graph.unreachableFrom(1, g))
    }

    @Test
    def unreachable02(): Unit \ Assert = {
        let g = graph02();
        assertEq(expected = Set#{}, Graph.unreachableFrom(1, g))
    }

    @Test
    def unreachable03(): Unit \ Assert = {
        let g = graph02();
        assertEq(expected = Set#{1}, Graph.unreachableFrom(2, g))
    }

    @Test
    def unreachable04(): Unit \ Assert = {
        let g = graph03();
        assertEq(expected = Set#{3, 4}, Graph.unreachableFrom(1, g))
    }

    @Test
    def unreachable05(): Unit \ Assert = {
        let g = graph04();
        assertEq(expected = Set#{1}, Graph.unreachableFrom(2, g))
    }

    @Test
    def unreachable06(): Unit \ Assert = {
        let g = graph05();
        assertEq(expected = Set#{1, 2}, Graph.unreachableFrom(3, g))
    }

    @Test
    def unreachable07(): Unit \ Assert = {
        let g = graph05();
        assertEq(expected = Set#{1, 2, 3, 4}, Graph.unreachableFrom(5, g))
    }


    ////////////////////////////////////////////////////////////////////////////
    // reachable                                                              //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def isConnected01(): Unit \ Assert = {
        let g = graph01();
        assertFalse(Graph.reachable(src = 1, dst = 2, g))
    }

    @Test
    def isConnected02(): Unit \ Assert = {
        let g = graph02();
        assertTrue(Graph.reachable(src = 1, dst = 2, g))
    }

    @Test
    def isConnected03(): Unit \ Assert = {
        let g = graph02();
        assertFalse(Graph.reachable(src = 2, dst = 1, g))
    }

    @Test
    def isConnected04(): Unit \ Assert = {
        let g = graph03();
        assertFalse(Graph.reachable(src = 1, dst = 3, g))
    }

    @Test
    def isConnected05(): Unit \ Assert = {
        let g = graph04();
        assertTrue(Graph.reachable(src = 1, dst = 3, g))
    }

    @Test
    def isConnected06(): Unit \ Assert = {
        let g = graph05();
        assertTrue(Graph.reachable(src = 2, dst = 4, g))
    }

    @Test
    def isConnected07(): Unit \ Assert = {
        let g = graph05();
        assertFalse(Graph.reachable(src = 4, dst = 1, g))
    }

    @Test
    def isConnected08(): Unit \ Assert = {
        let g = graph05();
        assertFalse(Graph.reachable(src = 5, dst = 6, g))
    }


    ////////////////////////////////////////////////////////////////////////////
    // stronglyConnectedComponents                                            //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def stronglyConnectedComponents01(): Unit \ Assert = {
        let g = graph01();
        let s = Set#{};
        assertEq(expected = s, Graph.stronglyConnectedComponents(g))
    }

    @Test
    def stronglyConnectedComponents02(): Unit \ Assert = {
        let g = graph02();
        let s = Set#{Set#{1}, Set#{2}};
        assertEq(expected = s, Graph.stronglyConnectedComponents(g))
    }

    @Test
    def stronglyConnectedComponents03(): Unit \ Assert = {
        let g = graph03();
        let s = Set#{Set#{1}, Set#{2}, Set#{3}, Set#{4}};
        assertEq(expected = s, Graph.stronglyConnectedComponents(g))
    }

    @Test
    def stronglyConnectedComponents04(): Unit \ Assert = {
        let g = graph04();
        let s = Set#{Set#{1}, Set#{2}, Set#{3}};
        assertEq(expected = s, Graph.stronglyConnectedComponents(g))
    }

    @Test
    def stronglyConnectedComponents05(): Unit \ Assert = {
        let g = graph05();
        let s = Set#{Set#{1}, Set#{2}, Set#{3}, Set#{4}};
        assertEq(expected = s, Graph.stronglyConnectedComponents(g))
    }

    @Test
    def stronglyConnectedComponents06(): Unit \ Assert = {
        let g = graph06();
        let s = Set#{Set#{1, 2, 3, 4}};
        assertEq(expected = s, Graph.stronglyConnectedComponents(g))
    }

    @Test
    def stronglyConnectedComponents07(): Unit \ Assert = {
        let g = graph07();
        let s = Set#{Set#{1, 2, 3, 4}, Set#{5, 6}};
        assertEq(expected = s, Graph.stronglyConnectedComponents(g))
    }

    ////////////////////////////////////////////////////////////////////////////
    // isCyclic                                                               //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def isCyclic01(): Unit \ Assert = {
        let g = graph01();
        assertFalse(Graph.isCyclic(g))
    }

    @Test
    def isCyclic02(): Unit \ Assert = {
        let g = graph02();
        assertFalse(Graph.isCyclic(g))
    }

    @Test
    def isCyclic03(): Unit \ Assert = {
        let g = graph03();
        assertFalse(Graph.isCyclic(g))
    }

    @Test
    def isCyclic04(): Unit \ Assert = {
        let g = graph04();
        assertFalse(Graph.isCyclic(g))
    }

    @Test
    def isCyclic05(): Unit \ Assert = {
        let g = graph05();
        assertFalse(Graph.isCyclic(g))
    }

    @Test
    def isCyclic06(): Unit \ Assert = {
        let g = graph06();
        assertTrue(Graph.isCyclic(g))
    }

    @Test
    def isCyclic07(): Unit \ Assert = {
        let g = graph07();
        assertTrue(Graph.isCyclic(g))
    }

    ////////////////////////////////////////////////////////////////////////////
    // boundedDistances                                                       //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def distances01(): Unit \ Assert = {
        let g = graphWithDist01();
        assertEq(expected = Map.empty(), Graph.boundedDistances(g))
    }

    @Test
    def distances02(): Unit \ Assert = {
        let g = graphWithDist02();
        assertEq(expected = Map#{
            (1, 1) => 0,
            (2, 2) => 0,

            (1, 2) => 4
        }, Graph.boundedDistances(g))
    }

    @Test
    def distances03(): Unit \ Assert = {
        let g = graphWithDist03();
        assertEq(expected = Map#{
            (1, 1) => 0,
            (2, 2) => 0,
            (3, 3) => 0,
            (4, 4) => 0,

            (1, 2) => 4,
            (3, 4) => 7
        }, Graph.boundedDistances(g))
    }

    @Test
    def distances04(): Unit \ Assert = {
        let g = graphWithDist04();
        assertEq(expected = Map#{
            (1, 1) => 0,
            (2, 2) => 0,
            (3, 3) => 0,
            (4, 4) => 0,

            (1, 2) => 4,
            (1, 3) => 4+5,
            (1, 4) => 4+5+7,
            (2, 3) => 5,
            (2, 4) => 5+7,
            (3, 4) => 7
        }, Graph.boundedDistances(g))
    }

    @Test
    def distances05(): Unit \ Assert = {
        let g = graphWithDist05();
        assertEq(expected = Map#{
            (1, 1) => 0,
            (2, 2) => 0,
            (3, 3) => 0,
            (4, 4) => 0,

            (1, 2) => 4,
            (1, 3) => 4+2,
            (1, 4) => 4+2+1,
            (2, 1) => 2+1+5,
            (2, 3) => 2,
            (2, 4) => 2+1,
            (3, 1) => 1+5,
            (3, 2) => 1+5+4,
            (3, 4) => 1,
            (4, 1) => 5,
            (4, 2) => 5+4,
            (4, 3) => 5+4+2
        }, Graph.boundedDistances(g))
    }

    @Test
    def distances06(): Unit \ Assert = {
        let g = graphWithDist06();
        assertEq(expected = Map#{
            (1, 1) => 0,
            (2, 2) => 0,
            (3, 3) => 0,
            (4, 4) => 0,
            (5, 5) => 0,
            (6, 6) => 0,

            (1, 2) => 2,
            (1, 3) => 2+10,
            (1, 4) => 5,
            (1, 5) => 5+2,
            (1, 6) => 5+2+1,
            (2, 3) => 10,
            (4, 2) => 7,
            (4, 3) => 7+10,
            (4, 5) => 2,
            (4, 6) => 3,
            (5, 6) => 1
        }, Graph.boundedDistances(g))
    }

    ////////////////////////////////////////////////////////////////////////////
    // distancesFrom                                                          //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def distancesFrom01(): Unit \ Assert = {
        let g = graphWithDist01();
        assertEq(expected = Map#{1 => 0}, Graph.distancesFrom(1, g))
    }

    @Test
    def distancesFrom02(): Unit \ Assert = {
        let g = graphWithDist02();
        assertEq(expected = Map#{1 => 0, 2 => 4}, Graph.distancesFrom(1, g))
    }

    @Test
    def distancesFrom03(): Unit \ Assert = {
        let g = graphWithDist02();
        assertEq(expected = Map#{2 => 0}, Graph.distancesFrom(2, g))
    }

    @Test
    def distancesFrom04(): Unit \ Assert = {
        let g = graphWithDist03();
        assertEq(expected = Map#{3 => 0, 4 => 7}, Graph.distancesFrom(3, g))
    }

    @Test
    def distancesFrom05(): Unit \ Assert = {
        let g = graphWithDist04();
        assertEq(expected = Map#{2 => 0, 3 => 5, 4 => (5 + 7)}, Graph.distancesFrom(2, g))
    }

    @Test
    def distancesFrom06(): Unit \ Assert = {
        let g = graphWithDist05();
        assertEq(expected = Map#{
            1 => 5,
            2 => (5 + 4),
            3 => (5 + 4 + 2),
            4 => 0
        }, Graph.distancesFrom(4, g))
    }

    @Test
    def distancesFrom07(): Unit \ Assert = {
        let g = graphWithDist06();
        assertEq(expected = Map#{
            2 => 7,
            3 => (7 + 10),
            4 => 0,
            5 => 2,
            6 => 3
        }, Graph.distancesFrom(4, g))
    }


    ////////////////////////////////////////////////////////////////////////////
    // distance                                                               //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def distance01(): Unit \ Assert = {
        let g = graphWithDist01();
        assertEq(expected = None, Graph.distance(src = 1, dst = 2, g))
    }

    @Test
    def distance02(): Unit \ Assert = {
        let g = graphWithDist02();
        assertEq(expected = Some(4), Graph.distance(src = 1, dst = 2, g))
    }

    @Test
    def distance03(): Unit \ Assert = {
        let g = graphWithDist02();
        assertEq(expected = None, Graph.distance(src = 2, dst = 1, g))
    }

    @Test
    def distance04(): Unit \ Assert = {
        let g = graphWithDist03();
        assertEq(expected = Some(4), Graph.distance(src = 1, dst = 2, g))
    }

    @Test
    def distance05(): Unit \ Assert = {
        let g = graphWithDist03();
        assertEq(expected = Some(7), Graph.distance(src = 3, dst = 4, g))
    }

    @Test
    def distance06(): Unit \ Assert = {
        let g = graphWithDist03();
        assertEq(expected = None, Graph.distance(src = 1, dst = 4, g))
    }

    @Test
    def distance07(): Unit \ Assert = {
        let g = graphWithDist04();
        assertEq(expected = Some(4 + 5 + 7), Graph.distance(src = 1, dst = 4, g))
    }

    @Test
    def distance08(): Unit \ Assert = {
        let g = graphWithDist05();
        assertEq(expected = Some(5 + 4), Graph.distance(src = 4, dst = 2, g))
    }

    @Test
    def distance09(): Unit \ Assert = {
        let g = graphWithDist06();
        assertEq(expected = Some(5 + 2 + 1), Graph.distance(src = 1, dst = 6, g))
    }

    ////////////////////////////////////////////////////////////////////////////
    // toUndirected                                                           //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def toUndirected01(): Unit \ Assert = {
        let g = graph01() |> Graph.toUndirected;
        assertEq(expected = Set#{}, g)
    }

    @Test
    def toUndirected02(): Unit \ Assert = {
        let g = graph02() |> Graph.toUndirected;
        assertEq(expected = Set#{(1, 2), (2, 1)}, g)
    }

    @Test
    def toUndirected03(): Unit \ Assert = {
        let g = graph03() |> Graph.toUndirected;
        assertEq(expected = Set#{(1, 2), (2, 1), (3, 4), (4, 3)}, g)
    }

    @Test
    def toUndirected04(): Unit \ Assert = {
        let g = graph04() |> Graph.toUndirected;
        assertEq(expected = Set#{(1, 2), (2, 1), (2, 3), (3, 2)}, g)
    }

    @Test
    def toUndirected05(): Unit \ Assert = {
        let g = graph05() |> Graph.toUndirected;
        assertEq(expected = Set#{(1, 2), (2, 1), (2, 3), (3, 2), (3, 4), (4, 3)}, g)
    }

    @Test
    def toUndirected06(): Unit \ Assert = {
        let g = graph06() |> Graph.toUndirected;
        assertEq(expected = Set#{
            (1, 2), (2, 1),
            (2, 3), (3, 2),
            (3, 4), (4, 3),
            (4, 1), (1, 4)
        }, g)
    }

    @Test
    def toUndirected07(): Unit \ Assert = {
        let g = graph07() |> Graph.toUndirected;
        assertEq(expected = Set#{
            (1, 2), (2, 1),
            (1, 4), (4, 1),
            (2, 3), (3, 2),
            (2, 5), (5, 2),
            (3, 4), (4, 3),
            (5, 6), (6, 5)
        }, g)
    }

    ////////////////////////////////////////////////////////////////////////////
    // toUndirectedLabeled                                                   //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def toUndirectedLabeled01(): Unit \ Assert = {
        let g = graphWithDist01() |> Graph.toUndirectedLabeled;
        assertEq(expected = Set#{}, g)
    }

    @Test
    def toUndirectedLabeled02(): Unit \ Assert = {
        let g = graphWithDist02() |> Graph.toUndirectedLabeled;
        assertEq(expected = Set#{(1, 4, 2), (2, 4, 1)}, g)
    }

    @Test
    def toUndirectedLabeled03(): Unit \ Assert = {
        let g = graphWithDist03() |> Graph.toUndirectedLabeled;
        assertEq(expected = Set#{
            (1, 4, 2), (2, 4, 1),
            (3, 7, 4), (4, 7, 3)
        }, g)
    }

    @Test
    def toUndirectedLabeled04(): Unit \ Assert = {
        let g = graphWithDist04() |> Graph.toUndirectedLabeled;
        assertEq(expected = Set#{
            (1, 4, 2), (2, 4, 1),
            (2, 5, 3), (3, 5, 2),
            (3, 7, 4), (4, 7, 3)
        }, g)
    }

    @Test
    def toUndirectedLabeled05(): Unit \ Assert = {
        let g = graphWithDist05() |> Graph.toUndirectedLabeled;
        assertEq(expected = Set#{
            (1, 4, 2), (2, 4, 1),
            (1, 5, 4), (4, 5, 1),
            (2, 2, 3), (3, 2, 2),
            (3, 1, 4), (4, 1, 3)
        }, g)
    }

    @Test
    def toUndirectedLabeled06(): Unit \ Assert = {
        let g = graphWithDist06() |> Graph.toUndirectedLabeled;
        assertEq(expected = Set#{
            (1, 2, 2), (2, 2, 1),
            (1, 5, 4), (4, 5, 1),
            (2, 10, 3), (3, 10, 2),
            (2, 7, 4), (4, 7, 2),
            (4, 2, 5), (5, 2, 4),
            (4, 4, 6), (6, 4, 4),
            (5, 1, 6), (6, 1, 5)
        }, g)
    }

    @Test
    def toUndirectedLabeled07(): Unit \ Assert = {
        let g0 = Set#{(7, 9, 13), (13, 8, 7)};
        let g = g0 |> Graph.toUndirectedLabeled;
        assertEq(expected = Set#{
            (7, 9, 13), (13, 9, 7),
            (7, 8, 13), (13, 8, 7)
        }, g)
    }

    ////////////////////////////////////////////////////////////////////////////
    // inDegrees                                                              //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def inDegrees01(): Unit \ Assert = {
        assertEq(expected = Map#{}, Graph.inDegrees(graph01()))
    }

    @Test
    def inDegrees02(): Unit \ Assert = {
        assertEq(expected = Map#{
            1 => 0,
            2 => 1
        }, Graph.inDegrees(graph02()))
    }

    @Test
    def inDegrees03(): Unit \ Assert = {
        assertEq(expected = Map#{
            1 => 0,
            2 => 1,
            3 => 0,
            4 => 1
        }, Graph.inDegrees(graph03()))
    }

    @Test
    def inDegrees04(): Unit \ Assert = {
        assertEq(expected = Map#{
            1 => 0,
            2 => 1,
            3 => 1
        }, Graph.inDegrees(graph04()))
    }

    @Test
    def inDegrees05(): Unit \ Assert = {
        assertEq(expected = Map#{
            1 => 0,
            2 => 1,
            3 => 1,
            4 => 1
        }, Graph.inDegrees(graph05()))
    }

    @Test
    def inDegrees06(): Unit \ Assert = {
        assertEq(expected = Map#{
            1 => 1,
            2 => 1,
            3 => 1,
            4 => 1
        }, Graph.inDegrees(graph06()))
    }

    @Test
    def inDegrees07(): Unit \ Assert = {
        assertEq(expected = Map#{
            1 => 1,
            2 => 1,
            3 => 1,
            4 => 1,
            5 => 2,
            6 => 1
        }, Graph.inDegrees(graph07()))
    }

    ////////////////////////////////////////////////////////////////////////////
    // outDegrees                                                             //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def outDegrees01(): Unit \ Assert = {
        assertEq(expected = Map#{}, Graph.outDegrees(graph01()))
    }

    @Test
    def outDegrees02(): Unit \ Assert = {
        assertEq(expected = Map#{
            1 => 1,
            2 => 0
        }, Graph.outDegrees(graph02()))
    }

    @Test
    def outDegrees03(): Unit \ Assert = {
        assertEq(expected = Map#{
            1 => 1,
            2 => 0,
            3 => 1,
            4 => 0
        }, Graph.outDegrees(graph03()))
    }

    @Test
    def outDegrees04(): Unit \ Assert = {
        assertEq(expected = Map#{
            1 => 1,
            2 => 1,
            3 => 0
        }, Graph.outDegrees(graph04()))
    }

    @Test
    def outDegrees05(): Unit \ Assert = {
        assertEq(expected = Map#{
            1 => 1,
            2 => 1,
            3 => 1,
            4 => 0
        }, Graph.outDegrees(graph05()))
    }

    @Test
    def outDegrees06(): Unit \ Assert = {
        assertEq(expected = Map#{
            1 => 1,
            2 => 1,
            3 => 1,
            4 => 1
        }, Graph.outDegrees(graph06()))
    }

    @Test
    def outDegrees07(): Unit \ Assert = {
        assertEq(expected = Map#{
            1 => 1,
            2 => 2,
            3 => 1,
            4 => 1,
            5 => 1,
            6 => 1
        }, Graph.outDegrees(graph07()))
    }

    ////////////////////////////////////////////////////////////////////////////
    // toGraphviz                                                             //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def toGraphviz01(): Unit \ Assert = {
        assertEq(expected = String.unlines(
            "digraph {" ::
            "}" ::
            Nil
        ), Graph.toGraphviz(graph01()))
    }

    @Test
    def toGraphviz02(): Unit \ Assert = {
        let g = Set#{
            ("node \"A\"", "node \"B\"")
        };
        assertEq(expected = String.unlines(
            "digraph {" ::
            "  \"node \\\"A\\\"\" -> \"node \\\"B\\\"\"" ::
            "}" ::
            Nil
        ), Graph.toGraphviz(g))
    }

    @Test
    def toGraphviz03(): Unit \ Assert = {
        let result = Graph.toGraphviz(graph03());
        let test1 = result == String.unlines(
            "digraph {" ::
            "  \"1\" -> \"2\"" ::
            "  \"3\" -> \"4\"" ::
            "}" ::
            Nil
        );
        let test2 = result == String.unlines(
            "digraph {" ::
            "  \"3\" -> \"4\"" ::
            "  \"1\" -> \"2\"" ::
            "}" ::
            Nil
        );
        assertTrue(test1 or test2)
    }

    ////////////////////////////////////////////////////////////////////////////
    // toGraphvizLabeled                                                     //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def toGraphvizLabeled01(): Unit \ Assert = {
        assertEq(expected = String.unlines(
            "digraph {" ::
            "}" ::
            Nil
        ), Graph.toGraphvizLabeled(graphWithDist01()))
    }

    @Test
    def toGraphvizLabeled02(): Unit \ Assert = {
        let g = Set#{
            ("node \"A\"", -42, "node \"B\"")
        };
        assertEq(expected = String.unlines(
            "digraph {" ::
            "  \"node \\\"A\\\"\" -> \"node \\\"B\\\"\" [label = -42]" ::
            "}" ::
            Nil
        ), Graph.toGraphvizLabeled(g))
    }

    @Test
    def toGraphvizLabeled03(): Unit \ Assert = {
        let gv = Graph.toGraphvizLabeled(graphWithDist03());
        let test1 = gv == String.unlines(
            "digraph {" ::
            "  \"1\" -> \"2\" [label = 4]" ::
            "  \"3\" -> \"4\" [label = 7]" ::
            "}" ::
            Nil
        );
        let test2 = gv == String.unlines(
            "digraph {" ::
            "  \"3\" -> \"4\" [label = 7]" ::
            "  \"1\" -> \"2\" [label = 4]" ::
            "}" ::
            Nil
        );
        assertTrue(test1 or test2)
    }

    ////////////////////////////////////////////////////////////////////////////
    // flipEdges                                                              //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def flipEdges01(): Unit \ Assert = {
        assertEq(expected = Set#{}, Graph.flipEdges(graph01()))
    }

    @Test
    def flipEdges02(): Unit \ Assert = {
        assertEq(expected = Set#{(2, 1)}, Graph.flipEdges(graph02()))
    }

    @Test
    def flipEdges03(): Unit \ Assert = {
        assertEq(expected = Set#{(2, 1), (4, 3)}, Graph.flipEdges(graph03()))
    }

    @Test
    def flipEdges04(): Unit \ Assert = {
        assertEq(expected = Set#{(2, 1), (3, 2)}, Graph.flipEdges(graph04()))
    }

    @Test
    def flipEdges05(): Unit \ Assert = {
        assertEq(expected = Set#{(2, 1), (3, 2), (4, 3)}, Graph.flipEdges(graph05()))
    }

    @Test
    def flipEdges06(): Unit \ Assert = {
        assertEq(expected = Set#{(2, 1), (3, 2), (4, 3), (1, 4)}, Graph.flipEdges(graph06()))
    }

    @Test
    def flipEdges07(): Unit \ Assert = {
        assertEq(expected = Set#{
            (2, 1), (3, 2), (4, 3), (1, 4), (5, 2), (6, 5), (5, 6)
        }, Graph.flipEdges(graph07()))
    }

    ////////////////////////////////////////////////////////////////////////////
    // invert                                                                 //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def invert01(): Unit \ Assert = {
        assertEq(expected = Set#{}, Graph.invert(graph01()))
    }

    @Test
    def invert02(): Unit \ Assert = {
        assertEq(expected = Set#{(2, 1)}, Graph.invert(graph02()))
    }

    @Test
    def invert03(): Unit \ Assert = {
        assertEq(expected = Set#{
            (1, 3), (1, 4),
            (2, 1), (2, 3), (2, 4),
            (3, 1), (3, 2),
            (4, 1), (4, 2), (4, 3)
        }, Graph.invert(graph03()))
    }

    @Test
    def invert04(): Unit \ Assert = {
        assertEq(expected = Set#{
            (1, 3),
            (2, 1),
            (3, 1), (3, 2)
        }, Graph.invert(graph04()))
    }

    @Test
    def invert05(): Unit \ Assert = {
        assertEq(expected = Set#{
            (1, 3), (1, 4),
            (2, 1), (2, 4),
            (3, 1), (3, 2),
            (4, 1), (4, 2), (4, 3)
        }, Graph.invert(graph05()))
    }

    @Test
    def invert06(): Unit \ Assert = {
        assertEq(expected = Set#{
            (1, 3), (1, 4),
            (2, 1), (2, 4),
            (3, 1), (3, 2),
            (4, 2), (4, 3)
        }, Graph.invert(graph06()))
    }

    @Test
    def invert07(): Unit \ Assert = {
        assertEq(expected = Set#{
            (1, 3), (1, 4), (1, 5), (1, 6),
            (2, 1), (2, 4), (2, 6),
            (3, 1), (3, 2), (3, 5), (3, 6),
            (4, 2), (4, 3), (4, 5), (4, 6),
            (5, 1), (5, 2), (5, 3), (5, 4),
            (6, 1), (6, 2), (6, 3), (6, 4)
        }, Graph.invert(graph07()))
    }

    @Test
    def invert08(): Unit \ Assert = {
        assertEq(expected = Set#{}, Graph.invert(Set#{(1, 1)}))
    }

    @Test
    def invert09(): Unit \ Assert = {
        assertEq(expected = Set#{
            (1, 2), (1, 3),
            (2, 1),
            (3, 1), (3, 2)
        }, Graph.invert(Set#{(1, 1), (2, 3)}))
    }

    ////////////////////////////////////////////////////////////////////////////
    // distances                                                              //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def stableDistances01(): Unit \ Assert = {
        let g = graphWithDist01();
        assertEq(expected = Some(Graph.boundedDistances(g)), Graph.distances(g))
    }

    @Test
    def stableDistances02(): Unit \ Assert = {
        let g = graphWithDist02();
        assertEq(expected = Some(Graph.boundedDistances(g)), Graph.distances(g))
    }

    @Test
    def stableDistances03(): Unit \ Assert = {
        let g = graphWithDist03();
        assertEq(expected = Some(Graph.boundedDistances(g)), Graph.distances(g))
    }

    @Test
    def stableDistances04(): Unit \ Assert = {
        let g = graphWithDist04();
        assertEq(expected = Some(Graph.boundedDistances(g)), Graph.distances(g))
    }

    @Test
    def stableDistances05(): Unit \ Assert = {
        let g = graphWithDist05();
        assertEq(expected = Some(Graph.boundedDistances(g)), Graph.distances(g))
    }

    @Test
    def stableDistances06(): Unit \ Assert = {
        let g = graphWithDist06();
        assertEq(expected = Some(Graph.boundedDistances(g)), Graph.distances(g))
    }

    @Test
    def stableDistances07(): Unit \ Assert = {
        let g = Set#{("a", 1, "b"), ("b", 2, "c"), ("c", -4, "a")};
        assertEq(expected = None, Graph.distances(g))
    }

    ////////////////////////////////////////////////////////////////////////////
    // withinDistanceOf                                                       //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def withinDistanceOf01(): Unit \ Assert = {
        assertEq(expected = Set#{42}, Graph.withinDistanceOf(42, 999, graphWithDist01()))
    }

    @Test
    def withinDistanceOf02(): Unit \ Assert = {
        assertEq(expected = Set#{}, Graph.withinDistanceOf(1, -42, graphWithDist02()))
    }

    @Test
    def withinDistanceOf03(): Unit \ Assert = {
        assertEq(expected = Set#{1}, Graph.withinDistanceOf(1, 3, graphWithDist02()))
    }

    @Test
    def withinDistanceOf04(): Unit \ Assert = {
        assertEq(expected = Set#{1, 2}, Graph.withinDistanceOf(1, 4, graphWithDist02()))
    }

    @Test
    def withinDistanceOf05(): Unit \ Assert = {
        assertEq(expected = Set#{1, 2}, Graph.withinDistanceOf(1, 99, graphWithDist02()))
    }

    @Test
    def withinDistanceOf06(): Unit \ Assert = {
        assertEq(expected = Set#{3, 4}, Graph.withinDistanceOf(3, 7, graphWithDist03()))
    }

    @Test
    def withinDistanceOf07(): Unit \ Assert = {
        assertEq(expected = Set#{1, 2, 3}, Graph.withinDistanceOf(1, 11, graphWithDist04()))
    }

    @Test
    def withinDistanceOf08(): Unit \ Assert = {
        assertEq(expected = Set#{2, 3, 4}, Graph.withinDistanceOf(2, 7, graphWithDist05()))
    }

    @Test
    def withinDistanceOf09(): Unit \ Assert = {
        assertEq(expected = Set#{4, 5, 6}, Graph.withinDistanceOf(4, 3, graphWithDist06()))
    }

    ////////////////////////////////////////////////////////////////////////////
    // withinEdgesOf                                                          //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def withinEdgesOf01(): Unit \ Assert = {
        assertEq(expected = Set#{42}, Graph.withinEdgesOf(42, 999, graph01()))
    }

    @Test
    def withinEdgesOf02(): Unit \ Assert = {
        assertEq(expected = Set#{}, Graph.withinEdgesOf(1, -42, graph02()))
    }

    @Test
    def withinEdgesOf03(): Unit \ Assert = {
        assertEq(expected = Set#{1, 2}, Graph.withinEdgesOf(1, 1, graph02()))
    }

    @Test
    def withinEdgesOf04(): Unit \ Assert = {
        assertEq(expected = Set#{1, 2}, Graph.withinEdgesOf(1, 4, graph02()))
    }

    @Test
    def withinEdgesOf05(): Unit \ Assert = {
        assertEq(expected = Set#{3, 4}, Graph.withinEdgesOf(3, 1, graph03()))
    }

    @Test
    def withinEdgesOf06(): Unit \ Assert = {
        assertEq(expected = Set#{1, 2, 3}, Graph.withinEdgesOf(1, 2, graph04()))
    }

    @Test
    def withinEdgesOf07(): Unit \ Assert = {
        assertEq(expected = Set#{1, 2, 3}, Graph.withinEdgesOf(1, 2, graph05()))
    }

    @Test
    def withinEdgesOf08(): Unit \ Assert = {
        assertEq(expected = Set#{1, 4}, Graph.withinEdgesOf(4, 1, graph06()))
    }

    @Test
    def withinEdgesOf09(): Unit \ Assert = {
        assertEq(expected = Set#{1, 2, 3, 5}, Graph.withinEdgesOf(1, 2, graph07()))
    }

    ////////////////////////////////////////////////////////////////////////////
    // topologicalSort                                                        //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def topologicalSort01(): Unit \ Assert = {
        assertEq(expected = Nil, Graph.topologicalSort(graph01()))
    }

    @Test
    def topologicalSort02(): Unit \ Assert = {
        assertEq(expected = 1 :: 2 :: Nil, Graph.topologicalSort(graph02()))
    }

    @Test
    def topologicalSort03(): Unit \ Assert = {
        assertEq(expected = 1 :: 3 :: 2 :: 4 :: Nil, Graph.topologicalSort(graph03()))
    }

    @Test
    def topologicalSort04(): Unit \ Assert = {
        assertEq(expected = 1 :: 2 :: 3 :: Nil, Graph.topologicalSort(graph04()))
    }

    @Test
    def topologicalSort05(): Unit \ Assert = {
        assertEq(expected = 1 :: 2 :: 3 :: 4 :: Nil, Graph.topologicalSort(graph05()))
    }

    ////////////////////////////////////////////////////////////////////////////
    // degrees                                                                //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def degrees01(): Unit \ Assert = {
        assertEq(expected = Map#{}, Graph.degrees(graph01()))
    }

    @Test
    def degrees02(): Unit \ Assert = {
        assertEq(expected = Map#{1 => 1, 2 => 1}, Graph.degrees(graph02()))
    }

    @Test
    def degrees03(): Unit \ Assert = {
        assertEq(expected = Map#{1 => 1, 2 => 1, 3 => 1, 4 => 1}, Graph.degrees(graph03()))
    }

    @Test
    def degrees04(): Unit \ Assert = {
        assertEq(expected = Map#{1 => 1, 2 => 2, 3 => 1}, Graph.degrees(graph04()))
    }

    @Test
    def degrees05(): Unit \ Assert = {
        assertEq(expected = Map#{1 => 1, 2 => 2, 3 => 2, 4 => 1}, Graph.degrees(graph05()))
    }

    @Test
    def degrees06(): Unit \ Assert = {
        assertEq(expected = Map#{1 => 2, 2 => 2, 3 => 2, 4 => 2}, Graph.degrees(graph06()))
    }

    @Test
    def degrees07(): Unit \ Assert = {
        assertEq(expected = Map#{
            1 => 2, 2 => 3, 3 => 2,
            4 => 2, 5 => 3, 6 => 2
        }, Graph.degrees(graph07()))
    }

    ////////////////////////////////////////////////////////////////////////////
    // frontiersFrom                                                          //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def frontiersFrom01(): Unit \ Assert = {
        assertEq(expected = Map#{0 => Set#{42}}, Graph.frontiersFrom(42, graph01()))
    }

    @Test
    def frontiersFrom02(): Unit \ Assert = {
        assertEq(expected = Map#{0 => Set#{1}, 1 => Set#{2}}, Graph.frontiersFrom(1, graph02()))
    }

    @Test
    def frontiersFrom03(): Unit \ Assert = {
        assertEq(expected = Map#{
            0 => Set#{3},
            1 => Set#{4}
        }, Graph.frontiersFrom(3, graph03()))
    }

    @Test
    def frontiersFrom04(): Unit \ Assert = {
        assertEq(expected = Map#{
            0 => Set#{1},
            1 => Set#{2},
            2 => Set#{3}
        }, Graph.frontiersFrom(1, graph04()))
    }

    @Test
    def frontiersFrom05(): Unit \ Assert = {
        assertEq(expected = Map#{
            0 => Set#{2},
            1 => Set#{3},
            2 => Set#{4}
        }, Graph.frontiersFrom(2, graph05()))
    }

    @Test
    def frontiersFrom06(): Unit \ Assert = {
        assertEq(expected = Map#{
            0 => Set#{3},
            1 => Set#{4},
            2 => Set#{1},
            3 => Set#{2}
        }, Graph.frontiersFrom(3, graph06()))
    }

    @Test
    def frontiersFrom07(): Unit \ Assert = {
        assertEq(expected = Map#{
            0 => Set#{4},
            1 => Set#{1},
            2 => Set#{2},
            3 => Set#{3, 5},
            4 => Set#{6}
        }, Graph.frontiersFrom(4, graph07()))
    }

    @Test
    def frontiersFrom08(): Unit \ Assert = {
        assertEq(expected = Map#{
            0 => Set#{5},
            1 => Set#{6}
        }, Graph.frontiersFrom(5, graph07()))
    }

    ////////////////////////////////////////////////////////////////////////////
    // cutPoints                                                              //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def cutPoints01(): Unit \ Assert = {
        assertEq(expected = Set#{}, Graph.cutPoints(graph01()))
    }

    @Test
    def cutPoints02(): Unit \ Assert = {
        assertEq(expected = Set#{}, Graph.cutPoints(graph02()))
    }

    @Test
    def cutPoints03(): Unit \ Assert = {
        assertEq(expected = Set#{}, Graph.cutPoints(graph03()))
    }

    @Test
    def cutPoints04(): Unit \ Assert = {
        assertEq(expected = Set#{(1, 2, 3)}, Graph.cutPoints(graph04()))
    }

    @Test
    def cutPoints05(): Unit \ Assert = {
        assertEq(expected = Set#{
            (1, 2, 3), (1, 3, 4), (2, 3, 4)
        }, Graph.cutPoints(graph05()))
    }

    @Test
    def cutPoints06(): Unit \ Assert = {
        assertEq(expected = Set#{
            (1, 2, 3), (1, 3, 4),
            (2, 3, 4), (2, 4, 1),
            (3, 4, 1), (3, 4, 2),
            (4, 1, 2), (4, 2, 3)
        }, Graph.cutPoints(graph06()))
    }

    @Test
    def cutPoints07(): Unit \ Assert = {
        assertEq(expected = Set#{
            (1, 2, 3), (1, 3, 4), (1, 2, 5), (1, 5, 6),
            (2, 3, 4), (2, 4, 1), (2, 5, 6),
            (3, 4, 1), (3, 4, 2), (3, 4, 5), (3, 5, 6),
            (4, 1, 2), (4, 2, 3), (4, 2, 5), (4, 5, 6)
        }, Graph.cutPoints(graph07()))
    }

}

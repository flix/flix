mod TestGraph {

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
    def closure01(): Bool = {
        let g = graph01();
        let s = Set#{};
        Graph.closure(g) == s
    }

    @Test
    def closure02(): Bool = {
        let g = graph02();
        let s = Set#{(1, 1), (1, 2), (2, 2)};
        Graph.closure(g) == s
    }

    @Test
    def closure03(): Bool = {
        let g = graph03();
        let s = Set#{(1, 1), (1, 2), (2, 2), (3, 3), (3, 4), (4, 4)};
        Graph.closure(g) == s
    }

    @Test
    def closure04(): Bool = {
        let g = graph04();
        let s = Set#{(1, 1), (1, 2), (1, 3), (2, 2), (2, 3), (3, 3)};
        Graph.closure(g) == s
    }

    @Test
    def closure05(): Bool = {
        let g = graph05();
        let s = Set#{(1, 1), (1, 2), (1, 3), (1, 4),
                     (2, 2), (2, 3), (2, 4),
                     (3, 3), (3, 4),
                     (4, 4)};
        Graph.closure(g) == s
    }


    ////////////////////////////////////////////////////////////////////////////
    // reachableFrom                                                          //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def reachable01(): Bool = {
        let g = graph01();
        Graph.reachableFrom(1, g) == Set#{1}
    }

    @Test
    def reachable02(): Bool = {
        let g = graph02();
        Graph.reachableFrom(1, g) == Set#{1, 2}
    }

    @Test
    def reachable03(): Bool = {
        let g = graph03();
        Graph.reachableFrom(1, g) == Set#{1, 2}
    }

    @Test
    def reachable04(): Bool = {
        let g = graph04();
        Graph.reachableFrom(1, g) == Set#{1, 2, 3}
    }

    @Test
    def reachable05(): Bool = {
        let g = graph05();
        Graph.reachableFrom(1, g) == Set#{1, 2, 3, 4}
    }

    @Test
    def reachable06(): Bool = {
        let g = graph05();
        Graph.reachableFrom(3, g) == Set#{3, 4}
    }

    @Test
    def reachable07(): Bool = {
        let g = graph05();
        Graph.reachableFrom(5, g) == Set#{5}
    }


    ////////////////////////////////////////////////////////////////////////////
    // unreachableFrom                                                        //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def unreachable01(): Bool = {
        let g = graph01();
        Graph.unreachableFrom(1, g) == Set#{}
    }

    @Test
    def unreachable02(): Bool = {
        let g = graph02();
        Graph.unreachableFrom(1, g) == Set#{}
    }

    @Test
    def unreachable03(): Bool = {
        let g = graph02();
        Graph.unreachableFrom(2, g) == Set#{1}
    }

    @Test
    def unreachable04(): Bool = {
        let g = graph03();
        Graph.unreachableFrom(1, g) == Set#{3, 4}
    }

    @Test
    def unreachable05(): Bool = {
        let g = graph04();
        Graph.unreachableFrom(2, g) == Set#{1}
    }

    @Test
    def unreachable06(): Bool = {
        let g = graph05();
        Graph.unreachableFrom(3, g) == Set#{1, 2}
    }

    @Test
    def unreachable07(): Bool = {
        let g = graph05();
        Graph.unreachableFrom(5, g) == Set#{1, 2, 3, 4}
    }


    ////////////////////////////////////////////////////////////////////////////
    // reachable                                                              //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def isConnected01(): Bool = {
        let g = graph01();
        Graph.reachable(src = 1, dst = 2, g) == false
    }

    @Test
    def isConnected02(): Bool = {
        let g = graph02();
        Graph.reachable(src = 1, dst = 2, g)
    }

    @Test
    def isConnected03(): Bool = {
        let g = graph02();
        Graph.reachable(src = 2, dst = 1, g) == false
    }

    @Test
    def isConnected04(): Bool = {
        let g = graph03();
        Graph.reachable(src = 1, dst = 3, g) == false
    }

    @Test
    def isConnected05(): Bool = {
        let g = graph04();
        Graph.reachable(src = 1, dst = 3, g)
    }

    @Test
    def isConnected06(): Bool = {
        let g = graph05();
        Graph.reachable(src = 2, dst = 4, g)
    }

    @Test
    def isConnected07(): Bool = {
        let g = graph05();
        Graph.reachable(src = 4, dst = 1, g) == false
    }

    @Test
    def isConnected08(): Bool = {
        let g = graph05();
        Graph.reachable(src = 5, dst = 6, g) == false
    }


    ////////////////////////////////////////////////////////////////////////////
    // stronglyConnectedComponents                                            //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def stronglyConnectedComponents01(): Bool = {
        let g = graph01();
        let s = Set#{};
        Graph.stronglyConnectedComponents(g) == s
    }

    @Test
    def stronglyConnectedComponents02(): Bool = {
        let g = graph02();
        let s = Set#{Set#{1}, Set#{2}};
        Graph.stronglyConnectedComponents(g) == s
    }

    @Test
    def stronglyConnectedComponents03(): Bool = {
        let g = graph03();
        let s = Set#{Set#{1}, Set#{2}, Set#{3}, Set#{4}};
        Graph.stronglyConnectedComponents(g) == s
    }

    @Test
    def stronglyConnectedComponents04(): Bool = {
        let g = graph04();
        let s = Set#{Set#{1}, Set#{2}, Set#{3}};
        Graph.stronglyConnectedComponents(g) == s
    }

    @Test
    def stronglyConnectedComponents05(): Bool = {
        let g = graph05();
        let s = Set#{Set#{1}, Set#{2}, Set#{3}, Set#{4}};
        Graph.stronglyConnectedComponents(g) == s
    }

    @Test
    def stronglyConnectedComponents06(): Bool = {
        let g = graph06();
        let s = Set#{Set#{1, 2, 3, 4}};
        Graph.stronglyConnectedComponents(g) == s
    }

    @Test
    def stronglyConnectedComponents07(): Bool = {
        let g = graph07();
        let s = Set#{Set#{1, 2, 3, 4}, Set#{5, 6}};
        Graph.stronglyConnectedComponents(g) == s
    }

    ////////////////////////////////////////////////////////////////////////////
    // isCyclic                                                               //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def isCyclic01(): Bool = {
        let g = graph01();
        Graph.isCyclic(g) == false
    }

    @Test
    def isCyclic02(): Bool = {
        let g = graph02();
        Graph.isCyclic(g) == false
    }

    @Test
    def isCyclic03(): Bool = {
        let g = graph03();
        Graph.isCyclic(g) == false
    }

    @Test
    def isCyclic04(): Bool = {
        let g = graph04();
        Graph.isCyclic(g) == false
    }

    @Test
    def isCyclic05(): Bool = {
        let g = graph05();
        Graph.isCyclic(g) == false
    }

    @Test
    def isCyclic06(): Bool = {
        let g = graph06();
        Graph.isCyclic(g)
    }

    @Test
    def isCyclic07(): Bool = {
        let g = graph07();
        Graph.isCyclic(g)
    }

    ////////////////////////////////////////////////////////////////////////////
    // boundedDistances                                                       //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def distances01(): Bool = {
        let g = graphWithDist01();
        Graph.boundedDistances(g) == Map.empty()
    }

    @Test
    def distances02(): Bool = {
        let g = graphWithDist02();
        Graph.boundedDistances(g) == Map#{
            (1, 1) => 0,
            (2, 2) => 0,

            (1, 2) => 4
        }
    }

    @Test
    def distances03(): Bool = {
        let g = graphWithDist03();
        Graph.boundedDistances(g) == Map#{
            (1, 1) => 0,
            (2, 2) => 0,
            (3, 3) => 0,
            (4, 4) => 0,

            (1, 2) => 4,
            (3, 4) => 7
        }
    }

    @Test
    def distances04(): Bool = {
        let g = graphWithDist04();
        Graph.boundedDistances(g) == Map#{
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
        }
    }

    @Test
    def distances05(): Bool = {
        let g = graphWithDist05();
        Graph.boundedDistances(g) == Map#{
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
        }
    }

    @Test
    def distances06(): Bool = {
        let g = graphWithDist06();
        Graph.boundedDistances(g) == Map#{
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
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    // distancesFrom                                                          //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def distancesFrom01(): Bool = {
        let g = graphWithDist01();
        Graph.distancesFrom(1, g) == Map#{1 => 0}
    }

    @Test
    def distancesFrom02(): Bool = {
        let g = graphWithDist02();
        Graph.distancesFrom(1, g) == Map#{1 => 0, 2 => 4}
    }

    @Test
    def distancesFrom03(): Bool = {
        let g = graphWithDist02();
        Graph.distancesFrom(2, g) == Map#{2 => 0}
    }

    @Test
    def distancesFrom04(): Bool = {
        let g = graphWithDist03();
        Graph.distancesFrom(3, g) == Map#{3 => 0, 4 => 7}
    }

    @Test
    def distancesFrom05(): Bool = {
        let g = graphWithDist04();
        Graph.distancesFrom(2, g) == Map#{2 => 0, 3 => 5, 4 => (5 + 7)}
    }

    @Test
    def distancesFrom06(): Bool = {
        let g = graphWithDist05();
        Graph.distancesFrom(4, g) == Map#{
            1 => 5,
            2 => (5 + 4),
            3 => (5 + 4 + 2),
            4 => 0
        }
    }

    @Test
    def distancesFrom07(): Bool = {
        let g = graphWithDist06();
        Graph.distancesFrom(4, g) == Map#{
            2 => 7,
            3 => (7 + 10),
            4 => 0,
            5 => 2,
            6 => 3
        }
    }


    ////////////////////////////////////////////////////////////////////////////
    // distance                                                               //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def distance01(): Bool = {
        let g = graphWithDist01();
        Graph.distance(src = 1, dst = 2, g) == None
    }

    @Test
    def distance02(): Bool = {
        let g = graphWithDist02();
        Graph.distance(src = 1, dst = 2, g) == Some(4)
    }

    @Test
    def distance03(): Bool = {
        let g = graphWithDist02();
        Graph.distance(src = 2, dst = 1, g) == None
    }

    @Test
    def distance04(): Bool = {
        let g = graphWithDist03();
        Graph.distance(src = 1, dst = 2, g) == Some(4)
    }

    @Test
    def distance05(): Bool = {
        let g = graphWithDist03();
        Graph.distance(src = 3, dst = 4, g) == Some(7)
    }

    @Test
    def distance06(): Bool = {
        let g = graphWithDist03();
        Graph.distance(src = 1, dst = 4, g) == None
    }

    @Test
    def distance07(): Bool = {
        let g = graphWithDist04();
        Graph.distance(src = 1, dst = 4, g) == Some(4 + 5 + 7)
    }

    @Test
    def distance08(): Bool = {
        let g = graphWithDist05();
        Graph.distance(src = 4, dst = 2, g) == Some(5 + 4)
    }

    @Test
    def distance09(): Bool = {
        let g = graphWithDist06();
        Graph.distance(src = 1, dst = 6, g) == Some(5 + 2 + 1)
    }

    ////////////////////////////////////////////////////////////////////////////
    // toUndirected                                                           //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def toUndirected01(): Bool = {
        let g = graph01() |> Graph.toUndirected;
        g == Set#{}
    }

    @Test
    def toUndirected02(): Bool = {
        let g = graph02() |> Graph.toUndirected;
        g == Set#{(1, 2), (2, 1)}
    }

    @Test
    def toUndirected03(): Bool = {
        let g = graph03() |> Graph.toUndirected;
        g == Set#{(1, 2), (2, 1), (3, 4), (4, 3)}
    }

    @Test
    def toUndirected04(): Bool = {
        let g = graph04() |> Graph.toUndirected;
        g == Set#{(1, 2), (2, 1), (2, 3), (3, 2)}
    }

    @Test
    def toUndirected05(): Bool = {
        let g = graph05() |> Graph.toUndirected;
        g == Set#{(1, 2), (2, 1), (2, 3), (3, 2), (3, 4), (4, 3)}
    }

    @Test
    def toUndirected06(): Bool = {
        let g = graph06() |> Graph.toUndirected;
        g == Set#{
            (1, 2), (2, 1),
            (2, 3), (3, 2),
            (3, 4), (4, 3),
            (4, 1), (1, 4)
        }
    }

    @Test
    def toUndirected07(): Bool = {
        let g = graph07() |> Graph.toUndirected;
        g == Set#{
            (1, 2), (2, 1),
            (1, 4), (4, 1),
            (2, 3), (3, 2),
            (2, 5), (5, 2),
            (3, 4), (4, 3),
            (5, 6), (6, 5)
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    // toUndirectedWeighted                                                   //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def toUndirectedWeighted01(): Bool = {
        let g = graphWithDist01() |> Graph.toUndirectedWeighted;
        g == Set#{}
    }

    @Test
    def toUndirectedWeighted02(): Bool = {
        let g = graphWithDist02() |> Graph.toUndirectedWeighted;
        g == Set#{(1, 4, 2), (2, 4, 1)}
    }

    @Test
    def toUndirectedWeighted03(): Bool = {
        let g = graphWithDist03() |> Graph.toUndirectedWeighted;
        g == Set#{
            (1, 4, 2), (2, 4, 1),
            (3, 7, 4), (4, 7, 3)
        }
    }

    @Test
    def toUndirectedWeighted04(): Bool = {
        let g = graphWithDist04() |> Graph.toUndirectedWeighted;
        g == Set#{
            (1, 4, 2), (2, 4, 1),
            (2, 5, 3), (3, 5, 2),
            (3, 7, 4), (4, 7, 3)
        }
    }

    @Test
    def toUndirectedWeighted05(): Bool = {
        let g = graphWithDist05() |> Graph.toUndirectedWeighted;
        g == Set#{
            (1, 4, 2), (2, 4, 1),
            (1, 5, 4), (4, 5, 1),
            (2, 2, 3), (3, 2, 2),
            (3, 1, 4), (4, 1, 3)
        }
    }

    @Test
    def toUndirectedWeighted06(): Bool = {
        let g = graphWithDist06() |> Graph.toUndirectedWeighted;
        g == Set#{
            (1, 2, 2), (2, 2, 1),
            (1, 5, 4), (4, 5, 1),
            (2, 10, 3), (3, 10, 2),
            (2, 7, 4), (4, 7, 2),
            (4, 2, 5), (5, 2, 4),
            (4, 4, 6), (6, 4, 4),
            (5, 1, 6), (6, 1, 5)
        }
    }

    @Test
    def toUndirectedWeighted07(): Bool = {
        let g0 = Set#{(7, 9, 13), (13, 8, 7)};
        let g = g0 |> Graph.toUndirectedWeighted;
        g == Set#{
            (7, 9, 13), (13, 9, 7),
            (7, 8, 13), (13, 8, 7)
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    // inDegrees                                                              //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def inDegrees01(): Bool = {
        Graph.inDegrees(graph01()) == Map#{}
    }

    @Test
    def inDegrees02(): Bool = {
        Graph.inDegrees(graph02()) == Map#{
            1 => 0,
            2 => 1
        }
    }

    @Test
    def inDegrees03(): Bool = {
        Graph.inDegrees(graph03()) == Map#{
            1 => 0,
            2 => 1,
            3 => 0,
            4 => 1
        }
    }

    @Test
    def inDegrees04(): Bool = {
        Graph.inDegrees(graph04()) == Map#{
            1 => 0,
            2 => 1,
            3 => 1
        }
    }

    @Test
    def inDegrees05(): Bool = {
        Graph.inDegrees(graph05()) == Map#{
            1 => 0,
            2 => 1,
            3 => 1,
            4 => 1
        }
    }

    @Test
    def inDegrees06(): Bool = {
        Graph.inDegrees(graph06()) == Map#{
            1 => 1,
            2 => 1,
            3 => 1,
            4 => 1
        }
    }

    @Test
    def inDegrees07(): Bool = {
        Graph.inDegrees(graph07()) == Map#{
            1 => 1,
            2 => 1,
            3 => 1,
            4 => 1,
            5 => 2,
            6 => 1
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    // outDegrees                                                             //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def outDegrees01(): Bool = {
        Graph.outDegrees(graph01()) == Map#{}
    }

    @Test
    def outDegrees02(): Bool = {
        Graph.outDegrees(graph02()) == Map#{
            1 => 1,
            2 => 0
        }
    }

    @Test
    def outDegrees03(): Bool = {
        Graph.outDegrees(graph03()) == Map#{
            1 => 1,
            2 => 0,
            3 => 1,
            4 => 0
        }
    }

    @Test
    def outDegrees04(): Bool = {
        Graph.outDegrees(graph04()) == Map#{
            1 => 1,
            2 => 1,
            3 => 0
        }
    }

    @Test
    def outDegrees05(): Bool = {
        Graph.outDegrees(graph05()) == Map#{
            1 => 1,
            2 => 1,
            3 => 1,
            4 => 0
        }
    }

    @Test
    def outDegrees06(): Bool = {
        Graph.outDegrees(graph06()) == Map#{
            1 => 1,
            2 => 1,
            3 => 1,
            4 => 1
        }
    }

    @Test
    def outDegrees07(): Bool = {
        Graph.outDegrees(graph07()) == Map#{
            1 => 1,
            2 => 2,
            3 => 1,
            4 => 1,
            5 => 1,
            6 => 1
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    // toGraphviz                                                             //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def toGraphviz01(): Bool = {
        Graph.toGraphviz(graph01()) == String.unlines(
            "digraph {" ::
            "}" ::
            Nil
        )
    }

    @Test
    def toGraphviz02(): Bool = {
        let g = Set#{
            ("node \"A\"", "node \"B\"")
        };
        Graph.toGraphviz(g) == String.unlines(
            "digraph {" ::
            "  \"node \\\"A\\\"\" -> \"node \\\"B\\\"\"" ::
            "}" ::
            Nil
        )
    }

    @Test
    def toGraphviz03(): Bool = {
        let test1 = Graph.toGraphviz(graph03()) == String.unlines(
            "digraph {" ::
            "  \"1\" -> \"2\"" ::
            "  \"3\" -> \"4\"" ::
            "}" ::
            Nil
        );
        let test2 = Graph.toGraphviz(graph03()) == String.unlines(
            "digraph {" ::
            "  \"3\" -> \"4\"" ::
            "  \"1\" -> \"2\"" ::
            "}" ::
            Nil
        );
        test1 or test2
    }

    ////////////////////////////////////////////////////////////////////////////
    // toGraphvizWeighted                                                     //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def toGraphvizWeighted01(): Bool = {
        Graph.toGraphvizWeighted(graphWithDist01()) == String.unlines(
            "digraph {" ::
            "}" ::
            Nil
        )
    }

    @Test
    def toGraphvizWeighted02(): Bool = {
        let g = Set#{
            ("node \"A\"", -42, "node \"B\"")
        };
        Graph.toGraphvizWeighted(g) == String.unlines(
            "digraph {" ::
            "  \"node \\\"A\\\"\" -> \"node \\\"B\\\"\" [label = -42]" ::
            "}" ::
            Nil
        )
    }

    @Test
    def toGraphvizWeighted03(): Bool = {
        let gv = Graph.toGraphvizWeighted(graphWithDist03());
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
        test1 or test2
    }

    ////////////////////////////////////////////////////////////////////////////
    // flipEdges                                                              //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def flipEdges01(): Bool = {
        Graph.flipEdges(graph01()) == Set#{}
    }

    @Test
    def flipEdges02(): Bool = {
        Graph.flipEdges(graph02()) == Set#{(2, 1)}
    }

    @Test
    def flipEdges03(): Bool = {
        Graph.flipEdges(graph03()) == Set#{(2, 1), (4, 3)}
    }

    @Test
    def flipEdges04(): Bool = {
        Graph.flipEdges(graph04()) == Set#{(2, 1), (3, 2)}
    }

    @Test
    def flipEdges05(): Bool = {
        Graph.flipEdges(graph05()) == Set#{(2, 1), (3, 2), (4, 3)}
    }

    @Test
    def flipEdges06(): Bool = {
        Graph.flipEdges(graph06()) == Set#{(2, 1), (3, 2), (4, 3), (1, 4)}
    }

    @Test
    def flipEdges07(): Bool = {
        Graph.flipEdges(graph07()) == Set#{
            (2, 1), (3, 2), (4, 3), (1, 4), (5, 2), (6, 5), (5, 6)
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    // invert                                                                 //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def invert01(): Bool = {
        Graph.invert(graph01()) == Set#{}
    }

    @Test
    def invert02(): Bool = {
        Graph.invert(graph02()) == Set#{(2, 1)}
    }

    @Test
    def invert03(): Bool = {
        Graph.invert(graph03()) == Set#{
            (1, 3), (1, 4),
            (2, 1), (2, 3), (2, 4),
            (3, 1), (3, 2),
            (4, 1), (4, 2), (4, 3)
        }
    }

    @Test
    def invert04(): Bool = {
        Graph.invert(graph04()) == Set#{
            (1, 3),
            (2, 1),
            (3, 1), (3, 2)
        }
    }

    @Test
    def invert05(): Bool = {
        Graph.invert(graph05()) == Set#{
            (1, 3), (1, 4),
            (2, 1), (2, 4),
            (3, 1), (3, 2),
            (4, 1), (4, 2), (4, 3)
        }
    }

    @Test
    def invert06(): Bool = {
        Graph.invert(graph06()) == Set#{
            (1, 3), (1, 4),
            (2, 1), (2, 4),
            (3, 1), (3, 2),
            (4, 2), (4, 3)
        }
    }

    @Test
    def invert07(): Bool = {
        Graph.invert(graph07()) == Set#{
            (1, 3), (1, 4), (1, 5), (1, 6),
            (2, 1), (2, 4), (2, 6),
            (3, 1), (3, 2), (3, 5), (3, 6),
            (4, 2), (4, 3), (4, 5), (4, 6),
            (5, 1), (5, 2), (5, 3), (5, 4),
            (6, 1), (6, 2), (6, 3), (6, 4)
        }
    }

    @Test
    def invert08(): Bool = {
        Graph.invert(Set#{(1, 1)}) == Set#{}
    }

    @Test
    def invert09(): Bool = {
        Graph.invert(Set#{(1, 1), (2, 3)}) == Set#{
            (1, 2), (1, 3),
            (2, 1),
            (3, 1), (3, 2)
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    // distances                                                              //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def stableDistances01(): Bool = {
        let g = graphWithDist01();
        Graph.distances(g) == Some(Graph.boundedDistances(g))
    }

    @Test
    def stableDistances02(): Bool = {
        let g = graphWithDist02();
        Graph.distances(g) == Some(Graph.boundedDistances(g))
    }

    @Test
    def stableDistances03(): Bool = {
        let g = graphWithDist03();
        Graph.distances(g) == Some(Graph.boundedDistances(g))
    }

    @Test
    def stableDistances04(): Bool = {
        let g = graphWithDist04();
        Graph.distances(g) == Some(Graph.boundedDistances(g))
    }

    @Test
    def stableDistances05(): Bool = {
        let g = graphWithDist05();
        Graph.distances(g) == Some(Graph.boundedDistances(g))
    }

    @Test
    def stableDistances06(): Bool = {
        let g = graphWithDist06();
        Graph.distances(g) == Some(Graph.boundedDistances(g))
    }

    @Test
    def stableDistances07(): Bool = {
        let g = Set#{("a", 1, "b"), ("b", 2, "c"), ("c", -4, "a")};
        Graph.distances(g) == None
    }

    ////////////////////////////////////////////////////////////////////////////
    // withinDistanceOf                                                       //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def withinDistanceOf01(): Bool = {
        Graph.withinDistanceOf(42, 999, graphWithDist01()) == Set#{42}
    }

    @Test
    def withinDistanceOf02(): Bool = {
        Graph.withinDistanceOf(1, -42, graphWithDist02()) == Set#{}
    }

    @Test
    def withinDistanceOf03(): Bool = {
        Graph.withinDistanceOf(1, 3, graphWithDist02()) == Set#{1}
    }

    @Test
    def withinDistanceOf04(): Bool = {
        Graph.withinDistanceOf(1, 4, graphWithDist02()) == Set#{1, 2}
    }

    @Test
    def withinDistanceOf05(): Bool = {
        Graph.withinDistanceOf(1, 99, graphWithDist02()) == Set#{1, 2}
    }

    @Test
    def withinDistanceOf06(): Bool = {
        Graph.withinDistanceOf(3, 7, graphWithDist03()) == Set#{3, 4}
    }

    @Test
    def withinDistanceOf07(): Bool = {
        Graph.withinDistanceOf(1, 11, graphWithDist04()) == Set#{1, 2, 3}
    }

    @Test
    def withinDistanceOf08(): Bool = {
        Graph.withinDistanceOf(2, 7, graphWithDist05()) == Set#{2, 3, 4}
    }

    @Test
    def withinDistanceOf09(): Bool = {
        Graph.withinDistanceOf(4, 3, graphWithDist06()) == Set#{4, 5, 6}
    }

    ////////////////////////////////////////////////////////////////////////////
    // withinEdgesOf                                                          //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def withinEdgesOf01(): Bool = {
        Graph.withinEdgesOf(42, 999, graph01()) == Set#{42}
    }

    @Test
    def withinEdgesOf02(): Bool = {
        Graph.withinEdgesOf(1, -42, graph02()) == Set#{}
    }

    @Test
    def withinEdgesOf03(): Bool = {
        Graph.withinEdgesOf(1, 1, graph02()) == Set#{1, 2}
    }

    @Test
    def withinEdgesOf04(): Bool = {
        Graph.withinEdgesOf(1, 4, graph02()) == Set#{1, 2}
    }

    @Test
    def withinEdgesOf05(): Bool = {
        Graph.withinEdgesOf(3, 1, graph03()) == Set#{3, 4}
    }

    @Test
    def withinEdgesOf06(): Bool = {
        Graph.withinEdgesOf(1, 2, graph04()) == Set#{1, 2, 3}
    }

    @Test
    def withinEdgesOf07(): Bool = {
        Graph.withinEdgesOf(1, 2, graph05()) == Set#{1, 2, 3}
    }

    @Test
    def withinEdgesOf08(): Bool = {
        Graph.withinEdgesOf(4, 1, graph06()) == Set#{1, 4}
    }

    @Test
    def withinEdgesOf09(): Bool = {
        Graph.withinEdgesOf(1, 2, graph07()) == Set#{1, 2, 3, 5}
    }

    ////////////////////////////////////////////////////////////////////////////
    // topologicalSort                                                        //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def topologicalSort01(): Bool = {
        Graph.topologicalSort(graph01()) == Nil
    }

    @Test
    def topologicalSort02(): Bool = {
        Graph.topologicalSort(graph02()) == 1 :: 2 :: Nil
    }

    @Test
    def topologicalSort03(): Bool = {
        Graph.topologicalSort(graph03()) == 1 :: 3 :: 2 :: 4 :: Nil
    }

    @Test
    def topologicalSort04(): Bool = {
        Graph.topologicalSort(graph04()) == 1 :: 2 :: 3 :: Nil
    }

    @Test
    def topologicalSort05(): Bool = {
        Graph.topologicalSort(graph05()) == 1 :: 2 :: 3 :: 4 :: Nil
    }

    ////////////////////////////////////////////////////////////////////////////
    // degrees                                                                //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def degrees01(): Bool = {
        Graph.degrees(graph01()) == Map#{}
    }

    @Test
    def degrees02(): Bool = {
        Graph.degrees(graph02()) == Map#{1 => 1, 2 => 1}
    }

    @Test
    def degrees03(): Bool = {
        Graph.degrees(graph03()) == Map#{1 => 1, 2 => 1, 3 => 1, 4 => 1}
    }

    @Test
    def degrees04(): Bool = {
        Graph.degrees(graph04()) == Map#{1 => 1, 2 => 2, 3 => 1}
    }

    @Test
    def degrees05(): Bool = {
        Graph.degrees(graph05()) == Map#{1 => 1, 2 => 2, 3 => 2, 4 => 1}
    }

    @Test
    def degrees06(): Bool = {
        Graph.degrees(graph06()) == Map#{1 => 2, 2 => 2, 3 => 2, 4 => 2}
    }

    @Test
    def degrees07(): Bool = {
        Graph.degrees(graph07()) == Map#{
            1 => 2, 2 => 3, 3 => 2,
            4 => 2, 5 => 3, 6 => 2
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    // frontiersFrom                                                          //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def frontiersFrom01(): Bool = {
        Graph.frontiersFrom(42, graph01()) == Map#{0 => Set#{42}}
    }

    @Test
    def frontiersFrom02(): Bool = {
        Graph.frontiersFrom(1, graph02()) == Map#{0 => Set#{1}, 1 => Set#{2}}
    }

    @Test
    def frontiersFrom03(): Bool = {
        Graph.frontiersFrom(3, graph03()) == Map#{
            0 => Set#{3},
            1 => Set#{4}
        }
    }

    @Test
    def frontiersFrom04(): Bool = {
        Graph.frontiersFrom(1, graph04()) == Map#{
            0 => Set#{1},
            1 => Set#{2},
            2 => Set#{3}
        }
    }

    @Test
    def frontiersFrom05(): Bool = {
        Graph.frontiersFrom(2, graph05()) == Map#{
            0 => Set#{2},
            1 => Set#{3},
            2 => Set#{4}
        }
    }

    @Test
    def frontiersFrom06(): Bool = {
        Graph.frontiersFrom(3, graph06()) == Map#{
            0 => Set#{3},
            1 => Set#{4},
            2 => Set#{1},
            3 => Set#{2}
        }
    }

    @Test
    def frontiersFrom07(): Bool = {
        Graph.frontiersFrom(4, graph07()) == Map#{
            0 => Set#{4},
            1 => Set#{1},
            2 => Set#{2},
            3 => Set#{3, 5},
            4 => Set#{6}
        }
    }

    @Test
    def frontiersFrom08(): Bool = {
        Graph.frontiersFrom(5, graph07()) == Map#{
            0 => Set#{5},
            1 => Set#{6}
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    // cutPoints                                                              //
    ////////////////////////////////////////////////////////////////////////////

    @Test
    def cutPoints01(): Bool = {
        Graph.cutPoints(graph01()) == Set#{}
    }

    @Test
    def cutPoints02(): Bool = {
        Graph.cutPoints(graph02()) == Set#{}
    }

    @Test
    def cutPoints03(): Bool = {
        Graph.cutPoints(graph03()) == Set#{}
    }

    @Test
    def cutPoints04(): Bool = {
        Graph.cutPoints(graph04()) == Set#{(1, 2, 3)}
    }

    @Test
    def cutPoints05(): Bool = {
        Graph.cutPoints(graph05()) == Set#{
            (1, 2, 3), (1, 3, 4), (2, 3, 4)
        }
    }

    @Test
    def cutPoints06(): Bool = {
        Graph.cutPoints(graph06()) == Set#{
            (1, 2, 3), (1, 3, 4),
            (2, 3, 4), (2, 4, 1),
            (3, 4, 1), (3, 4, 2),
            (4, 1, 2), (4, 2, 3)
        }
    }

    @Test
    def cutPoints07(): Bool = {
        Graph.cutPoints(graph07()) == Set#{
            (1, 2, 3), (1, 3, 4), (1, 2, 5), (1, 5, 6),
            (2, 3, 4), (2, 4, 1), (2, 5, 6),
            (3, 4, 1), (3, 4, 2), (3, 4, 5), (3, 5, 6),
            (4, 1, 2), (4, 2, 3), (4, 2, 5), (4, 5, 6)
        }
    }

}

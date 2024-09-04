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

    @test
    def closure01(): Bool = {
        let g = graph01();
        let s = Set#{};
        Graph.closure(g) == s
    }

    @test
    def closure02(): Bool = {
        let g = graph02();
        let s = Set#{(1, 1), (1, 2), (2, 2)};
        Graph.closure(g) == s
    }

    @test
    def closure03(): Bool = {
        let g = graph03();
        let s = Set#{(1, 1), (1, 2), (2, 2), (3, 3), (3, 4), (4, 4)};
        Graph.closure(g) == s
    }

    @test
    def closure04(): Bool = {
        let g = graph04();
        let s = Set#{(1, 1), (1, 2), (1, 3), (2, 2), (2, 3), (3, 3)};
        Graph.closure(g) == s
    }

    @test
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

    @test
    def reachable01(): Bool = {
        let g = graph01();
        Graph.reachableFrom(1, g) == Set#{1}
    }

    @test
    def reachable02(): Bool = {
        let g = graph02();
        Graph.reachableFrom(1, g) == Set#{1, 2}
    }

    @test
    def reachable03(): Bool = {
        let g = graph03();
        Graph.reachableFrom(1, g) == Set#{1, 2}
    }

    @test
    def reachable04(): Bool = {
        let g = graph04();
        Graph.reachableFrom(1, g) == Set#{1, 2, 3}
    }

    @test
    def reachable05(): Bool = {
        let g = graph05();
        Graph.reachableFrom(1, g) == Set#{1, 2, 3, 4}
    }

    @test
    def reachable06(): Bool = {
        let g = graph05();
        Graph.reachableFrom(3, g) == Set#{3, 4}
    }

    @test
    def reachable07(): Bool = {
        let g = graph05();
        Graph.reachableFrom(5, g) == Set#{5}
    }


    ////////////////////////////////////////////////////////////////////////////
    // unreachableFrom                                                        //
    ////////////////////////////////////////////////////////////////////////////

    @test
    def unreachable01(): Bool = {
        let g = graph01();
        Graph.unreachableFrom(1, g) == Set#{}
    }

    @test
    def unreachable02(): Bool = {
        let g = graph02();
        Graph.unreachableFrom(1, g) == Set#{}
    }

    @test
    def unreachable03(): Bool = {
        let g = graph02();
        Graph.unreachableFrom(2, g) == Set#{1}
    }

    @test
    def unreachable04(): Bool = {
        let g = graph03();
        Graph.unreachableFrom(1, g) == Set#{3, 4}
    }

    @test
    def unreachable05(): Bool = {
        let g = graph04();
        Graph.unreachableFrom(2, g) == Set#{1}
    }

    @test
    def unreachable06(): Bool = {
        let g = graph05();
        Graph.unreachableFrom(3, g) == Set#{1, 2}
    }

    @test
    def unreachable07(): Bool = {
        let g = graph05();
        Graph.unreachableFrom(5, g) == Set#{1, 2, 3, 4}
    }


    ////////////////////////////////////////////////////////////////////////////
    // reachable                                                              //
    ////////////////////////////////////////////////////////////////////////////

    @test
    def isConnected01(): Bool = {
        let g = graph01();
        Graph.reachable(src = 1, dst = 2, g) == false
    }

    @test
    def isConnected02(): Bool = {
        let g = graph02();
        Graph.reachable(src = 1, dst = 2, g)
    }

    @test
    def isConnected03(): Bool = {
        let g = graph02();
        Graph.reachable(src = 2, dst = 1, g) == false
    }

    @test
    def isConnected04(): Bool = {
        let g = graph03();
        Graph.reachable(src = 1, dst = 3, g) == false
    }

    @test
    def isConnected05(): Bool = {
        let g = graph04();
        Graph.reachable(src = 1, dst = 3, g)
    }

    @test
    def isConnected06(): Bool = {
        let g = graph05();
        Graph.reachable(src = 2, dst = 4, g)
    }

    @test
    def isConnected07(): Bool = {
        let g = graph05();
        Graph.reachable(src = 4, dst = 1, g) == false
    }

    @test
    def isConnected08(): Bool = {
        let g = graph05();
        Graph.reachable(src = 5, dst = 6, g) == false
    }


    ////////////////////////////////////////////////////////////////////////////
    // stronglyConnectedComponents                                            //
    ////////////////////////////////////////////////////////////////////////////

    @test
    def stronglyConnectedComponents01(): Bool = {
        let g = graph01();
        let s = Set#{};
        Graph.stronglyConnectedComponents(g) == s
    }

    @test
    def stronglyConnectedComponents02(): Bool = {
        let g = graph02();
        let s = Set#{Set#{1}, Set#{2}};
        Graph.stronglyConnectedComponents(g) == s
    }

    @test
    def stronglyConnectedComponents03(): Bool = {
        let g = graph03();
        let s = Set#{Set#{1}, Set#{2}, Set#{3}, Set#{4}};
        Graph.stronglyConnectedComponents(g) == s
    }

    @test
    def stronglyConnectedComponents04(): Bool = {
        let g = graph04();
        let s = Set#{Set#{1}, Set#{2}, Set#{3}};
        Graph.stronglyConnectedComponents(g) == s
    }

    @test
    def stronglyConnectedComponents05(): Bool = {
        let g = graph05();
        let s = Set#{Set#{1}, Set#{2}, Set#{3}, Set#{4}};
        Graph.stronglyConnectedComponents(g) == s
    }

    @test
    def stronglyConnectedComponents06(): Bool = {
        let g = graph06();
        let s = Set#{Set#{1, 2, 3, 4}};
        Graph.stronglyConnectedComponents(g) == s
    }

    @test
    def stronglyConnectedComponents07(): Bool = {
        let g = graph07();
        let s = Set#{Set#{1, 2, 3, 4}, Set#{5, 6}};
        Graph.stronglyConnectedComponents(g) == s
    }

    ////////////////////////////////////////////////////////////////////////////
    // isCyclic                                                               //
    ////////////////////////////////////////////////////////////////////////////

    @test
    def isCyclic01(): Bool = {
        let g = graph01();
        Graph.isCyclic(g) == false
    }

    @test
    def isCyclic02(): Bool = {
        let g = graph02();
        Graph.isCyclic(g) == false
    }

    @test
    def isCyclic03(): Bool = {
        let g = graph03();
        Graph.isCyclic(g) == false
    }

    @test
    def isCyclic04(): Bool = {
        let g = graph04();
        Graph.isCyclic(g) == false
    }

    @test
    def isCyclic05(): Bool = {
        let g = graph05();
        Graph.isCyclic(g) == false
    }

    @test
    def isCyclic06(): Bool = {
        let g = graph06();
        Graph.isCyclic(g)
    }

    @test
    def isCyclic07(): Bool = {
        let g = graph07();
        Graph.isCyclic(g)
    }

    ////////////////////////////////////////////////////////////////////////////
    // boundedDistances                                                       //
    ////////////////////////////////////////////////////////////////////////////

    @test
    def distances01(): Bool = {
        let g = graphWithDist01();
        Graph.boundedDistances(g) == Map.empty()
    }

    @test
    def distances02(): Bool = {
        let g = graphWithDist02();
        Graph.boundedDistances(g) == Map#{
            (1, 1) => 0,
            (2, 2) => 0,

            (1, 2) => 4
        }
    }

    @test
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

    @test
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

    @test
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

    @test
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

    @test
    def distancesFrom01(): Bool = {
        let g = graphWithDist01();
        Graph.distancesFrom(1, g) == Map#{1 => 0}
    }

    @test
    def distancesFrom02(): Bool = {
        let g = graphWithDist02();
        Graph.distancesFrom(1, g) == Map#{1 => 0, 2 => 4}
    }

    @test
    def distancesFrom03(): Bool = {
        let g = graphWithDist02();
        Graph.distancesFrom(2, g) == Map#{2 => 0}
    }

    @test
    def distancesFrom04(): Bool = {
        let g = graphWithDist03();
        Graph.distancesFrom(3, g) == Map#{3 => 0, 4 => 7}
    }

    @test
    def distancesFrom05(): Bool = {
        let g = graphWithDist04();
        Graph.distancesFrom(2, g) == Map#{2 => 0, 3 => 5, 4 => (5 + 7)}
    }

    @test
    def distancesFrom06(): Bool = {
        let g = graphWithDist05();
        Graph.distancesFrom(4, g) == Map#{
            1 => 5,
            2 => (5 + 4),
            3 => (5 + 4 + 2),
            4 => 0
        }
    }

    @test
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

    @test
    def distance01(): Bool = {
        let g = graphWithDist01();
        Graph.distance(src = 1, dst = 2, g) == None
    }

    @test
    def distance02(): Bool = {
        let g = graphWithDist02();
        Graph.distance(src = 1, dst = 2, g) == Some(4)
    }

    @test
    def distance03(): Bool = {
        let g = graphWithDist02();
        Graph.distance(src = 2, dst = 1, g) == None
    }

    @test
    def distance04(): Bool = {
        let g = graphWithDist03();
        Graph.distance(src = 1, dst = 2, g) == Some(4)
    }

    @test
    def distance05(): Bool = {
        let g = graphWithDist03();
        Graph.distance(src = 3, dst = 4, g) == Some(7)
    }

    @test
    def distance06(): Bool = {
        let g = graphWithDist03();
        Graph.distance(src = 1, dst = 4, g) == None
    }

    @test
    def distance07(): Bool = {
        let g = graphWithDist04();
        Graph.distance(src = 1, dst = 4, g) == Some(4 + 5 + 7)
    }

    @test
    def distance08(): Bool = {
        let g = graphWithDist05();
        Graph.distance(src = 4, dst = 2, g) == Some(5 + 4)
    }

    @test
    def distance09(): Bool = {
        let g = graphWithDist06();
        Graph.distance(src = 1, dst = 6, g) == Some(5 + 2 + 1)
    }

    ////////////////////////////////////////////////////////////////////////////
    // toUndirected                                                           //
    ////////////////////////////////////////////////////////////////////////////

    @test
    def toUndirected01(): Bool = {
        let g = graph01() |> Graph.toUndirected;
        g == Set#{}
    }

    @test
    def toUndirected02(): Bool = {
        let g = graph02() |> Graph.toUndirected;
        g == Set#{(1, 2), (2, 1)}
    }

    @test
    def toUndirected03(): Bool = {
        let g = graph03() |> Graph.toUndirected;
        g == Set#{(1, 2), (2, 1), (3, 4), (4, 3)}
    }

    @test
    def toUndirected04(): Bool = {
        let g = graph04() |> Graph.toUndirected;
        g == Set#{(1, 2), (2, 1), (2, 3), (3, 2)}
    }

    @test
    def toUndirected05(): Bool = {
        let g = graph05() |> Graph.toUndirected;
        g == Set#{(1, 2), (2, 1), (2, 3), (3, 2), (3, 4), (4, 3)}
    }

    @test
    def toUndirected06(): Bool = {
        let g = graph06() |> Graph.toUndirected;
        g == Set#{
            (1, 2), (2, 1),
            (2, 3), (3, 2),
            (3, 4), (4, 3),
            (4, 1), (1, 4)
        }
    }

    @test
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
    // toUndirectedLabeled                                                   //
    ////////////////////////////////////////////////////////////////////////////

    @test
    def toUndirectedLabeled01(): Bool = {
        let g = graphWithDist01() |> Graph.toUndirectedLabeled;
        g == Set#{}
    }

    @test
    def toUndirectedLabeled02(): Bool = {
        let g = graphWithDist02() |> Graph.toUndirectedLabeled;
        g == Set#{(1, 4, 2), (2, 4, 1)}
    }

    @test
    def toUndirectedLabeled03(): Bool = {
        let g = graphWithDist03() |> Graph.toUndirectedLabeled;
        g == Set#{
            (1, 4, 2), (2, 4, 1),
            (3, 7, 4), (4, 7, 3)
        }
    }

    @test
    def toUndirectedLabeled04(): Bool = {
        let g = graphWithDist04() |> Graph.toUndirectedLabeled;
        g == Set#{
            (1, 4, 2), (2, 4, 1),
            (2, 5, 3), (3, 5, 2),
            (3, 7, 4), (4, 7, 3)
        }
    }

    @test
    def toUndirectedLabeled05(): Bool = {
        let g = graphWithDist05() |> Graph.toUndirectedLabeled;
        g == Set#{
            (1, 4, 2), (2, 4, 1),
            (1, 5, 4), (4, 5, 1),
            (2, 2, 3), (3, 2, 2),
            (3, 1, 4), (4, 1, 3)
        }
    }

    @test
    def toUndirectedLabeled06(): Bool = {
        let g = graphWithDist06() |> Graph.toUndirectedLabeled;
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

    @test
    def toUndirectedLabeled07(): Bool = {
        let g0 = Set#{(7, 9, 13), (13, 8, 7)};
        let g = g0 |> Graph.toUndirectedLabeled;
        g == Set#{
            (7, 9, 13), (13, 9, 7),
            (7, 8, 13), (13, 8, 7)
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    // inDegrees                                                              //
    ////////////////////////////////////////////////////////////////////////////

    @test
    def inDegrees01(): Bool = {
        Graph.inDegrees(graph01()) == Map#{}
    }

    @test
    def inDegrees02(): Bool = {
        Graph.inDegrees(graph02()) == Map#{
            1 => 0,
            2 => 1
        }
    }

    @test
    def inDegrees03(): Bool = {
        Graph.inDegrees(graph03()) == Map#{
            1 => 0,
            2 => 1,
            3 => 0,
            4 => 1
        }
    }

    @test
    def inDegrees04(): Bool = {
        Graph.inDegrees(graph04()) == Map#{
            1 => 0,
            2 => 1,
            3 => 1
        }
    }

    @test
    def inDegrees05(): Bool = {
        Graph.inDegrees(graph05()) == Map#{
            1 => 0,
            2 => 1,
            3 => 1,
            4 => 1
        }
    }

    @test
    def inDegrees06(): Bool = {
        Graph.inDegrees(graph06()) == Map#{
            1 => 1,
            2 => 1,
            3 => 1,
            4 => 1
        }
    }

    @test
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

    @test
    def outDegrees01(): Bool = {
        Graph.outDegrees(graph01()) == Map#{}
    }

    @test
    def outDegrees02(): Bool = {
        Graph.outDegrees(graph02()) == Map#{
            1 => 1,
            2 => 0
        }
    }

    @test
    def outDegrees03(): Bool = {
        Graph.outDegrees(graph03()) == Map#{
            1 => 1,
            2 => 0,
            3 => 1,
            4 => 0
        }
    }

    @test
    def outDegrees04(): Bool = {
        Graph.outDegrees(graph04()) == Map#{
            1 => 1,
            2 => 1,
            3 => 0
        }
    }

    @test
    def outDegrees05(): Bool = {
        Graph.outDegrees(graph05()) == Map#{
            1 => 1,
            2 => 1,
            3 => 1,
            4 => 0
        }
    }

    @test
    def outDegrees06(): Bool = {
        Graph.outDegrees(graph06()) == Map#{
            1 => 1,
            2 => 1,
            3 => 1,
            4 => 1
        }
    }

    @test
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

    @test
    def toGraphviz01(): Bool = {
        Graph.toGraphviz(graph01()) == String.unlines(
            "digraph {" ::
            "}" ::
            Nil
        )
    }

    @test
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

    @test
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
    // toGraphvizLabeled                                                     //
    ////////////////////////////////////////////////////////////////////////////

    @test
    def toGraphvizLabeled01(): Bool = {
        Graph.toGraphvizLabeled(graphWithDist01()) == String.unlines(
            "digraph {" ::
            "}" ::
            Nil
        )
    }

    @test
    def toGraphvizLabeled02(): Bool = {
        let g = Set#{
            ("node \"A\"", -42, "node \"B\"")
        };
        Graph.toGraphvizLabeled(g) == String.unlines(
            "digraph {" ::
            "  \"node \\\"A\\\"\" -> \"node \\\"B\\\"\" [label = -42]" ::
            "}" ::
            Nil
        )
    }

    @test
    def toGraphvizLabeled03(): Bool = {
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
        test1 or test2
    }

    ////////////////////////////////////////////////////////////////////////////
    // flipEdges                                                              //
    ////////////////////////////////////////////////////////////////////////////

    @test
    def flipEdges01(): Bool = {
        Graph.flipEdges(graph01()) == Set#{}
    }

    @test
    def flipEdges02(): Bool = {
        Graph.flipEdges(graph02()) == Set#{(2, 1)}
    }

    @test
    def flipEdges03(): Bool = {
        Graph.flipEdges(graph03()) == Set#{(2, 1), (4, 3)}
    }

    @test
    def flipEdges04(): Bool = {
        Graph.flipEdges(graph04()) == Set#{(2, 1), (3, 2)}
    }

    @test
    def flipEdges05(): Bool = {
        Graph.flipEdges(graph05()) == Set#{(2, 1), (3, 2), (4, 3)}
    }

    @test
    def flipEdges06(): Bool = {
        Graph.flipEdges(graph06()) == Set#{(2, 1), (3, 2), (4, 3), (1, 4)}
    }

    @test
    def flipEdges07(): Bool = {
        Graph.flipEdges(graph07()) == Set#{
            (2, 1), (3, 2), (4, 3), (1, 4), (5, 2), (6, 5), (5, 6)
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    // invert                                                                 //
    ////////////////////////////////////////////////////////////////////////////

    @test
    def invert01(): Bool = {
        Graph.invert(graph01()) == Set#{}
    }

    @test
    def invert02(): Bool = {
        Graph.invert(graph02()) == Set#{(2, 1)}
    }

    @test
    def invert03(): Bool = {
        Graph.invert(graph03()) == Set#{
            (1, 3), (1, 4),
            (2, 1), (2, 3), (2, 4),
            (3, 1), (3, 2),
            (4, 1), (4, 2), (4, 3)
        }
    }

    @test
    def invert04(): Bool = {
        Graph.invert(graph04()) == Set#{
            (1, 3),
            (2, 1),
            (3, 1), (3, 2)
        }
    }

    @test
    def invert05(): Bool = {
        Graph.invert(graph05()) == Set#{
            (1, 3), (1, 4),
            (2, 1), (2, 4),
            (3, 1), (3, 2),
            (4, 1), (4, 2), (4, 3)
        }
    }

    @test
    def invert06(): Bool = {
        Graph.invert(graph06()) == Set#{
            (1, 3), (1, 4),
            (2, 1), (2, 4),
            (3, 1), (3, 2),
            (4, 2), (4, 3)
        }
    }

    @test
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

    @test
    def invert08(): Bool = {
        Graph.invert(Set#{(1, 1)}) == Set#{}
    }

    @test
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

    @test
    def stableDistances01(): Bool = {
        let g = graphWithDist01();
        Graph.distances(g) == Some(Graph.boundedDistances(g))
    }

    @test
    def stableDistances02(): Bool = {
        let g = graphWithDist02();
        Graph.distances(g) == Some(Graph.boundedDistances(g))
    }

    @test
    def stableDistances03(): Bool = {
        let g = graphWithDist03();
        Graph.distances(g) == Some(Graph.boundedDistances(g))
    }

    @test
    def stableDistances04(): Bool = {
        let g = graphWithDist04();
        Graph.distances(g) == Some(Graph.boundedDistances(g))
    }

    @test
    def stableDistances05(): Bool = {
        let g = graphWithDist05();
        Graph.distances(g) == Some(Graph.boundedDistances(g))
    }

    @test
    def stableDistances06(): Bool = {
        let g = graphWithDist06();
        Graph.distances(g) == Some(Graph.boundedDistances(g))
    }

    @test
    def stableDistances07(): Bool = {
        let g = Set#{("a", 1, "b"), ("b", 2, "c"), ("c", -4, "a")};
        Graph.distances(g) == None
    }

    ////////////////////////////////////////////////////////////////////////////
    // withinDistanceOf                                                       //
    ////////////////////////////////////////////////////////////////////////////

    @test
    def withinDistanceOf01(): Bool = {
        Graph.withinDistanceOf(42, 999, graphWithDist01()) == Set#{42}
    }

    @test
    def withinDistanceOf02(): Bool = {
        Graph.withinDistanceOf(1, -42, graphWithDist02()) == Set#{}
    }

    @test
    def withinDistanceOf03(): Bool = {
        Graph.withinDistanceOf(1, 3, graphWithDist02()) == Set#{1}
    }

    @test
    def withinDistanceOf04(): Bool = {
        Graph.withinDistanceOf(1, 4, graphWithDist02()) == Set#{1, 2}
    }

    @test
    def withinDistanceOf05(): Bool = {
        Graph.withinDistanceOf(1, 99, graphWithDist02()) == Set#{1, 2}
    }

    @test
    def withinDistanceOf06(): Bool = {
        Graph.withinDistanceOf(3, 7, graphWithDist03()) == Set#{3, 4}
    }

    @test
    def withinDistanceOf07(): Bool = {
        Graph.withinDistanceOf(1, 11, graphWithDist04()) == Set#{1, 2, 3}
    }

    @test
    def withinDistanceOf08(): Bool = {
        Graph.withinDistanceOf(2, 7, graphWithDist05()) == Set#{2, 3, 4}
    }

    @test
    def withinDistanceOf09(): Bool = {
        Graph.withinDistanceOf(4, 3, graphWithDist06()) == Set#{4, 5, 6}
    }

    ////////////////////////////////////////////////////////////////////////////
    // withinEdgesOf                                                          //
    ////////////////////////////////////////////////////////////////////////////

    @test
    def withinEdgesOf01(): Bool = {
        Graph.withinEdgesOf(42, 999, graph01()) == Set#{42}
    }

    @test
    def withinEdgesOf02(): Bool = {
        Graph.withinEdgesOf(1, -42, graph02()) == Set#{}
    }

    @test
    def withinEdgesOf03(): Bool = {
        Graph.withinEdgesOf(1, 1, graph02()) == Set#{1, 2}
    }

    @test
    def withinEdgesOf04(): Bool = {
        Graph.withinEdgesOf(1, 4, graph02()) == Set#{1, 2}
    }

    @test
    def withinEdgesOf05(): Bool = {
        Graph.withinEdgesOf(3, 1, graph03()) == Set#{3, 4}
    }

    @test
    def withinEdgesOf06(): Bool = {
        Graph.withinEdgesOf(1, 2, graph04()) == Set#{1, 2, 3}
    }

    @test
    def withinEdgesOf07(): Bool = {
        Graph.withinEdgesOf(1, 2, graph05()) == Set#{1, 2, 3}
    }

    @test
    def withinEdgesOf08(): Bool = {
        Graph.withinEdgesOf(4, 1, graph06()) == Set#{1, 4}
    }

    @test
    def withinEdgesOf09(): Bool = {
        Graph.withinEdgesOf(1, 2, graph07()) == Set#{1, 2, 3, 5}
    }

    ////////////////////////////////////////////////////////////////////////////
    // topologicalSort                                                        //
    ////////////////////////////////////////////////////////////////////////////

    @test
    def topologicalSort01(): Bool = {
        Graph.topologicalSort(graph01()) == Nil
    }

    @test
    def topologicalSort02(): Bool = {
        Graph.topologicalSort(graph02()) == 1 :: 2 :: Nil
    }

    @test
    def topologicalSort03(): Bool = {
        Graph.topologicalSort(graph03()) == 1 :: 3 :: 2 :: 4 :: Nil
    }

    @test
    def topologicalSort04(): Bool = {
        Graph.topologicalSort(graph04()) == 1 :: 2 :: 3 :: Nil
    }

    @test
    def topologicalSort05(): Bool = {
        Graph.topologicalSort(graph05()) == 1 :: 2 :: 3 :: 4 :: Nil
    }

    ////////////////////////////////////////////////////////////////////////////
    // degrees                                                                //
    ////////////////////////////////////////////////////////////////////////////

    @test
    def degrees01(): Bool = {
        Graph.degrees(graph01()) == Map#{}
    }

    @test
    def degrees02(): Bool = {
        Graph.degrees(graph02()) == Map#{1 => 1, 2 => 1}
    }

    @test
    def degrees03(): Bool = {
        Graph.degrees(graph03()) == Map#{1 => 1, 2 => 1, 3 => 1, 4 => 1}
    }

    @test
    def degrees04(): Bool = {
        Graph.degrees(graph04()) == Map#{1 => 1, 2 => 2, 3 => 1}
    }

    @test
    def degrees05(): Bool = {
        Graph.degrees(graph05()) == Map#{1 => 1, 2 => 2, 3 => 2, 4 => 1}
    }

    @test
    def degrees06(): Bool = {
        Graph.degrees(graph06()) == Map#{1 => 2, 2 => 2, 3 => 2, 4 => 2}
    }

    @test
    def degrees07(): Bool = {
        Graph.degrees(graph07()) == Map#{
            1 => 2, 2 => 3, 3 => 2,
            4 => 2, 5 => 3, 6 => 2
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    // frontiersFrom                                                          //
    ////////////////////////////////////////////////////////////////////////////

    @test
    def frontiersFrom01(): Bool = {
        Graph.frontiersFrom(42, graph01()) == Map#{0 => Set#{42}}
    }

    @test
    def frontiersFrom02(): Bool = {
        Graph.frontiersFrom(1, graph02()) == Map#{0 => Set#{1}, 1 => Set#{2}}
    }

    @test
    def frontiersFrom03(): Bool = {
        Graph.frontiersFrom(3, graph03()) == Map#{
            0 => Set#{3},
            1 => Set#{4}
        }
    }

    @test
    def frontiersFrom04(): Bool = {
        Graph.frontiersFrom(1, graph04()) == Map#{
            0 => Set#{1},
            1 => Set#{2},
            2 => Set#{3}
        }
    }

    @test
    def frontiersFrom05(): Bool = {
        Graph.frontiersFrom(2, graph05()) == Map#{
            0 => Set#{2},
            1 => Set#{3},
            2 => Set#{4}
        }
    }

    @test
    def frontiersFrom06(): Bool = {
        Graph.frontiersFrom(3, graph06()) == Map#{
            0 => Set#{3},
            1 => Set#{4},
            2 => Set#{1},
            3 => Set#{2}
        }
    }

    @test
    def frontiersFrom07(): Bool = {
        Graph.frontiersFrom(4, graph07()) == Map#{
            0 => Set#{4},
            1 => Set#{1},
            2 => Set#{2},
            3 => Set#{3, 5},
            4 => Set#{6}
        }
    }

    @test
    def frontiersFrom08(): Bool = {
        Graph.frontiersFrom(5, graph07()) == Map#{
            0 => Set#{5},
            1 => Set#{6}
        }
    }

    ////////////////////////////////////////////////////////////////////////////
    // cutPoints                                                              //
    ////////////////////////////////////////////////////////////////////////////

    @test
    def cutPoints01(): Bool = {
        Graph.cutPoints(graph01()) == Set#{}
    }

    @test
    def cutPoints02(): Bool = {
        Graph.cutPoints(graph02()) == Set#{}
    }

    @test
    def cutPoints03(): Bool = {
        Graph.cutPoints(graph03()) == Set#{}
    }

    @test
    def cutPoints04(): Bool = {
        Graph.cutPoints(graph04()) == Set#{(1, 2, 3)}
    }

    @test
    def cutPoints05(): Bool = {
        Graph.cutPoints(graph05()) == Set#{
            (1, 2, 3), (1, 3, 4), (2, 3, 4)
        }
    }

    @test
    def cutPoints06(): Bool = {
        Graph.cutPoints(graph06()) == Set#{
            (1, 2, 3), (1, 3, 4),
            (2, 3, 4), (2, 4, 1),
            (3, 4, 1), (3, 4, 2),
            (4, 1, 2), (4, 2, 3)
        }
    }

    @test
    def cutPoints07(): Bool = {
        Graph.cutPoints(graph07()) == Set#{
            (1, 2, 3), (1, 3, 4), (1, 2, 5), (1, 5, 6),
            (2, 3, 4), (2, 4, 1), (2, 5, 6),
            (3, 4, 1), (3, 4, 2), (3, 4, 5), (3, 5, 6),
            (4, 1, 2), (4, 2, 3), (4, 2, 5), (4, 5, 6)
        }
    }

}

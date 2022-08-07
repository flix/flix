namespace Reachable/Imperative {
    pub def reachable(origin: node, edges: f[(node, node)]): Set[node] with Foldable[f], Order[node] =
        region r {
            // collect edge lists
            let edgeMap: MutMap[node, MutList[node, r], r] = new MutMap(r);
            foreach((start, end) <- Foldable.iterator(r, edges)) {
                let startEdges = MutMap.getOrElsePut!(start, new MutList(r), edgeMap);
                MutList.push!(end, startEdges)
            };
            // define the reachable set
            let reachable = new MutSet(r);
            // explore graph depth first by task list
            let taskList = new MutDeque(r);
            taskList |> MutDeque.pushFront(origin);
            def whileLoop() = {
                MutDeque.popFront(taskList) |> Option.foreach(node -> {
                    // this node has now been reached
                    reachable |> MutSet.add!(node);
                    // add all non-reached end points to tasklist
                    let endPoints = MutMap.getWithDefault(node, new MutList(r), edgeMap);
                    foreach(nextNode <- Iterable.iterator(endPoints)) {
                        let alreadyReached = MutSet.exists(Eq.eq(nextNode), reachable);
                        if (not alreadyReached)
                            taskList |> MutDeque.pushBack(nextNode)
                        else ()
                    };
                    whileLoop()
                })
            };
            whileLoop();
            MutSet.toSet(reachable)
        }

    @test
    pub def test(): Bool = Graphs.validate(reachable)
}

namespace Reachable/Functional {
    pub def reachable(origin: node, edges: f[(node, node)]): Set[node] with Foldable[f], Order[node] =
        // collect edge lists
        let edgeMap = (Map.empty(), edges) ||> Foldable.foldLeft(map -> match (start, end) -> {
            let startEdges = Map.getWithDefault(start, Nil, map);
            Map.insert(start, end :: startEdges, map)
        });
        // explore the graph depth first
        def dfs(node: node, reachable: Set[node]) = {
            // this node has now been reached
            let reachable1 = Set.insert(node, reachable);
            let endPoints = Map.getWithDefault(node, Nil, edgeMap);
            // visit all non-reached end points and collects their sets
            (reachable1, endPoints) ||> List.foldLeft(
                (acc, nextNode) ->
                    let alreadyReached = Set.exists(Eq.eq(nextNode), reachable);
                    if (not alreadyReached)
                        dfs(nextNode, acc)
                    else acc
            )
        };
        dfs(origin, Set.empty())

    @test
    pub def test(): Bool = Graphs.validate(reachable)
}

namespace Reachable/Logic {
    // Note that Boxable includes Order
    pub def reachable(origin: node, edges: f[(node, node)]): Set[node] with Foldable[f], Boxable[node] =
        let edgeFacts = project edges into Edge;
        // origin is reachable and this set is expanded by the edge facts
        let reachable = #{
            Reachable(origin).
            Reachable(end) :- Reachable(start), Edge(start, end).
        };
        query edgeFacts, reachable select x from Reachable(x) |> List.toSet

    @test
    pub def test(): Bool = Graphs.validate(reachable)
}

namespace Graphs {
    pub def graph1(): Set[(Int32, Int32)] = Set#{}

    pub def graph2(): Set[(Int32, Int32)] = Set#{
        (1, 2), (2, 3), (3, 4), (4, 1)
    }

    pub def graph3(): Set[(Int32, Int32)] = Set#{
        (1, 2), (1, 3), (4, 5)
    }

    pub def graph4(): Set[(Int32, Int32)] = Set#{
        (2, 3), (3, 4)
    }

    pub def graph5(): Set[(Int32, Int32)] = Set#{
        (4, 5), (5, 6), (4, 3), (4, 2), (12, 13), (29, 4)
    }

    pub def validate(reachable: Int32 -> Set[(Int32, Int32)] -> Set[Int32]): Bool = {
        let test1 = reachable(42, Graphs.graph1()) == Set#{42};
        let test2 = reachable(1, Graphs.graph2()) == Set#{1, 2, 3, 4};
        let test3 = reachable(1, Graphs.graph3()) == Set#{1, 2, 3};
        let test4 = reachable(1, Graphs.graph4()) == Set#{1};
        let test5 = reachable(4, Graphs.graph5()) == Set#{2, 3, 4, 5, 6};
        test1 and test2 and test3 and test4 and test5
    }
}

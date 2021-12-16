def isCyclic(edges: List[(Int, Int)]): Bool =
    let e = project edges into Edge;
    let lp = #{
        Path(x, y) :- Edge(x, y).
        Path(x, z) :- Path(x, y), Edge(y, z).
        // Cycle as a predicate for further logic programming
        // Cycle() :- Path(x, x).
    };
    let cycleNodes = query e, lp select x from Path(x, x);
    cycleNodes.length > 0

pub def pairwiseAcyclic(graphs: List[List[(Int, Int)]]): List[List[List[(Int, Int)]]] =
    graphs |> List.groupBy((g1, g2) -> not isCyclic(g1 ::: g2))

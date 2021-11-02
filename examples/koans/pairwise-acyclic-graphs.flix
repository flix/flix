def isCyclic(edges: Array[(Int, Int)]): Bool =
    let e = project edges into Edge;
    let lp = #{
        Path(x, y) :- Edge(x, y).
        Path(x, z) :- Path(x, y), Edge(y, z).
        // Cycle as a predicate for further logic programming
        // Cycle() :- Path(x, x).
    };
    let cycleNodes = query e, lp select x from Path(x, x);
    cycleNodes.length > 0

pub def pairwiseAcyclic(graphs: List[Array[(Int, Int)]]): List[List[Array[(Int, Int)]]] =
    let combineGraphs = (g1, g2) -> query (project g1 into Edge) <+> (project g2 into Edge) select (x, y) from Edge(x, y);
    graphs |> List.groupBy((g1, g2) -> not isCyclic(combineGraphs(g1, g2)))

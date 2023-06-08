def isCyclic(edges: List[(Int32, Int32)]): Bool =
    let e = inject edges into Edge;
    let lp = #{
        Path(x, y) :- Edge(x, y).
        Path(x, z) :- Path(x, y), Edge(y, z).
        // Cycle as a predicate for further logic programming
        // Cycle() :- Path(x, x).
    };
    let cycleNodes = query e, lp select x from Path(x, x);
    Vector.length(cycleNodes) > 0

pub def pairwiseAcyclic(graphs: List[List[(Int32, Int32)]]): List[List[List[(Int32, Int32)]]] =
    graphs |> List.groupBy((g1, g2) -> not isCyclic(g1 ::: g2))

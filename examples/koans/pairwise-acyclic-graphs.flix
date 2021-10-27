def isCyclic(g: #{Edge(Int, Int)}): Bool =
    let p = #{
        Path(x, y) :- Edge(x, y).
        Path(x, z) :- Path(x, y), Edge(y, z).
        Cyclic() :- Path(x, x).
    };
    (solve g <+> p) |= Cyclic().

def pairwiseAcyclic(l: List[#{Edge(Int, Int)}]): List[List[#{Edge(Int, Int)}]] =
    l |> List.groupBy((g1, g2) -> not isCyclic(g1 <+> g2))

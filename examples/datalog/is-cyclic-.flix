///
/// Returns `true` if the given graph `edges` has a cycle.
///
def isCyclic(edges: List[(t, t)]): Bool with Order[t] =
    let db = inject edges into Edge/2;
    let pr = #{
        Path(x, y) :- Edge(x, y).
        Path(x, z) :- Path(x, y), Edge(y, z).
        Cycle :- Path(x, x).
    };
    query db, pr select () from Cycle |> Vector.nonEmpty

///
/// Runs `isCyclic` on a small graph.
///
def main(): Unit \ IO =
    let edges = List#{(1, 2), (2, 3), (3, 4), (4, 2)};
    println(isCyclic(edges))

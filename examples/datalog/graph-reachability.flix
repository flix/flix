///
/// Returns all vertices reachable from the given `origin` using the given `edges`.
///
def reachable(origin: t, edges: List[(t, t)]): Set[t] with Order[t] =
    let db = inject edges into Edge/2;
    let pr = #{
        Reachable(origin).
        Reachable(y) :- Reachable(x), Edge(x, y).
    };
    query db, pr select x from Reachable(x) |> Vector.toSet

///
/// Runs `reachable` on a small graph.
///
def main(): Unit \ IO =
    let edges = List#{(1, 2), (2, 3), (4, 5), (5, 6)};
    println(reachable(4, edges))

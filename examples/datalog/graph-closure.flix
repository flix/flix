///
/// Returns the transitive closure of the given set of `edges`,  i.e. all pairs
/// of vertices `(src, dst)` where there is a path between `src` and `dst`.
///
def closure(edges: List[(t, t)]): Set[(t, t )] with Order[t] =
    let db = inject edges into Edge/2;
    let pr = #{
        Path(x, y) :- Edge(x, y).
        Path(x, z) :- Path(x, y), Edge(y, z).
    };
    query db, pr select (x, y) from Path(x, y) |> Vector.toSet

///
/// Runs `paths` on a small graph.
///
def main(): Unit \ IO =
    let edges = List#{(1, 2), (2, 3), (4, 5), (5, 6)};
    println(closure(edges))

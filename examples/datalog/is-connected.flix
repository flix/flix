///
/// Returns `true` if there is path from `src` to `dst` using the given `edges`.
///
def isConnected(src: t, dst: t, edges: List[(t, t)]): Bool with Order[t] =
    let db = inject edges into Edge/2;
    let pr = #{
        Path(x, y) :- Edge(x, y).
        Path(x, z) :- Path(x, y), Edge(y, z).
    };
    query db, pr select () from Path(src, dst) |> Vector.nonEmpty

///
/// Runs `isConnected` on a small graph.
///
def main(): Unit \ IO =
    let edges = List#{(1, 2), (2, 3), (3, 4), (5, 6)};
    println(isConnected(1, 4, edges))

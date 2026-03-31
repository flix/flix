///
/// Optionally returns a cycle in the graph formed by the given `edges`.
///
def cycle(edges: List[(t, t)]): Option[Vector[t]] with Order[t] =
    let db = inject edges into Edge/2;
    let pr = #{
        Path(x, y) :- Edge(x, y).
        Path(x, z) :- Path(x, y), Edge(y, z).
        Cycle(x)   :- Path(x, x).
    };
    let result = query db, pr select x from Cycle(x);
    match Vector.head(result) {
        case None    => None
        case Some(x) =>
            let p = pquery db, pr select Cycle(x) with {Edge};
            let l = Vector.flatMap(ematch Edge(src, dst) -> Vector#{src, dst}, p);
            Some(l |> Vector.removeAdjDups)
    }

///
/// Runs `cycle` on a graph with a cycle.
///
def main(): Unit \ IO =
    let edges = List#{(1, 2), (2, 3), (3, 1), (4, 5), (5, 6)};
    println(cycle(edges))

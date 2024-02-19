mod ShortestPath {

    use Down.Down;

    ///
    /// Returns a mapping from distances to the set of reachable vertices for which the
    /// shortest path from `src` in the directed graph `g` is of a given length.
    ///
    pub def sssp(src: t, g: m[(t, t)]): Map[Int32, Set[t]] with Foldable[m], Order[t] = {
        let edges = inject g into Edge;
        let frontiers = #{
            Dist(src; Down(0)).
            Dist(y; n + Down(1)) :- Dist(x; n), Edge(x, y).

            // Find the max frontier.
            Frontiers(;Set#{coerce(n)}) :- fix Dist(_; n).
            MaxFrontier(Set.maximum(s) |> Option.getWithDefault(0)) :- fix Frontiers(;s).

            // Initialize all frontiers (with non-bot element).
            Frontier(0; Some(Set#{})) :- MaxFrontier(m), if m > 0.
            Frontier(n+1; Some(Set#{})) :- Frontier(n; _), MaxFrontier(m), if n < m.

            // Collect the frontiers.
            Frontier(coerce(n); Some(Set#{x})) :- fix Dist(x; n).
        };
        let res = query edges, frontiers select (n, s) from Frontier(n; s);
        let unwrapOption = match (n, s) -> match s {
            case Some(v) => Some((n, v))
            case None => None
        };
        res |> Vector.filterMap(unwrapOption)
            |> Vector.toMap
    }

}

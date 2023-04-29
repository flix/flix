mod BenchmarkConstraint {

    rel DirectedEdge(x: Int32, y: Int32)
    rel DirectedPath(x: Int32, y: Int32)

    ///
    /// create a set of facts.
    ///
    @benchmark
    pub def benchmark01(): Bool =
        let fold = (acc: #{ DirectedEdge }, s: Int32) -> {
            let d = s + 1;
            acc <+> #{ DirectedEdge(s, d). }
        };
        let facts = List.range(1, 20) |> List.foldLeft(fold, #{});
        query facts select true from DirectedEdge(1, 2) |> List.isEmpty |> Bool.not

    ///
    /// create a set of facts and compute the symmetric facts.
    ///
    @benchmark
    pub def benchmark02(): Bool =
        let fold = (acc: #{ DirectedEdge }, s: Int32) -> {
            let d = s + 1;
            acc <+> #{ DirectedEdge(s, d). }
        };
        let facts = List.range(1, 20) |> List.foldLeft(fold, #{});
        let rules = #{
            DirectedEdge(y, x) :- DirectedEdge(x, y).
        };
        query facts <+> rules select true from DirectedEdge(1, 2) |> List.isEmpty |> Bool.not

    ///
    /// create a set of facts and compute the transitive closure.
    ///
    @benchmark
    pub def benchmark03(): Bool =
        let fold = (acc: #{ DirectedEdge, DirectedPath }, s: Int32) -> {
            let d = s + 1;
            acc <+> #{ DirectedEdge(s, d). }
        };
        let facts = List.range(1, 20) |> List.foldLeft(fold, #{});
        let rules = #{
            DirectedPath(x, y) :- DirectedPath(x, y).
            DirectedPath(x, z) :- DirectedPath(x, y), DirectedEdge(y, z).
        };
        query facts <+> rules select true from DirectedEdge(1, 20) |> List.isEmpty |> Bool.not

}

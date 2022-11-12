/*
 *  Copyright 2022 Nina Andrup Pedersen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
namespace Graph {

    ///
    /// Returns the transitive closure of the directed graph `g`.
    ///
    pub def closure(g: m[(t, t)]): Set[(t, t)] with Foldable[m], Boxable[t] = {
        let edges = inject g into Edge;
        let res = query edges, nodes(), reachability()
                  select (src, dst)
                  from Reachable(src, dst);
        List.toSet(res)
    }

    ///
    /// Returns the vertices that are reachable from the `src` in the directed
    /// graph `g`.
    ///
    pub def reachable(src: t, g: m[(t, t)]): Set[t] with Foldable[m], Boxable[t] = {
        let edges = inject g into Edge;
        let res = query edges, reachabilityFromSrc(src)
                  select dst
                  from Reachable(dst);
        List.toSet(res)
    }

    ///
    /// Returns the vertices that are unreachable from the `src` in the directed
    /// graph `g`.
    ///
    pub def unreachable(src: t, g: m[(t, t)]): Set[t] with Foldable[m], Boxable[t] = {
        let edges = inject g into Edge;
        let unreachablility = #{
            // If there is a node that is not Reachable from src then it is
            // unreachable.
            UnReachable(x) :- Node(x), not Reachable(x).
        };
        let res = query edges, nodes(), reachabilityFromSrc(src), unreachablility
                  select dst
                  from UnReachable(dst);
        List.toSet(res)
    }

    ///
    /// Returns `true` if there is a path from `src` to `dst` in the directed
    /// graph `g`.
    ///
    pub def isConnected(src: {src = t}, dst: {dst = t}, g: m[(t, t)]): Bool with Foldable[m], Boxable[t] =
        reachable(src.src, g) |> Set.exists(x -> dst.dst == x)

    ///
    /// Returns the strongly connected components of the directed graph `g`.
    ///
    pub def stronglyConnectedComponents(g: m[(t, t)]): Set[Set[t]] with Foldable[m], Boxable[t] = {
        let edges = inject g into Edge;
        let connected = #{
            // If `n1` can reach `n2` and `n2` can reach `n1` then they are part
            // of the same strongly connected component.
            Connected(n1; Set#{n2}) :- Reachable(n1, n2), Reachable(n2, n1).
        };
        let components = #{
            // After the full computation of Connected, dublicates are removed
            // by checking `n` is the minimum in the strongly connected
            // component.
            Components(s) :- fix Connected(n; s), if Some(n) == Set.minimum(s).
        };
        let res = query edges, nodes(), reachability(), connected, components
                  select x
                  from Components(x);
        List.toSet(res)
    }

    ///
    /// Returns `true` if the directed graph `g` contains at least one cycle.
    ///
    pub def isCyclic(g: m[(t, t)]): Bool with Foldable[m], Boxable[t] = {
        let edges = inject g into Edge;
        let reachability = #{
            // Reachability given the edges.
            Reachable(n1, n2) :- Edge(n1, n2), if n1 != n2.
            // If `n1` can reach `m` and there is an edge from `m` to `n2` then
            // `n1` can also reach `n2`. This adds all node pairs to the
            // relational that are reachable using any number of nodes.
            Reachable(n1, n2) :- Reachable(n1, m), Edge(m, n2).
        };
        let res = query edges, reachability
                  select ()
                  from Reachable(x, y)
                  where x == y;
        List.length(res) > 0
    }

    ///
    /// Returns the shortest distance between all pairs of vertices in the
    /// weighted directed graph `g`.
    ///
    pub def distances(g: m[(t, Int32, t)]): Map[(t, t), Int32] with Foldable[m], Boxable[t] = {
        let edges = inject g into Edge;
        let dists = #{
            // Initialize all node distances to `0`.
            Dist(x, x; Down(0)) :- Edge(x, _, _).
            Dist(x, x; Down(0)) :- Edge(_, _, x).
            // Add distances from the edges.
            Dist(x, y; Down(d)) :- Edge(x, d, y).
            // Add transitive distances.
            Dist(x, y; d1 + Down(d2)) :- Dist(x, z; d1) , Edge(z, d2, y).
        };
        let mapping = #{
            // Create the output mapping.
            Mapping((x, y), destructDown(d)) :- fix Dist(x, y; d).
        };
        let res = query edges, dists, mapping
                  select (p, d)
                  from Mapping(p, d);
        res |> List.toMap
    }

    ///
    /// Returns the shortest distance from `src` to every other vertex in the
    /// weighted directed graph `g`.
    ///
    pub def distancesFrom(src: t, g: m[(t, Int32, t)]): Map[t, Int32] with Foldable[m], Boxable[t] = {
        let edges = inject g into Edge;
        let dists = #{
            Dist(src; Down(0)).
            Dist(y; d + Down(w)) :- Dist(x; d), Edge(x, w, y).
        };
        let mapping = #{
            Mapping(x, destructDown(d)) :- fix Dist(x; d).
        };
        let res = query edges, dists, mapping
                  select (x, d)
                  from Mapping(x, d);
        res |> List.toMap
    }

    ///
    /// Returns the shortest distance from `src` to `dst` in the weighted
    /// directed graph `g`.
    ///
    pub def distance(src: { src = t }, dst: { dst = t }, g: m[(t, Int32, t)]): Option[Int32] with Foldable[m], Boxable[t] =
        distancesFrom(src.src, g) |> Map.get(dst.dst)

    ///
    /// Returns a copy of `g` where `(a, b)` edges is added for all edges
    /// `(b, a)` in `g`, i.e. An undirected graph in directed representation.
    ///
    pub def toUndirected(g: m[(t, t)]): Set[(t, t)] with Foldable[m], Boxable[t] = {
        g |>
            Foldable.toSet |>
            Set.flatMap(match (a, b) -> Set#{(a, b), (b, a)})
    }

    ///
    /// Returns a copy of `g` where `(a, w, b)` edges is added for all edges
    /// `(b, w, a)` in `g`, i.e. An undirected graph in directed representation.
    ///
    pub def toUndirectedWeighted(g: m[(t, Int32, t)]): Set[(t, Int32, t)] with Foldable[m], Boxable[t] = {
        g |>
            Foldable.toSet |>
            Set.flatMap(match (a, w, b) -> Set#{(a, w, b), (b, w, a)})
    }

    ///
    /// Returns the in-degree (how many edges end in a given node)
    /// of each node in `g`.
    ///
    pub def inDegrees(g: m[(t, t)]): Map[t, Int32] with Foldable[m], Boxable[t] = {
        use Option.{getWithDefault, map};
        let edges = inject g into Edge;
        let in = #{
            // Careful use of option type here to make sure the empty set is not
            // the bottom lattice element. that would exclude degrees of zero.
            InSet(n; Some(Set#{})) :- Node(n).
            InSet(n; Some(Set#{other})) :- Edge(other, n).
            InDegree(n, s |> map(Set.size) |> getWithDefault(0)) :-
                fix InSet(n; s).
        };
        let res = query edges, in, nodes()
                  select (n, d)
                  from InDegree(n, d);
        List.toMap(res)
    }

    ///
    /// Returns the out-degree (how many edges start in a given node)
    /// of each node in `g`.
    ///
    pub def outDegrees(g: m[(t, t)]): Map[t, Int32] with Foldable[m], Boxable[t] = {
        use Option.{getWithDefault, map};
        let edges = inject g into Edge;
        let out = #{
            // Careful use of option type here to make sure the empty set is not
            // the bottom lattice element. that would exclude degrees of zero.
            OutSet(n; Some(Set#{})) :- Node(n).
            OutSet(n; Some(Set#{other})) :- Edge(n, other).
            OutDegree(n, s |> map(Set.size) |> getWithDefault(0)) :-
                fix OutSet(n; s).
        };
        let res = query edges, out, nodes()
                  select (n, d)
                  from OutDegree(n, d);
        List.toMap(res)
    }

    /// Returns a Graphviz (DOT) string of the graph `g`.
    /// The strings of nodes are put in quotes with existing quotes escaped,
    /// otherwise DOT identifier validity is up to the caller.
    ///
    pub def toGraphviz(g: m[((t, t))]): String with Foldable[m], Boxable[t], ToString[t] = region r {
        let sb = new StringBuilder(r);
        def output(s) = StringBuilder.appendLine!(s, sb);
        output("digraph {");
        foreach((x, y) <- Foldable.iterator(r, g)) {
            output("  ${graphvizId(x)} -> ${graphvizId(y)}")
        };
        output("}");
        StringBuilder.toString(sb)
    }

    ///
    /// Returns a Graphviz (DOT) string of the graph `g`.
    /// The strings of nodes are put in quotes with existing quotes escaped,
    /// otherwise DOT identifier validity is up to the caller.
    ///
    pub def toGraphvizWeighted(g: m[((t, Int32, t))]): String with Foldable[m], Boxable[t], ToString[t] = region r {
        let sb = new StringBuilder(r);
        def output(s) = StringBuilder.appendLine!(s, sb);
        output("digraph {");
        foreach((x, w, y) <- Foldable.iterator(r, g)) {
            output("  ${graphvizId(x)} -> ${graphvizId(y)} [label = ${w}]")
        };
        output("}");
        StringBuilder.toString(sb)
    }

    // -------------------------------------------------------------------------
    // Private Functions -------------------------------------------------------
    // -------------------------------------------------------------------------

    ///
    /// Wraps `toString(id)` with double quotes and escapes any existing quotes
    /// with backslash.
    ///
    def graphvizId(id: t): String with ToString[t] = {
        "\"${String.replace(from = "\"", to = "\\\"", "${id}")}\""
    }

    ///
    /// Returns a Datalog program which computes the reachable nodes when given
    /// a set of `Edge` and `Node` facts.
    ///
    def reachability(): #{ Edge(t, t), Reachable(t, t) | r } with Boxable[t] = #{
        // All nodes can reach themselves.
        Reachable(n, n) :- Node(n).
        // If `n1` can reach `m` and there is an edge from `m` to `n2` then `n1`
        // can also reach `n2`. This adds all node pairs to the relational that
        // are reachable using any number of nodes.
        Reachable(n1, n2) :- Reachable(n1, m), Edge(m, n2).
    }

    ///
    /// Returns a Datalog program which when given a set of `Edge` facts
    /// computes the nodes.
    ///
    def nodes(): #{ Edge(t, t), Node(t) | r } with Boxable[t] = #{
        Node(x) :- Edge(x, _).
        Node(x) :- Edge(_, x).
    }

    ///
    /// Returns a Datalog program which computes the reachable nodes from `src`
    /// when given a set of `Edge` facts.
    ///
    def reachabilityFromSrc(src: t): #{ Edge(t, t), Reachable(t) | r } with Boxable[t] = #{
        // A node can reach itself.
        Reachable(src).
        // If `src` can reach `m` and there is an edge from `m` to `n` then
        // `src` can also reach `n`. This adds all node pairs to the relational
        // that are reachable using any number of nodes.
        Reachable(n) :- Reachable(m), Edge(m, n).
    }

    ///
    /// Returns the value inside the `d` value.
    ///
    pub def destructDown(d: Down[a]): a = {
        let Down(a) = d;
        a
    }

}

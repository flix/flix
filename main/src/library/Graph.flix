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

///
/// A library of functions that operates on graphs, represented as foldable
/// collections of either pairs or triples, for unweighted and weighted graphs
/// respectively. These pairs, or triples, represent directed edges while
/// nodes(vertices) are given implicitly.
///
mod Graph {

    ///
    /// Returns the pairs `(a, b)` where `a` can reach `b` through a number of
    /// edges in the directed graph `g`, including zero.
    ///
    pub def closure(g: m[(t, t)]): Set[(t, t)] \ Foldable.Aef[m] with Foldable[m], Order[t] = {
        let edges = inject g into Edge;
        let res = query edges, nodes(), reachability()
                  select (src, dst)
                  from Reachable(src, dst);
        Vector.toSet(res)
    }

    ///
    /// Returns the nodes that are reachable from `src` in the directed
    /// graph `g`.
    ///
    pub def reachableFrom(src: t, g: m[(t, t)]): Set[t] \ Foldable.Aef[m] with Foldable[m], Order[t] = {
        let edges = inject g into Edge;
        let res = query edges, reachabilityFromSrc(src)
                  select dst
                  from Reachable(dst);
        Vector.toSet(res)
    }

    ///
    /// Returns the nodes that are unreachable from `src` in the directed
    /// graph `g`.
    ///
    pub def unreachableFrom(src: t, g: m[(t, t)]): Set[t] \ Foldable.Aef[m] with Foldable[m], Order[t] = {
        let edges = inject g into Edge;
        let unreachablility = #{
            // If there is a node that is not reachable from `src` then it is
            // unreachable.
            UnReachable(x) :- Node(x), not Reachable(x).
        };
        let res = query edges, nodes(), reachabilityFromSrc(src), unreachablility
                  select dst
                  from UnReachable(dst);
        Vector.toSet(res)
    }

    ///
    /// Returns `true` if there is a path from `src` to `dst` in the directed
    /// graph `g`.
    ///
    pub def reachable(src: {src = t}, dst: {dst = t}, g: m[(t, t)]): Bool \ Foldable.Aef[m] with Foldable[m], Order[t] =
        reachableFrom(src#src, g) |> Set.exists(x -> dst#dst == x)

    ///
    /// Returns the strongly connected components of the directed graph `g`.
    /// Two nodes are in the same component if and only if they can both
    /// reach each other.
    ///
    pub def stronglyConnectedComponents(g: m[(t, t)]): Set[Set[t]] \ Foldable.Aef[m] with Foldable[m], Order[t] = {
        let edges = inject g into Edge;
        let connected = #{
            // If `n1` can reach `n2` and `n2` can reach `n1` then they are part
            // of the same strongly connected component.
            Connected(n1; Set#{n2}) :- Reachable(n1, n2), Reachable(n2, n1).
        };
        let components = #{
            // After the full computation of `Connected`, duplicates are removed
            // by checking that `n` is the minimum in the strongly connected
            // component.
            Components(s) :- fix Connected(n; s), if Some(n) == Set.minimum(s).
        };
        let res = query edges, nodes(), reachability(), connected, components
                  select x
                  from Components(x);
        Vector.toSet(res)
    }

    ///
    /// Returns the graph where all edges in the directed graph `g` have their
    /// nodes flipped.
    ///
    pub def flipEdges(g: m[(t, t)]): Set[(t, t)] \ Foldable.Aef[m] with Foldable[m], Order[t] = {
        let edges = inject g into Edge;
        let rev = #{
            RevEdge(y, x) :- Edge(x, y).
        };
        let res = query edges, rev
                  select (x, y)
                  from RevEdge(x, y);
        Vector.toSet(res)
    }

    ///
    /// Returns the inverse graph of the directed graph `g`. For all nodes in
    /// `g`. The new graph contains exactly those edges that are not in `g`.
    ///
    /// OBS: No self-edges are returned no matter the input.
    ///
    pub def invert(g: m[(t, t)]): Set[(t, t)] \ Foldable.Aef[m] with Foldable[m], Order[t] = {
        let edges = inject g into Edge;
        let inverse = #{
            InvEdge(x, y) :- Node(x), Node(y), not Edge(x, y), if x != y.
        };
        let res = query nodes(), edges, inverse
                  select (x, y)
                  from InvEdge(x, y);
        Vector.toSet(res)
    }

    ///
    /// Returns `true` if the directed graph `g` contains at least one cycle.
    ///
    pub def isCyclic(g: m[(t, t)]): Bool \ Foldable.Aef[m] with Foldable[m], Order[t] = {
        let edges = inject g into Edge;
        let reachability = #{
            // Reachability given the edges.
            Reachable(n1, n2) :- Edge(n1, n2).
            // If `n1` can reach `m` and there is an edge from `m` to `n2` then
            // `n1` can also reach `n2`.
            Reachable(n1, n2) :- Reachable(n1, m), Edge(m, n2).
        };
        let res = query edges, reachability
                  select ()
                  from Reachable(x, y)
                  where x == y;
        Vector.length(res) > 0
    }

    ///
    /// Returns the shortest distance between all pairs of nodes in the
    /// weighted directed graph `g`. Returns `None` if `g` contains a
    /// negative cycle.
    ///
    pub def distances(g: m[(t, Int32, t)]): Option[Map[(t, t), Int32]] \ Foldable.Aef[m] with Foldable[m], Order[t] = {
        // Algorithm based on floyd-warshall.
        /// Returns the number of items in `s` less than `v`.
        def rank(v, s) = s |> Set.partition(v0 -> v0 < v) |> fst |> Set.size;
        let edges = inject g into Edge;
        let negCycle = #{
            Node(x) :- Edge(x, _, _).
            Node(x) :- Edge(_, _, x).
            // Collect all nodes.
            Nodes(;Set#{x}) :- Node(x).
            // Compute the number of nodes.
            NodeCount(Set.size(ns)) :- fix Nodes(;ns).
            // Compute the rank (the index in a sorted order) each node has.
            NodeRank(x, rank(x, ns)) :- Node(x), fix Nodes(;ns).
            // `Dist(x, y, k; w)` says that `x` can reach `y` with a path of
            // length `w` using only intermediate nodes of rank less than
            // `k`. The `k` parameter is to control the number of iterations.
            // Direct paths use no intermediate vertices.
            Dist(x, x, 0; Down.Down(0)) :- Node(x).
            Dist(x, y, 0; Down.Down(w)) :- Edge(x, w, y).
            // If there exists a path of length `w` without using node `k+1`
            // then that path is also valid using node `k+1`.
            Dist(x, y, k+1; w) :- Dist(x, y, k; w), NodeCount(c), if k < c.
            // Combine paths if they use intermediate nodes of the correct
            // rank (also making sure `k` doesn't increase forever).
            Dist(x, y, k+1; w1 + w2) :- Dist(x, kth, k; w1), Dist(kth, y, k; w2),
                NodeRank(kth, k), NodeCount(c), if k < c.
            // If `x` can go to `x` with a negative distances, then it can be
            // repeated.
            NegCycle() :- fix Dist(x, x, _; w), if destructDown(w) < 0.
        };
        let mapping = #{
            Mapping((x, y), destructDown(d)) :- fix Dist(x, y, c; d), NodeCount(c).
        };
        let sol = solve edges, negCycle, mapping;
        let cycle = query sol
                    select ()
                    from NegCycle();
        if (Vector.isEmpty(cycle)) {
            // No negative cycle.
            let res = query sol
                      select (p, d)
                      from Mapping(p, d);
            Some(Vector.toMap(res))
        } else {
            // Negative cycle.
            None
        }
    }

    ///
    /// Returns the shortest distance between all pairs of nodes in the
    /// weighted directed graph `g`.
    ///
    /// OBS: No negative cycles must be present.
    ///
    pub def boundedDistances(g: m[(t, Int32, t)]): Map[(t, t), Int32] \ Foldable.Aef[m] with Foldable[m], Order[t] = {
        let edges = inject g into Edge;
        let dists = #{
            // Initialize all node distances to zero.
            Dist(x, x; Down.Down(0)) :- Edge(x, _, _).
            Dist(x, x; Down.Down(0)) :- Edge(_, _, x).
            // Add distances from the edges.
            Dist(x, y; Down.Down(d)) :- Edge(x, d, y).
            // Add transitive distances.
            Dist(x, y; d1 + Down.Down(d2)) :- Dist(x, z; d1) , Edge(z, d2, y).
        };
        let mapping = #{
            Mapping((x, y), destructDown(d)) :- fix Dist(x, y; d).
        };
        let res = query edges, dists, mapping
                  select (p, d)
                  from Mapping(p, d);
        res |> Vector.toMap
    }

    ///
    /// Returns the shortest distance from `src` to every other reachable vertex in the
    /// weighted directed graph `g`.
    ///
    pub def distancesFrom(src: t, g: m[(t, Int32, t)]): Map[t, Int32] \ Foldable.Aef[m] with Foldable[m], Order[t] = {
        let edges = inject g into Edge;
        let dists = #{
            Dist(src; Down.Down(0)).
            Dist(y; d + Down.Down(w)) :- Dist(x; d), Edge(x, w, y).
        };
        let res = query edges, dists select (x, coerce(d)) from Dist(x; d);
        res |> Vector.toMap
    }

    ///
    /// Returns the shortest distance from `src` to `dst` in the weighted
    /// directed graph `g`.
    ///
    pub def distance(src: { src = t }, dst: { dst = t }, g: m[(t, Int32, t)]): Option[Int32] \ Foldable.Aef[m] with Foldable[m], Order[t] =
        distancesFrom(src#src, g) |> Map.get(dst#dst)

    ///
    /// Returns a copy of the directed graph `g` where all flipped edges are
    /// added. An undirected graph in directed representation.
    ///
    pub def toUndirected(g: m[(t, t)]): Set[(t, t)] \ Foldable.Aef[m] with Foldable[m], Order[t] = {
        g |> Foldable.toSet
          |> Set.flatMap(match (a, b) -> Set#{(a, b), (b, a)})
    }

    ///
    /// Returns a copy of the weighted directed graph `g` where all flipped
    /// edges are added. An undirected graph in directed representation.
    ///
    pub def toUndirectedWeighted(g: m[(t, Int32, t)]): Set[(t, Int32, t)] \ Foldable.Aef[m] with Foldable[m], Order[t] = {
        g |> Foldable.toSet
          |> Set.flatMap(match (a, w, b) -> Set#{(a, w, b), (b, w, a)})
    }

    ///
    /// Returns the nodes that are at most `limit` (inclusive) distance away
    /// from `src` in the weighted directed graph `g`.
    ///
    /// OBS: No negative cycles must be present.
    ///
    pub def withinDistanceOf(src: t, limit: Int32, g: m[(t, Int32, t)]): Set[t] \ Foldable.Aef[m] with Foldable[m], Order[t] = {
        let edges = inject g into Edge;
        let nodes = #{
            Dist(src; Down.Down(0)) :- if limit >= 0.
            Dist(y; d + Down.Down(w)) :- Dist(x; d), Edge(x, w, y),
                                    if destructDown(d) + w <= limit.
            Node(x) :- fix Dist(x; _).
        };
        let res = query edges, nodes
                  select x
                  from Node(x);
        Vector.toSet(res)
    }

    ///
    /// Returns the nodes that are at most `limit` (inclusive) edges away
    /// from `src` in the directed graph `g`.
    ///
    pub def withinEdgesOf(src: t, limit: Int32, g: m[(t, t)]): Set[t] \ Foldable.Aef[m] with Foldable[m], Order[t] = {
        let edges = inject g into Edge;
        let nodes = #{
            Dist(src; Down.Down(0)) :- if limit >= 0.
            Dist(y; d + Down.Down(1)) :- Dist(x; d), Edge(x, y),
                                    if destructDown(d) + 1 <= limit.
            Node(x) :- fix Dist(x; _).
        };
        let res = query edges, nodes
                  select x
                  from Node(x);
        Vector.toSet(res)
    }

    ///
    /// Returns the topologically sorted nodes (all edges go from lower indices
    /// to higher indices of the list) in the directed graph `g`.
    /// Unordered nodes are consistently (although not intuitively) ordered.
    ///
    /// OBS: No cycles must be present.
    ///
    pub def topologicalSort(g: m[(t, t)]): List[t] \ Foldable.Aef[m] with Foldable[m], Order[t] = region rc {
        // https://github.com/souffle-lang/benchmarks/tree/master/benchmarks/topological_ordering
        let edges = inject g into Edge;
        let topSort = #{
            EdgeSecond(x) :- Edge(_, x).
            EdgeFirst(x) :- Edge(x, _).

            Before(x, y) :- Edge(x, y).
            Before(x, y) :- Before(x, z), Edge(z, y).

            After(x, y) :- Edge(y, x).
            After(x, y) :- After(z, y), Edge(z, x).

            // Find a consistent index for each node, not necessarily unique.
            Index(x; 0) :- Node(x), not EdgeSecond(x), EdgeFirst(x).
            Index(x; i+1) :- Before(y, x), not Before(x, y),
                               After(x, y), Index(y; i).
        };
        let res = query edges, nodes(), topSort select (x, i) from Index(x; i);
        let ml = MutList.empty(rc);
        // Extract nodes in fixed order.
        res |>
            Vector.sortBy(match (x, i) -> (i, x)) |>
            Vector.forEach(match (x, _) -> MutList.push!(x, ml));
        MutList.toList(ml)
    }

    ///
    /// Returns the degree of each node in the directed graph `g` (the number of
    /// times a node exists as an endpoint of an edge).
    ///
    pub def degrees(g: m[(t, t)]): Map[t, Int32] \ Foldable.Aef[m] with Foldable[m], Order[t] = {
        use Option.{getWithDefault, map};
        let edges = inject g into Edge;
        let in = #{
            // Careful use of option type here to make sure the empty set is not
            // the bottom lattice element. That would exclude degrees of zero.
            TouchSet(n; Some(Set#{})) :- Node(n).
            TouchSet(n; Some(Set#{(other, n)})) :- Edge(other, n).
            TouchSet(n; Some(Set#{(n, other)})) :- Edge(n, other).
            Degree(n, s |> map(Set.size) |> getWithDefault(0)) :-
                fix TouchSet(n; s).
        };
        let res = query edges, in, nodes()
                  select (n, d)
                  from Degree(n, d);
        Vector.toMap(res)
    }

    ///
    /// Returns a mapping from distances to the set of nodes for which the
    /// shortest path from `src` in the directed graph `g` is of a given length.
    ///
    pub def frontiersFrom(src: t, g: m[(t, t)]): Map[Int32, Set[t]] \ Foldable.Aef[m] with Foldable[m], Order[t] = {
        let edges = inject g into Edge;
        let frontiers = #{
            Dist(src; Down.Down(0)).
            Dist(y; n + Down.Down(1)) :- Dist(x; n), Edge(x, y).

            // Find the max frontier.
            Frontiers(;Set#{destructDown(n)}) :- fix Dist(_; n).
            MaxFrontier(Set.maximum(s) |> Option.getWithDefault(0)) :- fix Frontiers(;s).

            // Initialize all frontiers (with non-bot element).
            Frontier(0; Some(Set#{})) :- MaxFrontier(m), if m > 0.
            Frontier(n+1; Some(Set#{})) :- Frontier(n; _), MaxFrontier(m), if n < m.

            // Collect the frontiers.
            Frontier(destructDown(n); Some(Set#{x})) :- fix Dist(x; n).
        };
        let res = query edges, frontiers
                  select (n, s)
                  from Frontier(n; s);
        let unwrapOption = match (n, s) -> match s {
            case Some(v) => Some((n, v))
            case None => None
        };
        res |>
            Vector.filterMap(unwrapOption) |>
            Vector.toMap
    }

    ///
    /// Returns triples `(x, cut, y)` such that `x` cannot reach `y` without
    /// using `cut` (where `x`, `cut` and `y` are all distinct) in the directed
    /// graph `g`.
    ///
    /// There will at most be one triple for each pair of nodes (which will
    /// be the maximum `cut` of the possible choices).
    ///
    pub def cutPoints(g: m[(t, t)]): Set[(t, t, t)] \ Foldable.Aef[m] with Foldable[m], Order[t] = {
        let edges = inject g into Edge;
        def unpack(opt) = match opt {
            case Some(v) => v
            case None => unreachable!()
        };
        let cuts = #{
            CutPoint(x, Set.maximum(cut) |> unpack, y) :-
                fix CutPointSet(x, y; cut), if not Set.isEmpty(cut).
            CutPointSet(x, y; Set#{cut}) :- Reachable(x, y), Node(cut),
                                            not Circumvent(x, cut, y),
                                            if x != cut, if y != cut, if x != y.
            Circumvent(x, cut, y) :- Node(cut), Edge(x, y),
                                     if x != cut, if y != cut.
            Circumvent(x, cut, y) :- Circumvent(x, cut, z), Edge(z, y),
                                     Node(cut), if y != cut.
        };
        let res = query edges, nodes(), reachability(), cuts
                  select (x, cut, y)
                  from CutPoint(x, cut, y);
        Vector.toSet(res)
    }

    ///
    /// Returns the in-degree (how many edges end in a given node)
    /// of each node in the directed graph `g`.
    ///
    pub def inDegrees(g: m[(t, t)]): Map[t, Int32] \ Foldable.Aef[m] with Foldable[m], Order[t] = {
        use Option.{getWithDefault, map};
        let edges = inject g into Edge;
        let in = #{
            // Careful use of option type here to make sure the empty set is not
            // the bottom lattice element. That would exclude degrees of zero.
            InSet(n; Some(Set#{})) :- Node(n).
            InSet(n; Some(Set#{other})) :- Edge(other, n).
            InDegree(n, s |> map(Set.size) |> getWithDefault(0)) :-
                fix InSet(n; s).
        };
        let res = query edges, in, nodes()
                  select (n, d)
                  from InDegree(n, d);
        Vector.toMap(res)
    }

    ///
    /// Returns the out-degree (how many edges start in a given node)
    /// of each node in the directed graph `g`.
    ///
    pub def outDegrees(g: m[(t, t)]): Map[t, Int32] \ Foldable.Aef[m] with Foldable[m], Order[t] = {
        use Option.{getWithDefault, map};
        let edges = inject g into Edge;
        let out = #{
            // Careful use of option type here to make sure the empty set is not
            // the bottom lattice element. That would exclude degrees of zero.
            OutSet(n; Some(Set#{})) :- Node(n).
            OutSet(n; Some(Set#{other})) :- Edge(n, other).
            OutDegree(n, s |> map(Set.size) |> getWithDefault(0)) :-
                fix OutSet(n; s).
        };
        let res = query edges, out, nodes()
                  select (n, d)
                  from OutDegree(n, d);
        Vector.toMap(res)
    }

    ///
    /// Returns a Graphviz (DOT) string of the directed graph `g`.
    /// The strings of nodes are put in quotes but DOT identifier validity is
    /// up to the caller.
    ///
    pub def toGraphviz(g: m[(t, t)]): String \ Foldable.Aef[m] with Foldable[m], Order[t], ToString[t] = region rc {
        let sb = StringBuilder.empty(rc);
        def output(s) = StringBuilder.appendLine!(s, sb);
        output("digraph {");
        Foldable.forEach(match (x, y) -> {
            output("  ${graphvizId(x)} -> ${graphvizId(y)}")
        }, g);
        output("}");
        StringBuilder.toString(sb)
    }

    ///
    /// Returns a Graphviz (DOT) string of the directed graph `g`.
    /// The strings of nodes are put in quotes and existing quotes are escaped.
    /// Other than that, DOT identifier validity is up to the caller.
    ///
    pub def toGraphvizLabeled(g: m[(t, l, t)]): String \ Foldable.Aef[m] with Foldable[m], ToString[t], ToString[l] = region rc {
        let sb = StringBuilder.empty(rc);
        def output(s) = StringBuilder.appendLine!(s, sb);
        output("digraph {");
        Foldable.forEach(match (x, w, y) -> {
            output("  ${graphvizId(x)} -> ${graphvizId(y)} [label = ${w}]")
        }, g);
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
        "\"${String.replace(src = "\"", dst = "\\\"", "${id}")}\""
    }

    ///
    /// Returns a Datalog program which computes the reachable nodes when
    /// given a set of `Edge` and `Node` facts. Nodes can always reach
    /// themselves.
    ///
    def reachability(): #{ Edge(t, t), Reachable(t, t), Node(t) | r } with Order[t] = #{
        // All nodes can reach themselves.
        Reachable(n, n) :- Node(n).
        // If `n1` can reach `m` and there is an edge from `m` to `n2` then `n1`
        // can also reach `n2`. This adds all node pairs to the relational that
        // are reachable using any number of nodes.
        Reachable(n1, n2) :- Reachable(n1, m), Edge(m, n2).
    }

    ///
    /// Returns a Datalog program that, when given a set of `Edge` facts,
    /// computes the nodes.
    ///
    def nodes(): #{ Edge(t, t), Node(t) | r } with Order[t] = #{
        Node(x) :- Edge(x, _).
        Node(x) :- Edge(_, x).
    }

    ///
    /// Returns a Datalog program which computes the reachable nodes from `src`
    /// when given a set of `Edge` facts.
    ///
    def reachabilityFromSrc(src: t): #{ Edge(t, t), Reachable(t) | r } with Order[t] = #{
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
    def destructDown(d: Down[a]): a = {
        let Down.Down(a) = d;
        a
    }

}

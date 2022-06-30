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
    /// Returns the transitive closure of the graph `g`.
    ///
    pub def closure(g: m[(t, t)]): Set[(t, t)] with Foldable[m], Boxable[t] = {
        let edges = inject g into Edge;
        let reachability = #{
            // All nodes can reach themselves.
            Reachable(n1, n1) :- Edge(n1, _).
            Reachable(n2, n2) :- Edge(_, n2).
            // `n1` can reach `n2` by using an edge.
            Reachable(n1, n2) :- Edge(n1, n2).
            // if `n1` can reach `m` and `m` can reach `n2` then `n1` can
            // also reach `n2`. This adds node pairs to the relational
            // that is reachable using any number of nodes.
            Reachable(n1, n2) :- Reachable(n1, m), Reachable(m, n2).
        };
        let x = query edges, reachability select (src, dst) from Reachable(src, dst);
        Array.toSet(x)
    } as & Pure                         // TODO : delete cast when query uses regions

    ///
    /// Returns the vertices that are reachable from the origin `o` in the graph `g`.
    ///
    pub def reachable(o: t, g: m[(t, t)]): Set[t] with Foldable[m], Boxable[t] = {
        let edges = inject g into Edge;
        let reachability = #{
            // A node can reach itself.
            Reachable(o).
            // Directly reachable.
            Reachable(n) :- Edge(o, n).
            // if `o` can reach `m` and `m` can reach `n` then `o` can
            // also reach `n`. This adds node pairs to the relational
            // that is reachable using any number of nodes.
            Reachable(n) :- Reachable(m), Edge(m, n).
        };
        let x = query edges, reachability select dst from Reachable(dst);
        Array.toSet(x)
    } as & Pure

    ///
    /// Returns the vertices that are unreachable from the origin `o` in the graph `g`.
    ///
    pub def unreachable(o: String, g: m[(t, t)]): Set[t] = ???

    ///
    /// Returns `true` if there is a path from `src` to `dst` in the graph `g`.
    ///
    pub def isConnected(src: String, dst: String, g: m[(t, t)]): Set[t] = ???

    ///
    /// Returns the strongly connected components of the graph `g`.
    ///
    pub def stronglyConnectedComponents(g: m[(t, t)]): List[Set[t]] = ???

    ///
    /// Returns the topological sort of the graph `g`.
    ///
    pub def topologicalSort(g: m[(t, t)]): Option[List[t]] = ???

    ///
    /// ...
    ///
    pub def isCyclic(g: m[(t, t)]): Bool = ???

    ///
    /// Returns the shortest distance from `src` to `dst` in the graph `g`.
    ///
    pub def distance(src: String, dst: String, g: m[(t, t)]): d /*with LatticeStuff[d]*/ = ???

    ///
    /// Returns the shortest distance from `o` to every other vertex in the graph `g`.
    ///
    pub def distances(o: String, g: m[(t, d, t)]): Map[String, d] /*with LatticeStuff[d]*/ = ???

    ///
    /// Returns the given graph `g` as a GraphViz dot string.
    ///
    pub def toGraphViz(g: m[(t, d, t)]): String = ???

    ///
    /// Returns the undirected graph of the given directed graph.
    ///
    pub def toUndirected(g: m[(t, t)]): m[(t, t)] = ???


    /// Minimum spanning tree?


}

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
    pub def closure(g: m[(t, t)]): Set[(t, t)] with Foldable[m], Boxable[t] =
        let edges = inject g into Edge;
        let reachability = #{
            // All nodes can reach themselves.
            Reachable(n1, n1) :- Edge(n1, _).
            Reachable(n2, n2) :- Edge(_, n2).
            // If `n1` can reach `m` and there is an edge from `m` to `n2`
            // then `n1` can also reach `n2`. This adds all node pairs to
            // the relational that are reachable using any number of nodes.
            Reachable(n1, n2) :- Reachable(n1, m), Edge(m, n2).
        };
        let x = query edges, reachability select (src, dst) from Reachable(src, dst);
        List.toSet(x)

    ///
    /// Returns the vertices that are reachable from the `origin` in the directed graph `g`.
    ///
    pub def reachable(origin: t, g: m[(t, t)]): Set[t] with Foldable[m], Boxable[t] =
        let edges = inject g into Edge;
        let reachability = #{
            // A node can reach itself.
            Reachable(origin).
            // If `origin` can reach `m` and there is an edge from `m` to `n`
            // then `origin` can also reach `n`. This adds all node pairs to
            // the relational that are reachable using any number of nodes.
            Reachable(n) :- Reachable(m), Edge(m, n).
        };
        let x = query edges, reachability select dst from Reachable(dst);
        List.toSet(x)

    ///
    /// Returns the vertices that are unreachable from the `src` in the directed graph `g`.
    ///
    pub def unreachable(src: t, g: m[(t, t)]): Set[t] with Foldable[m], Boxable[t] =
        let edges = inject g into Edge;
        let nodes = #{
            Node(x) :- Edge(x, _).
            Node(x) :- Edge(_, x).
        };
        let reachability = #{
            // A node can reach itself.
            Reachable(src).
            // If `src` can reach `m` and there is an edge from `m` to `n`
            // then `src` can also reach `n`. This adds all node pairs to
            // the relational that are reachable using any number of nodes.
            Reachable(n) :- Reachable(m), Edge(m, n).
        };
        let unreachablility = #{
            // If there is a node that is not Reachable from src then it is unreachable.
            UnReachable(x) :- Node(x), not Reachable(x).
        };
        let x = query edges, nodes, reachability, unreachablility select dst from UnReachable(dst);
        List.toSet(x)

    ///
    /// Returns `true` if there is a path from `src` to `dst` in the directed graph `g`.
    ///
    pub def isConnected(src: {src :: t}, dst: {dst :: t}, g: m[(t, t)]): Bool with Foldable[m], Boxable[t] =
        reachable(src.src, g) |> Set.exists(x -> dst.dst == x)


    ///
    /// Returns the strongly connected components of the directed graph `g`.
    ///
    pub def stronglyConnectedComponents(g: m[(t, t)]): List[Set[t]] with Foldable[m], Boxable[t] =
        let edges = inject g into Edge;
        let nodes = #{
            Node(x) :- Edge(x, _).
            Node(x) :- Edge(_, x).
        };
        let reachability = #{
            // All nodes can reach themselves.
            Reachable(n1, n1) :- Edge(n1, _).
            Reachable(n2, n2) :- Edge(_, n2).
            // If `n1` can reach `m` and there is an edge from `m` to `n2`
            // then `n1` can also reach `n2`. This adds all node pairs to
            // the relational that are reachable using any number of nodes.
            Reachable(n1, n2) :- Reachable(n1, m), Edge(m, n2).
        };
        let connected = #{
            Connected(n1, n2) :- Reachable(n1, n2), Reachable(n2, n1).
        };

        let allNodes = query edges, nodes select x from Node(x);

        let f = (x) -> {
            query edges, reachability, connected select dst from Connected(src, dst) where src == x
        };

        fst(List.foldLeft((acc, x) -> {
            let (acc1, visited) = acc;
            if (Set.memberOf(x, visited)) {
                acc
            } else {
                let stronglyConnected = Set.insert(x, List.toSet(f(x)));
                (stronglyConnected :: acc1, Set.union(visited, stronglyConnected))
            }
        }, (Nil, Set.empty()), allNodes))

}

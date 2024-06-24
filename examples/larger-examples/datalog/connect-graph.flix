/*
 * Copyright 2022 Jonathan Lindegaard Starup
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

pub type alias Edge[node] = (node, node)

pub type alias Graph[node] = {
    nodes = Set[node],
    edges = Set[Edge[node]]
}

/// Given the undirected graph described by `nodes` and `edges`, connected components are
/// found and a relation with the component representative for each node is returned.
pub def connectedComponentRep(g: Graph[node]): #{ ComponentRep(node, node) | r } with Order[node] =
    let nodes = inject g#nodes into Node;
    let edges = inject g#edges into Edge;
    let reachability = #{
        // `Reachable(n1, n2)` describes that `n1` can reach `n2`.
        // All nodes can reach themselves.
        Reachable(n, n) :- Node(n).
        // `n1` can reach `n2` by using an edge.
        Reachable(n1, n2) :- Edge(n1, n2).
        // Edges can be used both ways since the graph is undirected.
        Reachable(n1, n2) :- Edge(n2, n1).
        // if `n1` can reach `m` and `m` can reach `n2` then `n1` can
        // also reach `n2`. This adds node pairs to the relational
        // that is reachable using any number of nodes.
        Reachable(n1, n2) :- Reachable(n1, m), Reachable(m, n2).
        // `ReachUp` contains nodes that can reach at least one other
        // node with a higher value. This contains all nodes
        // that are not the maximum node of their component.
        ReachUp(n1) :- Reachable(n1, n2), if n1 < n2.
        // `n` is in a component that is represented by `rep`.
        // `rep` is the highest node of the component.
        ComponentRep(n, rep) :- Reachable(n, rep), not ReachUp(rep).
    };
    solve nodes, edges, reachability project ComponentRep

/// Connects the undirected graph given by `nodes` and `edges`
/// ny connecting the connected components of the graph.
pub def connectGraph(g: Graph[node]): #{ Edge(Set[node], Set[node]) | r } with Order[node] =
    let missingEdges = #{
        // `Component(rep; c)` describes that the node `rep` is the representative of the component
        // `c` which is a set of nodes. The semicolon makes `c` use lattice semantics which
        // cause multiple components for a specific `rep` to be reduced to one value by union.
        Component(rep; Set#{n}) :- ComponentRep(n, rep).
        // every pair of components `(c1, c2)` where `c1` is lexicographically
        // smaller than `c2`. The fix keyword is used here to ensure that the
        // full union is computed before producing Edge values.
        Edge(c1, c2) :- fix Component(_; c1), fix Component(_; c2), if c1 < c2.
    };
    solve connectedComponentRep(g), missingEdges project Edge

def main(): Unit \ IO =
    let graph = {
        nodes = Set.range(0, 8),
        edges = Set#{(0, 4), (0, 7), (2, 3), (1, 6), (5, 6)}
    };
    let connectedGraph = connectGraph(graph);
    let result = query connectedGraph select (c1, c2) from Edge(c1, c2);
    let graphString =
        "    ┌─┐           ┌─┐" ::
        "    │5│       ┌─┐ │2│" ::
        "    └─┴─┐     │3├─┴─┘" ::
        "  ┌─┐   ├─┐   └─┘    " ::
        "  │1│   │6│          " ::
        "  └─┴───┴─┘          " ::
        "  ┌─┐                " ::
        "  │4│                " ::
        "  └─┴──┬─┐           " ::
        "       │0│    ┌─┐    " ::
        "       └─┴────┤7│    " ::
        "              └─┘    " ::
        Nil;

    println(graphString |> String.intercalate(String.lineSeparator()));
    println("The graph above has the components below with edges between them");

    def pad(s) = ToString.toString(s) |> String.padLeft(15, ' ');
    result |>
        Vector.map(match (c1, c2) -> "${pad(c1)} <--> ${c2}") |>
        Vector.forEach(println)

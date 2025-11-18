/*
 * Copyright 2025 Casper Dalgaard Nielsen
 *                Adam Yasser Tallouzi
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

///
/// `PrecedenceGraph` contains a graph module which is used to compute a stratification
/// of a Datalog program. The stratification is based on a topological sort
/// of the strongly connected components (SCC) of the precedence graph.
///
/// The topological sort describes is a legal stratification of the Datalog program.
///
/// A `PseudoStratum` is a group of mutually independent, consecutive strata, each
/// represented by a unique SCC in the topological order.
///
mod Fixpoint3.PrecedenceGraph {
    @Internal
    pub type alias Vertex = Int32

    ///
    /// A `PseudoStratum` is an unordered list of strata without dependencies between them.
    ///
    @Internal
    pub type alias PseudoStratum = List[Set[Vertex]]

    ///
    /// Returns a list of the pseudostrata of `g`.
    ///
    /// For example, the graph given by the edges
    /// ```
    /// 1 -> 2
    /// 2 -> 3
    /// 3 -> 2
    /// 4 -> 3
    /// ```
    /// would produce three SCCs: `{2, 3}`, `{1}`, and `{4}`. A topological sort of the
    /// condensation graph gives the ordering `[{1}, {4}, {2, 3}]`. Since the components
    /// `{1}` and `{4}` have no dependencies between them and are adjacent in the ordering,
    /// they are merged into the same pseudostratum `[{1}, {4}]`. Thus, the list of
    /// pseudostrata is `[[{1}, {4}], {2, 3}]`.
    ///
    @Internal
    pub def getPseudoStrata(g: MutGraph[r]): List[PseudoStratum] \ r = region rc {
        let sorted = MutGraph.getSCCOrder(g);
        let pseudoStratums = MutList.empty(rc);
        def loop(s1, list) = match list {
            case s2 :: rest =>
                let mergeable = s1 |>
                    List.forAll(
                        Set.forAll(u -> s2 |> Set.forAll(v -> not MutGraph.hasEdge(u, v, g)))
                    );
                let next =
                    if (mergeable) s2 :: s1
                    else {
                        MutList.push(s1, pseudoStratums);
                        s2 :: Nil
                    };
                loop(next, rest)
            case Nil => MutList.push(s1, pseudoStratums)
        };
        match sorted {
            case s :: rest => loop(s :: Nil, rest)
            case Nil       => ()
        };
        MutList.toList(pseudoStratums)
    }

    ///
    /// A mutable graph.
    ///
    @Internal
    pub struct MutGraph[r] {
        vertices: MutSet[Vertex, r],
        adjList: MutMap[Vertex, Set[Vertex], r]
    }

    mod MutGraph {
        use Fixpoint3.Counter
        use Fixpoint3.PrecedenceGraph.Vertex
        use Fixpoint3.Util.getOrCrash

        ///
        /// `Color` is used to label vertices during DFS traversal and topological sort.
        ///
        /// A vertex is colored `White` if unvisited, `Gray` if currently being visisted,
        /// and `Black` if it and all of its neighbors have been visited.
        ///
        enum Color with Eq {
            case White,
            case Gray,
            case Black
        }

        ///
        /// Returns an empty graph with no vertices or edges.
        ///
        @Internal
        pub def empty(rc: Region[r]): MutGraph[r] \ r =
            new MutGraph @ rc {
                vertices = MutSet.empty(rc),
                adjList = MutMap.empty(rc)
            }

        ///
        /// Returns `true` if `g` contains the edge `(u, v)`.
        ///
        @Internal
        pub def hasEdge(u: Vertex, v: Vertex, g: MutGraph[r]): Bool \ r =
            g->adjList |> MutMap.getWithDefault(u, Set.empty()) |> Set.memberOf(v)

        ///
        /// Returns the vertices of `g`.
        ///
        def getVertices(g: MutGraph[r]): MutSet[Vertex, r] = g->vertices

        ///
        /// Returns a `List` of the neighbors of `u` in `g`.
        ///
        def getNeighbors(u: Vertex, g: MutGraph[r]): Set[Vertex] \ r =
            MutMap.getWithDefault(u, Set.empty(), g->adjList)

        ///
        /// Returns the number of vertices in `g`. `MutGraph`s should start with vertex 0.
        ///
        @Internal
        pub def numberOfVertices(g: MutGraph[r]): Int32 \ r = match MutSet.maximum(g->vertices) {
            case Some(v) => v + 1
            case None    => 0
        }

        ///
        /// Adds the edge `(u, v)` to `g`.
        ///
        /// The vertices `u` and `v` are automatically added to `g`.
        ///
        @Internal
        pub def addEdge(u: Vertex, v: Vertex, g: MutGraph[r]): Unit \ r = {
            MutSet.add(v, g->vertices);
            MutSet.add(u, g->vertices);
            let neighbors = MutMap.getWithDefault(u, Set.empty(), g->adjList);
            if (Set.memberOf(v, neighbors))
                ()
            else
                MutMap.put(u, Set.insert(v, neighbors), g->adjList)
        }

        ///
        /// Adds the edge `(u, v)` to `g`.
        ///
        @Internal
        pub def addVertex(v: Vertex, g: MutGraph[r]): Unit \ r = MutSet.add(v, g->vertices)

        ///
        /// Returns a string representation of `g`.
        ///
        @Internal
        pub def toString(g: MutGraph[r]): String \ r =
            let edges = g->adjList |>
                MutMap.joinWith(u -> vSet ->
                    Set.joinWith(v -> "\n ${u} -> ${v}", ",", vSet), ","
                );
            "MutGraph(${edges})"

        ///
        /// Returns the strongly connected components of `g` in a topologically sorted
        /// order where the SCC's are represented by the set of vertices in them.
        ///
        @Internal
        pub def getSCCOrder(g: MutGraph[r]): List[Set[Vertex]] \ r = region rc {
            let components = stronglyConnectedComponents(g);
            let condensation = empty(rc);
            let componentMap: MutMap[Vertex, Set[Vertex], rc] = MutMap.empty(rc);
            let rootMap: MutMap[Vertex, Vertex, rc] = MutMap.empty(rc);
            foreach ((i, scc) <- ForEach.withIndex(components)) {
                foreach (partOfComp <- scc) {
                    MutMap.put(partOfComp, i, rootMap);
                    MutMap.put(partOfComp, scc, componentMap)
                }
            };
            foreach ((u, neighbors) <- g->adjList) {
                foreach (v <- neighbors) {
                    let uSCC = getOrCrash(MutMap.get(u, rootMap));
                    let vSCC = getOrCrash(MutMap.get(v, rootMap));
                    if (uSCC != vSCC) {
                        addEdge(uSCC, vSCC, condensation)
                    }
                }
            };
            let inverseRootMap = MutMap.toMap(rootMap)
                |> Map.invert
                // We are guaranteed to have singleton sets
                |> Map.map(Set.find(_ -> true) >> getOrCrash);
            topologicalSort(condensation)
                |> List.map(u ->
                    componentMap
                        |> MutMap.get(getOrCrash(Map.get(u, inverseRootMap)))
                        |> getOrCrash
            )
        }

        ///
        /// Return a `List` of the vertices of `g` in a topologically sorted order. A
        /// topological sort is a linear ordering of a directed acyclic graph such that
        /// for every edge `(u, v)`, vertex `u` comes before vertex `v` in the ordering.
        ///
        def topologicalSort(g: MutGraph[r]): List[Vertex] \ r = region rc {
            let sorted = MutList.empty(rc);
            let colors = Array.repeat(rc, numberOfVertices(g), Color.White);
            foreach (u <- getVertices(g)) {
                if (Array.get(u, colors) == Color.White) {
                    topologicalSortVisit(g, colors, sorted, u)
                }
            };
            sorted |> MutList.toList |> List.reverse
        }

        ///
        /// Pushes undiscovered vertices reachable from `u` of graph `g` in reverse
        /// topological to `sorted`.
        ///
        def topologicalSortVisit(
            g: MutGraph[r1],
            colors: Array[Color, r0],
            sorted: MutList[Vertex, r0],
            u: Vertex
        ): Unit \ r0 + r1 = {
            Array.put(Color.Gray, u, colors);
            foreach (v <- getNeighbors(u, g)) {
                if (Array.get(v, colors) == Color.White) {
                    topologicalSortVisit(g, colors, sorted, v)
                } else if (Array.get(v, colors) == Color.Gray) {
                    bug!("In Fixpoint.PrecedenceGraph.MutGraph.topologicalSortVisit: Cycle detected")
                }
            };
            Array.put(Color.Black, u, colors);
            MutList.push(u, sorted)
        }

        ///
        /// Returns a `List` of the strongly conected components of `g`.
        ///
        def stronglyConnectedComponents(g: MutGraph[r]): List[Set[Vertex]] \ r = region rc {
            let n = numberOfVertices(g);
            let disc = Array.repeat(rc, n, -1);
            let low = Array.repeat(rc, n, -1);
            let onStack = Array.empty(rc, n);
            let stack = MutList.empty(rc);
            let components = MutList.empty(rc);

            let time = Counter.fresh(rc);
            foreach (u <- getVertices(g)) {
                if (Array.get(u, disc) == -1) {
                    stronglyConnectedComponentsVisit(
                        components, stack, disc, low, onStack, time, g, u
                    )
                }
            };
            MutList.toList(components)
        }

        ///
        /// Helper method for `stronglyConnectedComponents`.
        ///
        /// `g` is the graph. `u` is the current vertex under consideration. `time` is
        /// used to assign discovery times. `onStack[u]` is true iff `u` has been met but
        /// has not yet been assigned to a component. Similarly, `stack` contains the
        /// vertices that have been met, but not assigned a component in order of discovery.
        /// `disc[u]` is the time `u` was discovered, initialized to `-1`. `low[u]` is
        /// assigned the lowest discovery time reachable from `u`, for vertices that were
        /// undiscovered when met as a neighbor of `u`, or still on the stack when
        /// considered as a neighbor of `u`.
        ///
        /// The following invariant is kept: When all calls to neighbors of `u` are
        /// finished everything discovered after `u` and still on `stack` is in the same
        /// SCC as `u`. Furthermore `low[u]` will be `disc[u]` iff `u` was the first vertex
        /// discovered in `u`'s SCC.
        ///
        /// `stronglyConnectedComponentsVisit(..., u)` will first handle all SCC reachable
        /// from `u`. Afterwards all nodes on `stack` most be in the same SCC as `u`. If
        /// `disc[u]==low[u]` then `u` was the first vertex discovered in the
        /// SCC and the SCC is built by popping from the stack until `u` is met.
        ///
        def stronglyConnectedComponentsVisit(
            components: MutList[Set[Vertex], r0],
            stack: MutList[Vertex, r0],
            disc: Array[Int32, r0],
            low: Array[Int32, r0],
            onStack: Array[Bool, r0],
            time: Counter[r0],
            g: MutGraph[r1],
            u: Vertex
        ): Unit \ r0 + r1 = {
            Array.put(Counter.get(time), u, disc);
            Array.put(Counter.get(time), u, low);
            Counter.increment(time);
            Array.put(true, u, onStack);
            MutList.push(u, stack);

            foreach (v <- getNeighbors(u, g)) {
                if (Array.get(v, disc) == -1) {
                    stronglyConnectedComponentsVisit(
                        components, stack, disc, low, onStack, time, g, v
                    );
                    let uLow = Array.get(u, low);
                    let vLow = Array.get(v, low);
                    Array.put(Int32.min(uLow, vLow), u, low)
                } else if (Array.get(v, onStack)) {
                    let uLow = Array.get(u, low);
                    let vLow = Array.get(v, low);
                    Array.put(Int32.min(uLow, vLow), u, low)
                }
            };
            if (Array.get(u, low) == Array.get(u, disc)) {
                region rc {
                    let mutSet = MutSet.empty(rc);
                    def loop() = {
                        let w = match MutList.pop(stack) {
                            case Some(v) => v
                            case None    => bug!("In Fixpoint.PrecedenceGraph.MutGraph: stack cannot be empty")
                        };
                        Array.put(false, w, onStack);
                        MutSet.add(w, mutSet);
                        if (u != w) { loop() }
                    };
                    loop();
                    let component = MutSet.toSet(mutSet);
                    MutList.push(component, components)
                }
            }
        }
    }

}

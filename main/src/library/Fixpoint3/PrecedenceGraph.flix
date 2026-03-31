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
pub mod Fixpoint3.PrecedenceGraph {
    pub type alias Vertex = Int32

    ///
    /// A `PseudoStratum` is an unordered list of strata without dependencies between them.
    ///
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
    pub struct MutGraph[r] {
        vertices: MutSet[Vertex, r],
        adjList: MutMap[Vertex, Set[Vertex], r]
    }

}

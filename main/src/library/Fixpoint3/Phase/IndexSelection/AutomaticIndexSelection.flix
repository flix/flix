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
/// This is based on the paper "Automatic Index Selection for Large-Scale Datalog
/// Computation" by Pavle Subotić, Herbert Jordan, Lijun Chang, Alan Fekete,
/// Bernhard Scholz (https://www.vldb.org/pvldb/vol12/p141-subotic.pdf)
///
pub mod Fixpoint3.Phase.IndexSelection.AutomaticIndexSelection {
    use Fixpoint3.Ast.Ram.Search

    type alias Vertex = Int32
    type alias Edge = (Vertex, Vertex)
    // Matching is a set but we use a `Map` here so we can call `rangeQueryWith`
    type alias Matching = Map[Edge, Unit]
    type alias AdjacencyList = MultiMap[Vertex, Vertex]

    ///
    /// Find the minimum set of indexes that covers `searches`.
    ///
    /// Using the example from earlier `S={{0, 1}, {0, 1, 2}, {0, 2}}` can be covered
    /// by `{0 < 1 < 2, 0 < 2}`. `{0, 1, 2}` and `{0, 2}` are trivially covered as they
    /// are explicitly in the set. `{0, 1}` is covered as the index `{0, 1, 2}` allows
    /// prefix searches.
    ///
    /// `{0 < 1 < 2}` does not cover `{0, 2}` as no permutation of `{0, 2}`
    /// is a prefix of `{0 < 1 < 2}`.
    ///
    pub def minIndex(searches: List[Search]): List[Search] = match searches {
        case Nil => Nil
        case _ :: Nil => searches
        case _ => minChainCover(searches)
            |> List.map(chain -> List.zip(chain, Vector.empty() :: chain))
            |> List.map(List.map(match (s1, s2) -> Vector.filter(x -> not Vector.memberOf(x, s2), s1)))
            |> List.map(List.fold)
    }

    ///
    /// Find the maximum matching in the bipartite graph `(us, vs, edges)`.
    ///
    def maximumMatching(us: List[Vertex], vs: List[Vertex], adjList: AdjacencyList): Matching = region rc {
        let matching = Array.repeat(rc, List.length(vs), -1);
        let matchingRev = Array.empty(rc, List.length(us));
        def getMatching(u: Vertex, seen: Array[Bool, rc]): Bool \ rc = {
            let cond = v -> if (not Array.get(v, seen)) {
                Array.put(true, v, seen);
                if (Array.get(v, matching) < 0 or getMatching(Array.get(v, matching), seen)) {
                    Array.put(u, v, matching);
                    Array.put(v, u, matchingRev);
                    true
                } else false
            } else false;
            Set.exists(cond, MultiMap.get(u, adjList))
        };
        List.filter(u -> getMatching(u, Array.empty(rc, List.length(us))), us) |>
            List.foldLeft(acc -> u -> Map.insert((u, Array.get(u, matchingRev)), (), acc), Map.empty())
    }

    ///
    /// Find the maximal path starting from the vertex `from_` in `matching`.
    ///
    def maximalPath(from_: Vertex, matching: Matching): List[Edge] = region rc {
        let allPaths = MutMap.empty(rc);
        def f(u: Vertex): Unit \ rc = match MutMap.get(u, allPaths) {
            case None => Map.rangeQueryWith(
                edge -> fst(edge) <=> u,
                edge -> _ -> {
                    let (u0, v0) = edge;
                    f(v0); // Compute the longest path of v0 and store in allPaths
                    let largest = l1 -> l2 -> if (List.length(l1) <= List.length(l2)) l2 else l1;
                    MutMap.putWith(
                        largest, u0,
                        edge :: Option.getWithDefault(List.empty(), MutMap.get(v0, allPaths)),
                        allPaths)
                }, matching)
            case _ => ()
        };
        f(from_);
        match MutMap.get(from_, allPaths) {
            case Some(x) => x
            case None    => List.empty()
        }
    }

    ///
    /// Create a graph from the list of searches `searches`.
    ///
    def graphFromSearches(searches: List[Search]): (List[Vertex], AdjacencyList) =
        // The vertices are are simply the indices of the searches in the list
        let vertices = List.range(0, List.length(searches));
        let zipped = searches |> List.map(Vector.toSet) |> List.zip(vertices);
        let adjList = zipped
            |> List.foldMap(match (u, s) ->
                List.filter(p -> Set.isProperSubsetOf(s, snd(p)), zipped) |>
                    List.map(p -> (u, fst(p)))
            )
            |> List.foldLeft(a -> match (u, v) -> MultiMap.insert(u, v, a), MultiMap.empty());
        (vertices, adjList)

    ///
    /// Find the minimum chain cover of `searches`.
    ///
    def minChainCover(searches: List[Search]): List[List[Search]] =
        let (vertices, adjList) = graphFromSearches(searches);
        let searchesVec = List.toVector(searches);
        let matching = maximumMatching(vertices, vertices, adjList);
        let toSearch = i -> Vector.get(i, searchesVec);
        List.filter(v -> not Map.exists(edge -> _ -> snd(edge) == v, matching), vertices) |>
            List.foldLeft(acc -> u1 -> {
                let chain = toSearch(u1) :: List.map(e -> toSearch(snd(e)), maximalPath(u1, matching));
                chain :: acc
            }, List.empty())

}

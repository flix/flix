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

mod Fixpoint.Phase.IndexSelection {
    use Fixpoint.Ast.Ram.{RamStmt, RamProgram, RelOp, BoolExp, RelSym, RamTerm, Search, ConstructedIndexes, IndexInformation, RowVar, collectRelSym, arityOf, arityOfNonLat};
    use Fixpoint.Predicates.{relSymFromPredType, relSymToPredType, PredType}
    use Fixpoint.UniqueInts
    use Fixpoint.UniqueInts.IndexState
    
    type alias Indexes[r: Eff] = MutMap[RelSym, List[Search], r]

    pub def indexProgram(program: RamProgram): RamProgram = match program {
        case RamProgram.Program(stmt, facts, meta, _) => 
            // This type annotation is apparently needed?
            let (newIndexes, newStmt): (Map[RelSym, Vector[Search]], RamStmt) = region rc {
                let indexes = MutMap.empty(rc);
                let collectedStmt = collectSearchesStmt(indexes, rc, stmt);
                let newIndexes =
                    indexes |>
                    MutMap.toMap |> 
                    Map.map(AutomaticIndexSelection.minIndex) |>
                    Map.map(List.toVector);
                (newIndexes, collectedStmt)
            };
            let indexesFull =
                newIndexes |>
                Map.mapWithKey(relSym -> searches ->
                    Vector.map(search -> 
                        Vector.range(0, arityOfNonLat(relSym)) |>
                        Vector.filter(x -> not Vector.memberOf(x, search)) |>
                        Vector.append(search),
                        searches
                    )
                );
            let indexesMissing = 
                collectRelSym(program) |>
                List.filter(relSym -> not Map.memberOf(relSym, indexesFull)) |>
                List.foldLeft(acc -> relSym -> {
                    let deltaRelSym = relSymFromPredType(relSym, PredType.Delta, meta);
                    let search = match relSymToPredType(relSym, meta) {
                        case PredType.New => 
                            match Map.get(deltaRelSym, indexesFull) {
                                case Some(v) => Vector.get(0, v)
                                case None => Vector.range(0, arityOfNonLat(deltaRelSym))
                            }
                        case _ => Vector.range(0, arityOfNonLat(relSym))
                    };
                    Map.insert(relSym, Vector#{search}, acc)
                }, Map#{});
            let indexes = Map.union(indexesFull, indexesMissing); 
            let (indexedStmt, indexPlacement) = region rc2 {
                let indexState = UniqueInts.empty(rc2);
                let indexedStmt = indexStmt(newStmt, indexes, indexState);
                Map.forEach(relSym -> _ -> {
                    UniqueInts.getIndex((relSym, 0), indexState);
                    ()
                }, indexes);
                let (finalState, _) = indexState;
                (indexedStmt, MutMap.toMap(finalState))
            };
            RamProgram.Program(indexedStmt, facts, meta, (indexes, indexPlacement))
    }

    def indexStmt(stmt: RamStmt, constructedIndexes: ConstructedIndexes, indexState: IndexState[(RelSym, Int32), r]): RamStmt \ r = match stmt {
        case RamStmt.Seq(xs) => RamStmt.Seq(Vector.map(x -> indexStmt(x, constructedIndexes, indexState), xs))
        case RamStmt.Insert(op) => RamStmt.Insert(indexOp(op, constructedIndexes, indexState))
        case RamStmt.Merge(_, _) => stmt
        case RamStmt.Swap(_, _) => stmt
        case RamStmt.Purge(_) => stmt
        case RamStmt.Par(xs) => RamStmt.Par(Vector.map(x -> indexStmt(x, constructedIndexes, indexState), xs))
        case RamStmt.Until(boolExps, body) => RamStmt.Until(boolExps, indexStmt(body, constructedIndexes, indexState))
        case RamStmt.Comment(_) => stmt 
        case RamStmt.EstimateJoinSize(relSym, _, writeTo, search) => 
            let absoluteIndex = UniqueInts.getIndex((relSym, lookupIndex(relSym, search, constructedIndexes)), indexState);
            RamStmt.EstimateJoinSize(relSym, absoluteIndex, writeTo, search)
    }

    def lookupIndex(
        relSym: RelSym, 
        search: Search,
        constructedIndexes: ConstructedIndexes    
    ): Int32 = match Map.get(relSym, constructedIndexes) {
        case Some(indexes) =>
            // We want to be able to search on a `search` of length `k`.
            // If we have a search `[3, 5, 2]` and an index `[X,Y,Z,...]`
            // we need to check that 3, 5 and 2 are in the first 3 positions
            // of the proposed index.
            // In other words some permutation of `search` must be a prefix of
            // index for the index to be usable.
            match Vector.findIndexOf(index ->
                let cutOfIndex = Vector.slice(start = 0, end = Vector.length(search), index);
                Vector.forAll(x -> Vector.memberOf(x, cutOfIndex), search)
            , indexes) {
                case Some(v) => v
                case None => 
                    bug!("In IndexSelection.indexOp: Could not find an index for search ${search} for relation ${relSym}")
            }
        case None => 
            bug!("In IndexSelection.indexOp: ${relSym} has no index!")
    }

    def indexOp(op: RelOp, constructedIndexes: ConstructedIndexes, indexState: IndexState[(RelSym, Int32), r]): RelOp \ r = match op {
        case RelOp.Search(rowVar, relSym, body) =>
            RelOp.Search(rowVar, relSym, indexOp(body, constructedIndexes, indexState))
        case RelOp.Query(rowVar, relSym, boolExps, _, body) =>
            let search =
                Vector.foldLeft(acc -> exp -> match exp {
                    case BoolExp.Eq(RamTerm.RowLoad(_, i, _), _) => i :: acc
                    case BoolExp.Eq(_, RamTerm.RowLoad(_, i, _)) => i :: acc
                    case _ => bug!("In IndexSelection.indexOp: Found non-equality BoolExp in indexOp!")
                }, List.empty(), boolExps) |> List.toVector;
            let index = lookupIndex(relSym, search, constructedIndexes);
            let absoluteIndex = UniqueInts.getIndex((relSym, index), indexState);
            RelOp.Query(rowVar, relSym, boolExps, absoluteIndex, indexOp(body, constructedIndexes, indexState))
        case RelOp.If(boolExps, body) => RelOp.If(boolExps, indexOp(body, constructedIndexes, indexState))
        case RelOp.Functional(rowVar, func, terms, body, arity) =>
            RelOp.Functional(rowVar, func, terms, indexOp(body, constructedIndexes, indexState), arity)
        case RelOp.Project(_, _) => op
    }

    def collectSearchesStmt(indexes: Indexes[r], rc: Region[r], stmt: RamStmt): RamStmt \ r = match stmt {
        case RamStmt.Insert(op) => 
            RamStmt.Insert(collectSearchesOp(op, indexes, MutSet.empty(rc), rc))
        case RamStmt.Merge(_, _) => stmt
        case RamStmt.Swap(_, _) => stmt
        case RamStmt.Purge(_) => stmt
        case RamStmt.Seq(xs) => RamStmt.Seq(Vector.map(collectSearchesStmt(indexes, rc), xs))
        case RamStmt.Par(xs) => RamStmt.Par(Vector.map(collectSearchesStmt(indexes, rc), xs))
        case RamStmt.Until(boolExps, body) => 
            RamStmt.Until(boolExps, collectSearchesStmt(indexes, rc, body))
        case RamStmt.Comment(_) => stmt 
        case RamStmt.EstimateJoinSize(relSym, _, _, search) => 
            collectSearch(relSym, search, indexes);
            stmt
    }

    def collectSearchesOp(
        op: RelOp,
        indexes: Indexes[r],
        seenTuples: MutSet[RowVar, r],
        rc: Region[r]
    ): RelOp \ r = match op {
        case RelOp.Search(rowVar, relSym, body) => 
            MutSet.add(rowVar, seenTuples);
            let res = RelOp.Search(rowVar, relSym, collectSearchesOp(body, indexes, seenTuples, rc));
            MutSet.remove(rowVar, seenTuples);
            res
        case RelOp.Query(rowVar, relSym, boolExps, _, body) =>
            let otherBoolExpList = MutList.empty(rc);
            let equalExpList = MutList.empty(rc);
            let search = boolExps |>
                Vector.foldLeft(acc -> exp -> match exp {
                    case BoolExp.Eq(RamTerm.RowLoad(rv1, i, _), t2) if rv1 == rowVar => 
                        if (isTermGround(t2, seenTuples)) {
                            MutList.push(exp, equalExpList);
                            i :: acc
                        } else {
                            MutList.push(exp, otherBoolExpList);
                            acc
                        }
                    case _ =>
                        MutList.push(exp, otherBoolExpList);
                        acc
                }, List.empty()) |> List.reverse |> List.toVector;
            MutSet.add(rowVar, seenTuples);
            let newBody = collectSearchesOp(body, indexes, seenTuples, rc);
            MutSet.remove(rowVar, seenTuples);
            collectSearch(relSym, search, indexes);
            let otherBoolExps = MutList.toVector(otherBoolExpList);
            let nestedBody = 
                if (Vector.length(otherBoolExps) == 0) newBody 
                else RelOp.If(otherBoolExps, newBody);
            let equalExps = MutList.toVector(equalExpList);
            if (Vector.length(equalExps) == 0)
                RelOp.Search(rowVar, relSym, nestedBody)
            else
                RelOp.Query(rowVar, relSym, equalExps, -1, nestedBody)
        case RelOp.If(boolExps, body) => RelOp.If(boolExps, collectSearchesOp(body, indexes, seenTuples, rc))
        case RelOp.Functional(rowVar, func, terms, body, arity) => 
            MutSet.add(rowVar, seenTuples);
            let res = RelOp.Functional(rowVar, func, terms, collectSearchesOp(body, indexes, seenTuples, rc), arity);
            MutSet.remove(rowVar, seenTuples);
            res
        case RelOp.Project(_, _) => op
    }

    def collectSearch(relSym: RelSym, search: Search, indexes: Indexes[r]): Unit \ r = 
        if (not Vector.isEmpty(search)){
            let searches = MutMap.getWithDefault(relSym, List.empty(), indexes);
            if (not List.memberOf(search, searches))
                MutMap.put(relSym, search :: searches, indexes)
            else ()
        } else ()

    def isTermGround(term: RamTerm, seenTuples: MutSet[RowVar, r]): Bool \ r = match term {
        case RamTerm.RowLoad(rowVar, _, _) =>
            MutSet.memberOf(rowVar, seenTuples)
        case RamTerm.Lit(_, _) => true
        case _ => false
    }

    mod AutomaticIndexSelection {
        //
        // This is based on the paper "Automatic Index Selection for Large-Scale Datalog
        // Computation" by Pavle Subotić, Herbert Jordan, Lijun Chang, Alan Fekete,
        // Bernhard Scholz (https://www.vldb.org/pvldb/vol12/p141-subotic.pdf)
        //
        use Fixpoint.Ast.Ram.{Search}
        type alias Vertex = Int32
        type alias Edge = (Vertex, Vertex)
        // Matching is a `Map` here so we can call `rangeQueryWith`
        type alias Matching = Map[Edge, Unit]
        type alias AdjacencyList = MultiMap[Vertex, Vertex]

        ///
        /// Find the maximum matching in the bipartite graph `(us, vs, edges)`.
        ///
        def maximumMatching(us: List[Vertex], vs: List[Vertex], adjList: AdjacencyList): Matching = region rc {
            let matching = Array.empty(rc, List.length(vs));
            let matchingRev = Array.empty(rc, List.length(us));
            Array.transform(_ -> -1, matching);
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
                case None => List.empty()
            }
        }

        ///
        /// Create a graph from the list of searches `searches`.
        ///
        def graphFromSearches(searches: List[Search]): (List[Vertex], AdjacencyList) =
            // The vertices are are simply the indices of the searches in the list
            let vertices = List.range(0, List.length(searches));
            let zipped = searches |> List.map(s -> Vector.toSet(s)) |> List.zip(vertices); 
            let adjList = List.foldMap(match (u, s) -> {
                    List.filter(p -> Set.isProperSubsetOf(s, snd(p)), zipped) |>
                    List.map(p -> (u, fst(p)))
                }, zipped) |> 
                List.foldLeft(a -> p -> {
                    let (u, v) = p;
                    MultiMap.insert(u, v, a)
                }, MultiMap.empty());
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

        ///
        /// Find the minimum set of indexes that covers `searches`.
        ///
        pub def minIndex(searches: List[Search]): List[Search] = 
            minChainCover(searches) |> 
            List.map(chain -> List.zip(chain, Vector.empty() :: chain)) |>
            List.map(List.map(match (s1, s2) -> Vector.filter(x -> not Vector.memberOf(x, s2), s1))) |>
            List.map(List.fold)

        pub def main(): Unit \ IO = 
            let u0: Search = Vector#{0,1};
            let u1: Search = Vector#{0,2};
            let u2: Search = Vector#{0,1,2};
            let indexes = minIndex(u0::u1::u2::Nil);
            println(indexes)

    }

}
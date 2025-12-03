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

mod Fixpoint3.Phase.IndexSelection {
    use Fixpoint3.Ast.Ram.{RamStmt, RamProgram, RelOp, BoolExp, RelSym, RamTerm, Search, Indexes, IndexInformation, RowVar, arityOf, arityOfNonLat};
    use Fixpoint3.Predicate.{relSymFromPredType, relSymToPredType, PredType, relSymsOfProgram}
    use Fixpoint3.UniqueInts

    /// In the following example, the set of primitive searches `S={{0, 1}, {0, 1, 2}, {0, 2}}`
    /// is produced for relation `B`:
    /// ```
    /// R1(x, y, z) :- A(x, y, z), B(x, y, _, _).
    /// R2(x, y, z) :- A(x, y, z), B(x, y, z, _).
    /// R3(x, y, z) :- A(x, y, z), B(x, _, z, _).
    /// ```
    /// The meaning here is that we need to be able to search `B` on the first 2 attributes,
    /// in some order, the first 3 attributes, in some order, and the first and third
    /// attriubte, in some order.
    ///
    /// A primitive search is unordered, e.g. `0 :: 1 :: Nil` is the same as
    /// `1 :: 0 :: Nil`. In contrast a `Search` is ordered and a `Search` of `0 :: 1 :: Nil`
    /// means that a relation is sorted by attribute `0` and then attribute `1`.
    ///
    type alias PrimitiveSearch = Search

    ///
    /// `PrimitiveSearches` maps `RelSym`s to the primitive searches that must be supported
    /// for the relation.
    ///
    /// In the example for `PrimitiveSearch` we would have
    /// `[B => (0 :: 1 :: Nil) :: (0 :: 1 :: 2 :: Nil) :: (0 :: 2 :: Nil) :: Nil]`.
    ///
    type alias PrimitiveSearches[r: Eff] = MutMap[RelSym, List[PrimitiveSearch], r]

    ///
    /// Creates indexes for each `RelSym` in `program`, places them in memory and
    /// propagates the positions to `Query` operations. To create the appropriate indexes
    /// for a `RelSym`, it first collects the set of `PrimitiveSearch` for each `RelSym` in
    /// `program` and computes the minimal set of `Search`es needed for `RelSym`.
    ///
    /// A `Search` (not to be confused with `RamStmt.Search`) describes the order in which
    /// entries in a tuple are ordered. These are used by indexes to store tuples for
    /// efficient retrieval in range queries. A search is represented a
    /// `Vector[Int32]` where the order is left-to-right in the vector.
    ///
    /// In the example for `PrimitiveSearch` a naive algorithm would create an index for each
    /// primitive search occuring in the program. A better solution, due to Subotić, Pavle, et al.
    /// (Automatic Index Selection for Large-Scale Datalog Computation), is to find the minimal
    /// set of indexes needed for a relation. This is implemented in `AutomaticIndexSelection` is.
    /// Thus, the program consisting of the 3 rules could produce the minimal set of searches to
    /// query relation `B` as `{0 < 1 < 2, 0 < 2}`.
    ///
    /// Note: for relation `A`, since it has no body atoms with bound variables, no
    /// particular search should be created for it, and the single search `0 < 1 < 2`
    /// covers all its cases.
    ///
    /// The searches returned by `minIndex` are expanded to encompass the whole tuple size
    /// of the relation. I.e., the search `0 < 2` gets expanded to `0 < 2 < 1`.
    ///
    /// Returns a new `RamProgram` where `Search`, `Query` and similar statements have
    /// been augmented with the memory position that the index they depend on will be placed.
    ///
    /// For details about memory positions see `lookupIndex`.
    ///
    @Internal
    pub def indexProgram(program: RamProgram): RamProgram = match program {
        case RamProgram.Program(stmt, facts, meta, _, typeInfo) =>
            // This type annotation is apparently needed?
            let (newIndexes, newStmt): (Map[RelSym, Vector[Search]], RamStmt) = region rc {
                let indexes = MutMap.empty(rc);
                let searchedStmt = searchesOfStmt(indexes, stmt);
                let newIndexes = indexes
                    |> MutMap.toMap
                    |> Map.map(AutomaticIndexSelection.minIndex)
                    |> Map.map(List.toVector);
                (newIndexes, searchedStmt)
            };
            // Expand the searches to full arity
            let indexesFull = newIndexes |>
                    Map.mapWithKey(relSym -> searches ->
                        Vector.map(search -> {
                            let unusedIndexes = Vector.range(0, arityOfNonLat(relSym)) |>
                                Vector.filter(x -> not Vector.memberOf(x, search));
                            search ++ unusedIndexes
                        }, searches)
                    );
            // Account for relations that have no body atoms with bound variables
            let indexesMissing =
                relSymsOfProgram(program)
                    |> List.filter(relSym -> not Map.memberOf(relSym, indexesFull))
                    |> List.foldLeft(acc -> relSym -> {
                        let deltaRelSym = relSymFromPredType(relSym, PredType.Delta, meta);
                        let search = match relSymToPredType(relSym, meta) {
                            case PredType.New =>
                                match Map.get(deltaRelSym, indexesFull) {
                                    case Some(v) => Vector.get(0, v)
                                    case None    => Vector.range(0, arityOfNonLat(deltaRelSym))
                                }
                            case _ => Vector.range(0, arityOfNonLat(relSym))
                        };
                        Map.insert(relSym, Vector#{search}, acc)
                    }, Map#{});
            let indexes = Map.union(indexesFull, indexesMissing);
            let (indexedStmt, indexPlacement) = region rc2 {
                let indexState = UniqueInts.empty(rc2);
                let indexedStmt = indexStmt(newStmt, indexes, indexState);
                foreach ((relSym, _) <- indexes) {
                    discard UniqueInts.createIndex((relSym, 0), indexState)
                };
                let finalState = UniqueInts.toMap(indexState);
                (indexedStmt, finalState)
            };
            RamProgram.Program(indexedStmt, facts, meta, (indexes, indexPlacement), typeInfo)
    }

    ///
    /// Returns a new `RamStmt` where `Search`, `Query` and similar statements have
    /// been augmented with the memory position that the index they depend on will be placed.
    /// The positions are generated on the fly using `indexState`.
    ///
    /// For details about memory positions see `lookupIndex`.
    ///
    def indexStmt(stmt: RamStmt, constructedIndexes: Indexes, indexState: UniqueInts[(RelSym, Int32), r]): RamStmt \ r = match stmt {
        case RamStmt.Insert(op)            => RamStmt.Insert(indexOp(op, constructedIndexes, indexState))
        case RamStmt.MergeInto(_, _)       => stmt
        case RamStmt.Swap(_, _)            => stmt
        case RamStmt.Purge(_)              => stmt
        case RamStmt.Seq(xs)               => RamStmt.Seq(Vector.map(x -> indexStmt(x, constructedIndexes, indexState), xs))
        case RamStmt.Par(xs)               => RamStmt.Par(Vector.map(x -> indexStmt(x, constructedIndexes, indexState), xs))
        case RamStmt.Until(boolExps, body) => RamStmt.Until(boolExps, indexStmt(body, constructedIndexes, indexState))
        case RamStmt.Comment(_)            => stmt
    }

    ///
    /// Returns the memory position of the index that uses `Search` for `RelSym`.
    /// If the index has yet to be given a position create a new one.
    ///
    /// Memory positions currently correspond to positions in an array maintained
    /// by the interpreter, though this is not strictly a requirement. The interpreter
    /// is free to use a map or similar data structures.
    ///
    def lookupIndex(
        relSym: RelSym,
        search: Search,
        constructedIndexes: Indexes
    ): Int32 = match Map.get(relSym, constructedIndexes) {
        case Some(indexes) =>
            // We want to be able to search on a `search` of length `k`.
            // If we have a search `3 < 5 < 2` and an index `X < Y < Z <...`
            // we need to check that 3, 5 and 2 are in the first 3 positions
            // of the proposed index.
            // In other words some permutation of `search` must be a prefix of
            // index for the index to be usable.
            let indexOf = indexes |>
                Vector.findIndexOf(index -> {
                    let cutOfIndex = Vector.slice(start = 0, end = Vector.length(search), index);
                    Vector.forAll(x -> Vector.memberOf(x, cutOfIndex), search)
                });
            match indexOf {
                case Some(v) => v
                case None =>
                    bug!("In Fixpoint.Phase.IndexSelection.indexOp: Could not find an index for search ${search} for relation ${relSym}")
            }
        case None => bug!("In Fixpoint.Phase.IndexSelection.indexOp: ${relSym} has no index!")
    }

    ///
    /// Returns a new `RelOp` where `Search`, `Query` and similar statements have
    /// been augmented with the memory position that the index they depend on will be placed.
    /// The positions are generated on the fly using `indexState`.
    ///
    /// For details about memory positions see `lookupIndex`.
    ///
    def indexOp(op: RelOp, constructedIndexes: Indexes, indexState: UniqueInts[(RelSym, Int32), r]): RelOp \ r = match op {
        case RelOp.Search(rowVar, relSym, t, body) =>
            RelOp.Search(rowVar, relSym, t, indexOp(body, constructedIndexes, indexState))
        case RelOp.Query(rowVar, relSym, boolExps, _, t, body) =>
            let search = (List.empty(), boolExps) ||>
                Vector.foldLeft(acc -> exp -> match exp {
                    case BoolExp.Eq(RamTerm.RowLoad(_, i, _, _), _) => i :: acc
                    case BoolExp.Eq(_, RamTerm.RowLoad(_, i, _, _)) => i :: acc
                    case _ =>
                        // This is only a bug when the only index type available is `BPlusTree`
                        bug!("In Fixpoint.Phase.IndexSelection.indexOp: Found non-equality BoolExp in indexOp!")
                }) |> List.toVector;
            let index = lookupIndex(relSym, search, constructedIndexes);
            let absoluteIndex = UniqueInts.createIndex((relSym, index), indexState);
            RelOp.Query(rowVar, relSym, boolExps, absoluteIndex, t, indexOp(body, constructedIndexes, indexState))
        case RelOp.Functional(rowVar, func, terms, body, arity, t) =>
            RelOp.Functional(rowVar, func, terms, indexOp(body, constructedIndexes, indexState), arity, t)
        case RelOp.Project(_, _, _) => op
        case RelOp.If(boolExps, body) => RelOp.If(boolExps, indexOp(body, constructedIndexes, indexState))
    }

    ///
    /// Collects the set of searches for each `RelSym` in `stmt` to `indexes` and lowers
    /// `BoolExp`'s in `Query` ops that are unusable by the index type into `If` ops.
    ///
    /// Returns the result of the described transformation.
    ///
    /// Note: The only index type currently available is `BPlusTree`, which only uses
    /// `Eq`, but future data-structures may make use of `NotMemberOf`, `Leq`, etc.
    ///
    def searchesOfStmt(indexes: PrimitiveSearches[r], stmt: RamStmt): RamStmt \ r = match stmt {
        case RamStmt.Insert(op)            => RamStmt.Insert(searchesOfOp(op, indexes, Set.empty()))
        case RamStmt.MergeInto(_, _)       => stmt
        case RamStmt.Swap(_, _)            => stmt
        case RamStmt.Purge(_)              => stmt
        case RamStmt.Seq(xs)               => RamStmt.Seq(Vector.map(searchesOfStmt(indexes), xs))
        case RamStmt.Par(xs)               => RamStmt.Par(Vector.map(searchesOfStmt(indexes), xs))
        case RamStmt.Until(boolExps, body) => RamStmt.Until(boolExps, searchesOfStmt(indexes, body))
        case RamStmt.Comment(_)            => stmt
    }

    ///
    /// Collects the set of searches for each `RelSym` in `op` to `indexes` given that we
    /// have seen the `RowVar`'s in `seenTuples` and lowers `BoolExp`'s in `Query` ops
    /// that are unusable by the index type into `If` ops.
    ///
    /// Returns the result of the described transformation.
    ///
    /// Note: The only index type currently available is `BPlusTree`, which only uses
    /// `Eq`, but future data-structures may make use of `NotMemberOf`, `Leq`, etc.
    ///
    def searchesOfOp(
        op: RelOp,
        indexes: PrimitiveSearches[r],
        seenTuples: Set[RowVar]
    ): RelOp \ r = match op {
        case RelOp.Search(rowVar, relSym, t, body) =>
            let extendedSeenTuples = Set.insert(rowVar, seenTuples);
            RelOp.Search(rowVar, relSym, t, searchesOfOp(body, indexes, extendedSeenTuples))
        case RelOp.Query(rowVar, relSym, boolExps, _, t, body) => region rc {
            let otherBoolExpList = MutList.empty(rc);
            let equalExpList = MutList.empty(rc);
            let search = boolExps |>
                Vector.foldLeft(acc -> exp -> match exp {
                    case BoolExp.Eq(RamTerm.RowLoad(rv1, i, _, _), t2) if rv1 == rowVar =>
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
            let extendedSeenTuples = Set.insert(rowVar, seenTuples);
            let newBody = searchesOfOp(body, indexes, extendedSeenTuples);
            searchOf(relSym, search, indexes);
            let otherBoolExps = MutList.toVector(otherBoolExpList);
            let nestedBody =
                if (Vector.length(otherBoolExps) == 0) newBody
                else RelOp.If(otherBoolExps, newBody);
            let equalExps = MutList.toVector(equalExpList);
            if (Vector.length(equalExps) == 0)
                RelOp.Search(rowVar, relSym, t, nestedBody)
            else
                RelOp.Query(rowVar, relSym, equalExps, -1, t, nestedBody)
        }
        case RelOp.Functional(rowVar, func, terms, body, arity, t) =>
            let extendedSeenTuples = Set.insert(rowVar, seenTuples);
            RelOp.Functional(rowVar, func, terms, searchesOfOp(body, indexes, extendedSeenTuples), arity, t)
        case RelOp.Project(_, _, _) => op
        case RelOp.If(boolExps, body) => RelOp.If(boolExps, searchesOfOp(body, indexes, seenTuples))
    }

    ///
    /// Returns the search `Search` for `relSym` to `indexes`.
    ///
    def searchOf(relSym: RelSym, search: Search, indexes: PrimitiveSearches[r]): Unit \ r =
        if (not Vector.isEmpty(search)) {
            let searches = MutMap.getWithDefault(relSym, List.empty(), indexes);
            if (not List.memberOf(search, searches)) {
                MutMap.put(relSym, search :: searches, indexes)
            }
        }

    ///
    /// Returns true if `term` is ground (no free variables) w.r.t. `seenTuples`.
    ///
    def isTermGround(term: RamTerm, seenTuples: Set[RowVar]): Bool = match term {
        case RamTerm.Lit(_, _)                => true
        case RamTerm.RowLoad(rowVar, _, _, _) => Set.memberOf(rowVar, seenTuples)
        case                                  _ => false
    }

    mod AutomaticIndexSelection {
        use Fixpoint3.Ast.Ram.Search
        ///
        /// This is based on the paper "Automatic Index Selection for Large-Scale Datalog
        /// Computation" by Pavle Subotić, Herbert Jordan, Lijun Chang, Alan Fekete,
        /// Bernhard Scholz (https://www.vldb.org/pvldb/vol12/p141-subotic.pdf)
        ///
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
        @Internal
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

}

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
/// This phase lowers a high-level `Ram.RamProgram` into a low-level `ExecutableRam.RamProgram`
/// executable by the Interpreter.
///
/// Concretely:
///
/// All `Boxed` values are replaced with representative `Int64` values according to the `Boxing`.
///
/// For relations with multiple indexes,
/// ```
/// MergeInto(newRel, otherRel)
/// ```
/// becomes:
/// ```
/// MergeInto(newRel, otherRelIndex1)
/// MergeInto(newRel, otherRelIndex2)
/// ...
/// ```
/// This ensures that all indexes are updated.
///
/// Similarly:
/// ```
/// Purge(rel)
/// ```
/// becomes:
/// ```
/// Purge(relIndex1)
/// Purge(relIndex2)
/// ...
/// ```
///
/// For:
/// ```
/// Swap(newRel, deltaRel)
/// ```
/// it is transformed into:
/// ```
/// Purge(deltaRelIndex2), Purge(deltaRelIndex3), ...
/// MergeInto(newRel, deltaRelIndex2), MergeInto(newRel, deltaRelIndex3), ...
/// Swap(newRel, deltaRelIndex1)
/// ```
/// The above transformation relies on `newRel` having exactly one index.
///
/// Statements that previously referenced a `RelSym` are instead augmented with explicit
/// index information (currently as `Int32`) indicating which index to use.
///
/// `NotBot`s are removed entirely from the AST. The interpreter handles this explicitly
/// whenever it encounters a `Search` or `Query` with `Latticenal`.
///
/// Every `RowVar` is assigned a unique position in memory (though not strictly necessary).
///
/// Finally instead of a query evaluating terms the terms are written to an array which
/// is kept up to date with the current query values.
///
/// As an example the program:
/// ```
/// search a ∈ A do
///     query {b ∈ B | b[0] = a[0]} do
///         project (a[0]) into R
///     end
/// end
/// ```
/// would be transformed to
/// ```
/// search a ∈ A | [lookupArrForB[0] := a[0]] do
///     query {b ∈ B | lookupArrForB } do
///         project (a[0]) into R
///     end
/// end
/// ```
/// Whenever we bind `a` we update the query-value of `B`.
///
/// An alternative approach would be to have `query` of the form 
/// `query {b ∈ B | searchTerms }`. Then `searchTerms` would be evaluated to generate the
/// restriction for the query. This would create a new object every time the `query` is 
/// met. Furthermore it would have to be evaluated every time `query {b ∈ B | lookupArrForB }`
/// is met. If the query is deeply nested with some values bound higher up in the nesting,
/// we do not need to evaluate these every time.
///
mod Fixpoint3.Phase.Lowering {
    use Fixpoint3.Ast.ExecutableRam.WriteTuple
    use Fixpoint3.Ast.ExecutableRam.{BoolExp => EBoolExp, ConstWrites => EConstWrites, RamProgram => EProgram, RamStmt => ERamStmt, RamTerm => ERamTerm, RelOp => ERelOp}
    use Fixpoint3.Ast.ExecutableRam.BoolExp.{Eq => EEq, Guard1 => EGuard1, Guard2 => EGuard2, Guard3 => EGuard3, Guard4 => EGuard4, Guard5 => EGuard5, IsEmpty => EIsEmpty, Leq => ELeq, Not => ENot, NotMemberOf => ENotMemberOf}
    use Fixpoint3.Ast.ExecutableRam.RamTerm.{App1 => EApp1, App2 => EApp2, App3 => EApp3, App4 => EApp4, App5 => EApp5, LoadFromTuple => ELoadFromTuple, LoadLatVar => ELoadLatVar, Lit => ELit, Meet => EMeet}
    use Fixpoint3.Ast.ExecutableRam.RelOp.{Functional => EFunctional, If => EIf, Project => EProject, Query => EQuery, Search => ESearch}
    use Fixpoint3.Ast.Ram.{arityOf, arityOfNonLat, getTermRamId, IndexInformation, RamId, RelSym, RowVar, Search, toDenotation}
    use Fixpoint3.Ast.Ram.{BoolExp => ABoolExp, RamProgram => AProgram, RamStmt => ARamStmt, RamTerm => ARamTerm, RelOp => ARelOp}
    use Fixpoint3.Ast.Ram.BoolExp.{Eq => AEq, Guard1 => AGuard1, Guard2 => AGuard2, Guard3 => AGuard3, Guard4 => AGuard4, Guard5 => AGuard5, IsEmpty => AIsEmpty, Leq => ALeq, Not => ANot, NotMemberOf => ANotMemberOf, NotBot => ANotBot}
    use Fixpoint3.Ast.Ram.RamTerm.{App1 => AApp1, App2 => AApp2, App3 => AApp3, App4 => AApp4, App5 => AApp5, Lit => ALit, Meet => AMeet, RowLoad => ARowLoad}
    use Fixpoint3.Ast.Ram.RelOp.{Functional => AFunctional, If => AIf, Project => AProject, Query => AQuery, Search => ASearch}
    use Fixpoint3.Ast.Shared.{Denotation, PredSym}
    use Fixpoint3.Boxing
    use Fixpoint3.BoxingType.{Boxing, RamIdToPos};
    use Fixpoint3.UniqueInts
    use Fixpoint3.Util.getOrCrash

    ///
    /// Maps `RowVar`s to their memory position (position in an array the interpreter 
    /// will use).
    ///
    type alias IdToIndex[r: Region] = UniqueInts[RowVar, r]

    ///
    /// Maps `RowVar`s to a list of writes that should be performed whenever the `RowVar` is 
    /// rebound.
    ///
    type alias WriteTos[r: Region] = MutMap[RowVar, List[WriteTuple], r]

    ///
    /// Maps a memory position to writes.
    ///
    /// For example, `[2 => [0 => 38i64]]` means that the first attribute of the `RowVar`,
    /// placed at position 2, should have value 38.
    ///
    type alias ConstWrites[r: Region] = MutMap[Int32, MutMap[Int32, Int64, r], r]

    ///
    /// Contains information in relation to computing meets. For `(list, disjoint)`, `list`
    /// will contain the currently bound `RowVar`s in reverse order. `disjoint` contains
    /// information about which `RowVar`s' lattice values should be meeted.
    ///
    /// For example, consider:
    /// ```
    /// search x ∈ A do
    ///   search y ∈ B do
    ///     project (0; Meet(x, y)) into ΔC'
    ///   end
    /// end
    /// ```
    /// Initially, `MeetWithMap` is `(Nil, {})`. When the `RelOp` `search y ∈ B do` is
    /// lowered, `x` is registered, and `MeetWithMap` becomes `(x :: Nil, {})`. When the 
    /// `project` is lowered, it `(y :: x :: Nil, {})`. Finally, `Meet(x, y)` is lowered, 
    /// which updates `disjoint` to `{y = x}`.
    ///
    /// The above code snippet is lowered to:
    /// ```
    /// search x ∈ A do
    ///   search y ∈ B | x_y_meet = Meet(x, y) do
    ///     project (0; x_y_meet) into ΔC'
    ///   end
    /// end
    /// ```
    /// Note that the lattice value of `y` is not actually stored, only the meet.
    ///
    type alias MeetWithMap[r: Region] = (List[RowVar], MutDisjointSets[RowVar, r])

    ///
    /// Lower `program` from `Ram` to `ExecutableRam`.
    ///
    /// Returns the result along with the `Boxing` used for the program
    ///
    @Internal
    pub def lowerProgram(rc: Region[r], withProv: Bool, program: AProgram): (EProgram, Boxing[r]) \ r = match program { 
        case AProgram.Program(stmt, _, meta, indexInfo) =>
            let idToIndex = UniqueInts.empty(rc);
            let (boxing, newFacts, idToBoxing) = Boxing.initialize(rc, withProv, program);
            let writeTo = (MutMap.empty(rc), MutMap.empty(rc));
            let loweredStmt = lowerStmt(rc, idToIndex, (idToBoxing, boxing), writeTo, indexInfo, stmt);

            let constWrites = snd(writeTo) |> MutMap.foldWithKey(acc1 -> outerPos -> inner ->
                MutMap.foldLeftWithKey(acc2 -> innerPos -> value -> (outerPos, innerPos, value) :: acc2, acc1, inner)
            , Nil) |> List.toVector;
            // Compute arities
            let arityInformation = collectRowVarArity(program);
            let numOfArities = UniqueInts.get(idToIndex);
            let arities = Array.empty(rc, numOfArities);
            arityInformation |> List.forEach(match (rv, arity) -> Array.put(arity, UniqueInts.getIndex(rv, idToIndex), arities));
            (EProgram.Program(loweredStmt, newFacts, meta, indexInfo, (Array.toVector(arities), constWrites), idToBoxing), boxing)
    }

    ///
    /// Lowers `stmt` from `Ram` to `ExecutableRam`.
    ///
    /// `idToIndex` assigns `RowVar`s to memory positions.
    ///
    /// `boxingInfo` is used to find the boxing information of `Boxed` values and to compute
    /// the `Int64` representative.
    ///
    /// `writeTo` contains write information. See `WriteTos` and `ConstWrites`.
    ///
    /// `indexInfo` is used to replace `RelSym` with an indexes.
    ///
    def lowerStmt(
        rc: Region[r],
        idToIndex: IdToIndex[r],
        boxingInfo: (RamIdToPos, Boxing[r]),
        writeTo: (WriteTos[r], ConstWrites[r]),
        indexInfo: IndexInformation,
        stmt: ARamStmt
    ): ERamStmt \ r =
        let lowerStmtRec = lowerStmt(rc, idToIndex, boxingInfo, writeTo, indexInfo);
        match stmt {
            case ARamStmt.Insert(op) =>
                let newOp = lowerOp(rc, idToIndex, boxingInfo, writeTo, indexInfo, (Nil, MutDisjointSets.empty(rc)), op);
                ERamStmt.Insert(newOp)
            case ARamStmt.MergeInto(newRel, otherRel) =>
                // `MergeInto` should simply be repeated for all indexes built on `deltaRel`.
                let (constructed, placements) = indexInfo;
                let newRelPos = getOrCrash(Map.get((newRel, 0), placements));
                let lowerToMerge = index -> ERamStmt.MergeInto(newRelPos, index, toDenotation(newRel));
                let deltaRels = Vector.length(getOrCrash(Map.get(otherRel, constructed)));
                let merges = Vector.map(index -> lowerToMerge(getOrCrash(Map.get((otherRel, index), placements))), Vector.range(0, deltaRels));
                match Vector.length(merges) {
                    case 1 => Vector.get(0, merges)
                    case _ => ERamStmt.Par(merges)
                }
            case ARamStmt.Swap(newRel, deltaRel) =>
                let (constructed, placements) = indexInfo;
                let newRelPos = getOrCrash(Map.get((newRel, 0), placements));
                let lowerToMerge = index -> ERamStmt.MergeInto(newRelPos, index, toDenotation(newRel));
                let lowerToPurge = index -> ERamStmt.Purge(index);
                let deltaRels = Vector.length(getOrCrash(Map.get(deltaRel, constructed)));
                let swapPart = ERamStmt.Swap(newRelPos, getOrCrash(Map.get((deltaRel, 0), placements)));
                match Vector.length(getOrCrash(Map.get(deltaRel, constructed))) {
                    // If there is only 1 index on `deltaRel` we can simply swap.
                    case 1 => swapPart
                    case _ =>
                    // We can still swap `newRel` and `deltaRelIndex[0]`. All other indexes will
                    // instead treated as a `Purge(deltaRel[i])` followed by a `MergeInto(newRel, deltaRel[i])`.
                    // Purges are cheap and performed sequentially. Merges can be expensive and are performed in
                    // parallel.
                    let merges =
                        Vector.range(1, deltaRels) |>
                        Vector.map(index -> lowerToMerge(getOrCrash(Map.get((deltaRel, index), placements))));
                    let purges = Vector.range(1, deltaRels) |>
                        Vector.map(index -> lowerToPurge(getOrCrash(Map.get((deltaRel, index), placements))));
                    if(Vector.length(merges) == 1) {
                        ERamStmt.Seq(Vector#{Vector.get(0, merges), swapPart})
                    } else {
                        ERamStmt.Seq(Vector#{ERamStmt.Seq(purges), ERamStmt.Par(merges), swapPart})
                    }
                }
            case ARamStmt.Purge(rel) =>
                // `Purge` should simply be repeated for all indexes built on `rel`.
                let (constructed, placements) = indexInfo;
                let indexNums = Vector.range(0, Vector.length(getOrCrash(Map.get(rel, constructed))));
                let purges = Vector.map(i -> ERamStmt.Purge(getOrCrash(Map.get((rel, i), placements))), indexNums);
                if (Vector.length(purges) == 1) {
                    Vector.get(0, purges)
                } else {
                    ERamStmt.Seq(purges)
                }
            case ARamStmt.Seq(xs) => Vector.map(x -> lowerStmtRec(x), xs) |> ERamStmt.Seq
            case ARamStmt.Par(xs) => Vector.map(x -> lowerStmtRec(x), xs) |> ERamStmt.Par
            case ARamStmt.Until(test, body) =>
                let newTests = test |>
                    Vector.filterMap(lowerBool(idToIndex, boxingInfo, indexInfo, (Nil, MutDisjointSets.empty(rc))));
                let newBody = lowerStmtRec(body);
                ERamStmt.Until(newTests, newBody)
            case ARamStmt.EstimateJoinSize(_, relIndex, estWriteTo, attr) =>
                ERamStmt.EstimateJoinSize(relIndex, estWriteTo, attr)
            case ARamStmt.Comment(s) => ERamStmt.Comment(s)
    }

    ///
    /// Lower `op` from `Ram` to `ExecutableRam`.
    ///
    /// See the description of the module and `lowerStmt`.
    ///
    def lowerOp(
        rc: Region[r],
        idToIndex: IdToIndex[r],
        boxingInfo: (RamIdToPos, Boxing[r]),
        writeTo: (WriteTos[r], ConstWrites[r]),
        indexInfo: IndexInformation,
        meetWithMap: MeetWithMap[r],
        op: ARelOp
    ): ERelOp \ r =
        let lowerOpRec = lowerOp(rc, idToIndex, boxingInfo, writeTo, indexInfo, meetWithMap);
        let lowerOpRecNewRowVar = rv -> lowerOp(rc, idToIndex, boxingInfo, writeTo, indexInfo, addRowVarToMeetWithMap(rv, meetWithMap));
        match op {
            case ASearch(rv, relSym, body) =>
                let den = toDenotation(relSym);
                let loweredBody = lowerOpRecNewRowVar(rv, body);
                let thisWriteTo = getWriteTo(rv, writeTo);
                let placements = snd(indexInfo);
                let relPos = getOrCrash(Map.get((relSym, 0), placements));
                let oldPos = computeMeetWithPos(rv, relSym, idToIndex, meetWithMap);
                ESearch(lowerRowVar(rv, idToIndex), relPos, oldPos, den, thisWriteTo, loweredBody)
            case AQuery(rv, relSym, tests, index, body) =>
                let den = toDenotation(relSym);
                let (idToBoxing, boxing) = boxingInfo;
                let loweredBody = lowerOpRecNewRowVar(rv, body);
                let oldPos = computeMeetWithPos(rv, relSym, idToIndex, meetWithMap);
                let thisWriteTo = getWriteTo(rv, writeTo);
                let otherBools = Vector.filterMap(x -> match x {
                    case ABoolExp.Eq(ARamTerm.RowLoad(rv1, i1, _), ARamTerm.RowLoad(rv2, i2, _)) if rv == rv1 =>
                        addWriteTo(rv2, i2, rv1, i1, idToIndex, writeTo);
                        None
                    case ABoolExp.Eq(ARamTerm.RowLoad(rv2, i2, _), ARamTerm.RowLoad(rv1, i1, _)) if rv == rv1 =>
                        addWriteTo(rv2, i2, rv1, i1, idToIndex, writeTo);
                        None
                    case ABoolExp.Eq(ARamTerm.RowLoad(rv1, i, _), ARamTerm.Lit(val, id)) if rv == rv1 =>
                        let unboxed = Boxing.unboxWith(val, getOrCrash(Map.get(id, idToBoxing)), boxing);
                        addConstWriteTo(unboxed, rv, i, idToIndex, writeTo, rc);
                        None
                    case ABoolExp.Eq(ARamTerm.Lit(val, id), ARamTerm.RowLoad(rv1, i, _)) if rv == rv1 =>
                        let unboxed = Boxing.unboxWith(val, getOrCrash(Map.get(id, idToBoxing)), boxing);
                        addConstWriteTo(unboxed, rv, i, idToIndex, writeTo, rc);
                        None
                    case _ => Some(x)
                }, tests);
                if(Vector.length(otherBools) != 0)
                    unchecked_cast(bug!("Bug in Fixpoint.Lowering: Bools except equality in query") as _ \ r)
                else ();
                EQuery(lowerRowVar(rv, idToIndex), index, oldPos, den, thisWriteTo, loweredBody)
            case AFunctional(rv, f, terms, body, arity) =>
                let (idToBoxing, _) = boxingInfo;
                let idToMarhsalled = id -> getOrCrash(Map.get(id, idToBoxing));
                let loweredBody = lowerOpRec(body);
                let thisWriteTo = getWriteTo(rv, writeTo);
                let to = Vector.map(i -> idToMarhsalled(RamId.TuplePos(rv, i)), Vector.range(0, arity));
                EFunctional(
                    lowerRowVar(rv, idToIndex), f,
                    Vector.map(lowerTerm(idToIndex, boxingInfo, meetWithMap), terms),
                    thisWriteTo, loweredBody, to
                )
            case AProject(terms, s) =>
                let (_, placements) = indexInfo;
                let newRelPos = getOrCrash(Map.get((s, 0), placements));
                let den = toDenotation(s);
                let loweredTerms = Vector.map(lowerTerm(idToIndex, boxingInfo, meetWithMap), terms);
                EProject(
                    loweredTerms, newRelPos, den
                )
            case AIf(boolExps, body) =>
                EIf(Vector.filterMap(lowerBool(idToIndex, boxingInfo, indexInfo, meetWithMap), boolExps), lowerOpRec(body))
        }

    ///
    /// Lower `term` from `Ram` to `ExecutableRam`.
    ///
    /// Computes unification for `Meet`. See `lowerMeet`.
    ///
    /// The following transformations are the most important.
    ///
    ///     1. `Lit(boxedVal)` is augmented with the `Int64` representative of `boxedVal`.
    ///     2. `RowVar`s are replaced by memory positions.
    ///     3. `RowLoad(rv, i, rel)` is replaced by `LoadFromTuple` or `LoadLatVar` depending
    ///         on whether `RowLoad(rv, i, rel)` refers to the lattice value or a 'normal' value.
    ///     4. `AMeet(_)` is replaced by `LoadLatVar`.
    ///
    def lowerTerm(
        idToIndex: IdToIndex[r],
        boxingInfo: (RamIdToPos, Boxing[r]),
        meetWithMap: MeetWithMap[r],
        term: ARamTerm
    ): ERamTerm \ r =
        let lowerT = lowerTerm(idToIndex, boxingInfo, meetWithMap);
        let (idToBoxing, boxing) = boxingInfo;
        let termToBoxing = t -> getOrCrash(Map.get(getTermRamId(t), idToBoxing));
        match term {
            case ALit(val, id) => ELit(Boxing.unboxWith(val, getOrCrash(Map.get(id, idToBoxing)), boxing), val)
            case ARowLoad(rv, index, RelSym.Symbol(_, arity, den)) =>
                match den {
                    case Denotation.Relational => ELoadFromTuple(UniqueInts.getIndex(rv, idToIndex), index, termToBoxing(term))
                    case Denotation.Latticenal(_, _, _, _) =>
                        if(index < arity - 1) {
                            ELoadFromTuple(UniqueInts.getIndex(rv, idToIndex), index, termToBoxing(term))
                        } else {
                            ELoadLatVar(UniqueInts.getIndex(rv, idToIndex), termToBoxing(term))
                        }
                }
            case AMeet(_, _, _, _) =>
                let representingRowVar = lowerMeet(term, idToIndex, meetWithMap);
                let pos = computeLatticeMeetPos(representingRowVar, idToIndex, meetWithMap);
                ELoadLatVar(pos, termToBoxing(term))
            case AApp1(f, t1, _)                   => EApp1(f, lowerT(t1), termToBoxing(term))
            case AApp2(f, t1, t2, _)               => EApp2(f, lowerT(t1), lowerT(t2), termToBoxing(term))
            case AApp3(f, t1, t2, t3, _)           => EApp3(f, lowerT(t1), lowerT(t2), lowerT(t3), termToBoxing(term))
            case AApp4(f, t1, t2, t3, t4, _)       => EApp4(f, lowerT(t1), lowerT(t2), lowerT(t3), lowerT(t4), termToBoxing(term))
            case AApp5(f, t1, t2, t3, t4, t5, _)   => EApp5(f, lowerT(t1), lowerT(t2), lowerT(t3), lowerT(t4), lowerT(t5), termToBoxing(term))
        }

    ///
    /// Return the position the `Meet` where `term` will be stored.
    ///
    /// Unify every `RowVar` meet during unpacking of `Meet` in `meetWithMap`.
    ///
    def lowerMeet(
        term: ARamTerm,
        idToIndex: IdToIndex[r],
        meetWithMap: MeetWithMap[r]
    ): RowVar \ r = match term {
        case ARowLoad(rv, _, _) =>
            let (_, meetDisjointSet) = meetWithMap;
            MutDisjointSets.makeSet(rv, meetDisjointSet);
            rv
        case AMeet(_, t1, (rv, _), _) =>
            let (_, meetDisjointSet) = meetWithMap;
            let prevRv = lowerMeet(t1, idToIndex, meetWithMap);
            MutDisjointSets.makeSet(rv, meetDisjointSet);
            MutDisjointSets.makeSet(prevRv, meetDisjointSet);
            MutDisjointSets.union(rv, prevRv, meetDisjointSet);
            rv
        case _ => unreachable!()
    }

    ///
    /// Lower `boolExp` from `Ram` to `ExecutableRam`.
    ///
    /// Returns `Some(lowered)` for `boolExp != NotBot(_)`.
    ///
    /// Returns `None` for `NotBot` as it is handled directly by the interpreter.
    ///
    /// Primarily lowers terms and replaces `RelSym`s with a concrete index.
    ///
    def lowerBool(
        idToIndex: IdToIndex[r],
        boxingInfo: (RamIdToPos, Boxing[r]),
        indexInfo: IndexInformation,
        meetWithMap: MeetWithMap[r],
        boolExp: ABoolExp
    ): Option[EBoolExp] \ r =
        let lowerT = lowerTerm(idToIndex, boxingInfo, meetWithMap);
        match boolExp {
            case ANot(s) =>
                lowerBool(idToIndex, boxingInfo, indexInfo, meetWithMap, s) |>
                Option.map(ENot)
            case AIsEmpty(s) =>
                let (_, placements) = indexInfo;
                let sPos = getOrCrash(Map.get((s, 0), placements));
                Some(EIsEmpty(sPos))
            case ANotMemberOf(terms, s) =>
                let (_, placements) = indexInfo;
                let den = toDenotation(s);
                let sPos = getOrCrash(Map.get((s, 0), placements));
                Some(ENotMemberOf(Vector.map(lowerT, terms), sPos, den))
            case ANotBot(t1, _, _) =>
                lowerMeet(t1, idToIndex, meetWithMap);
                None
            case ALeq(box, rv, relSym) =>
                let leq = match toDenotation(relSym) {
                    case Denotation.Relational => _ -> _ -> bug!("Using lattice logic on normal relation")
                    case Denotation.Latticenal(_, leq, _, _) => leq
                };
                Some(ELeq(box, leq, lowerRowVar(rv, idToIndex)))
            case AEq(term1, term2) =>
                Some(EEq(lowerT(term1), lowerT(term2)))
            case AGuard1(f, term1) => Some(EGuard1(f, lowerT(term1)))
            case AGuard2(f, term1, term2) => Some(EGuard2(f, lowerT(term1), lowerT(term2)))
            case AGuard3(f, term1, term2, term3) => Some(EGuard3(f, lowerT(term1), lowerT(term2), lowerT(term3)))
            case AGuard4(f, term1, term2, term3, term4) => Some(EGuard4(f, lowerT(term1), lowerT(term2), lowerT(term3), lowerT(term4)))
            case AGuard5(f, term1, term2, term3, term4, term5) => Some(EGuard5(f, lowerT(term1), lowerT(term2), lowerT(term3), lowerT(term4), lowerT(term5)))
        }

    ///
    /// Lower `rowVar` to a memory position.
    ///
    /// Note: every `RowVar` is assigned a unique memory position.
    ///
    def lowerRowVar(rowVar: RowVar, idToIndex: IdToIndex[r]): Int32 \ r =
        UniqueInts.getIndex(rowVar, idToIndex)

    ///
    /// Add `rv` to `meetWithMap`.
    ///
    /// Concretely, for `meetWithMap = (seenRV, equalitySet)`, return
    /// `(rv:: seen, equalitySet')` where `equalitySet` is simply updated with `rv` as a 
    /// possible value.
    ///
    def addRowVarToMeetWithMap(rv: RowVar, meetWithMap: MeetWithMap[r]): MeetWithMap[r] \ r =
        let (prevRowVars, meetDisjointSet) = meetWithMap;
        MutDisjointSets.makeSet(rv, meetDisjointSet);
        (rv :: prevRowVars, meetDisjointSet)

    ///
    /// Compute the memory position of the value that the lattice value associated with
    /// `rv` should be meeted (greatest lower bound) with.
    ///
    /// Returns -1 for 'normal' relations. Returns `idToIndex[rv]` when `rv` is not
    /// supposed to be meeted with anything. Otherwise, returns the memory position
    /// of the value that `rv` should be meeted with.
    ///
    /// For example, the program:
    /// ```
    /// R(;y) :- A(;x), B(;x), ... .
    /// ```
    /// is compiled to:
    /// ```
    /// search a ∈ A do
    ///     search b ∈ B | meet_a_b = meet(a, b) do
    ///         ...
    ///     end
    /// end
    /// ```
    /// where `A` and `B`. If `A` and `B` are lattice relations then for `RowVar` `a`
    /// it will return `idToIndex[a]` signalling that it should not meeted, but is a lattice-relation.
    ///
    /// For `RowVar` `b`, it also returns `idToIndex[a]`, but this is not the same as
    /// `idToIndex[b]` signalling that a meet should be performed with the value at
    /// `idToIndex[a]`.
    ///
    def computeMeetWithPos(
        rv: RowVar,
        relSym: RelSym,
        idToIndex: IdToIndex[r],
        meetWithMap: MeetWithMap[r]
    ): Int32 \ r = match toDenotation(relSym) {
        case Denotation.Relational => -1
        case Denotation.Latticenal(_, _, _, _) => computeLatticeMeetPos(rv, idToIndex, meetWithMap)
    }

    ///
    /// Returns the memory position of the first value equivalent to `rv`, w.r.t. 
    /// `meetWithMap`. If no such value exists, returns the memory position of `rv`.
    ///
    /// See `computeMeetWithPos`.
    ///
    def computeLatticeMeetPos(
        rv: RowVar,
        idToIndex: IdToIndex[r],
        meetWithMap: MeetWithMap[r]
    ): Int32 \ r = {
        let (prevRowVars, meetDisjointSet) = meetWithMap;
        let firstMatching = List.findLeft(x -> MutDisjointSets.equivalent(rv, x, meetDisjointSet), prevRowVars);
        match firstMatching {
            case None => lowerRowVar(rv, idToIndex)
            case Some(v) => lowerRowVar(v, idToIndex)
        }
    }

    ///
    /// Return a `Vector` of writes that should be performed when `rowVar` is bound.
    ///
    def getWriteTo(rowVar: RowVar, writeTo: (WriteTos[r], ConstWrites[r])): Vector[WriteTuple] \ r =
        let varWrite = fst(writeTo);
        List.toVector(Option.getWithDefault(Nil, MutMap.get(rowVar, varWrite)))

    ///
    /// Register that when `fromRowVar` is bound it should perform a write to `toRowVar`.
    ///
    /// Concretely, we need to preserve `fromRowVar[fromIndex] == toRowVar[toIndex]`.
    ///
    def addWriteTo(
        fromRowVar: RowVar,
        fromIndex: Int32,
        toRowVar: RowVar,
        toIndex: Int32,
        idToIndex: IdToIndex[r],
        writeTo: (WriteTos[r], ConstWrites[r])
    ): Unit \ r =
        let varWrite = fst(writeTo);
        let written = (fromIndex, lowerRowVar(toRowVar, idToIndex), toIndex);
        MutMap.putWith(_ -> old -> written :: old, fromRowVar, written :: Nil, varWrite)

    ///
    /// Register that `toRowVar` has the value `value`.
    ///
    /// Concretely, maintains that `toRowVar[toIndex] == value`.
    ///
    def addConstWriteTo(
        value: Int64,
        toRowVar: RowVar,
        toIndex: Int32,
        idToIndex: IdToIndex[r],
        writeTo: (WriteTos[r], ConstWrites[r]),
        rc: Region[r]
    ): Unit \ r =
        let constWrite = snd(writeTo);
        let toTuplePos = lowerRowVar(toRowVar, idToIndex);
        MutMap.getOrElsePut(toTuplePos, MutMap.empty(rc), constWrite) |>
            MutMap.putWith(v1 -> v2 ->
                if (v1 != v2)
                    bug!("Bug in Fixpoint.Lowering: Different value for position. Should have been removed by Hoisting")
                else v1
            , toIndex, value)

    ///
    /// Returns a list `(RowVar, arity)` in `program` in the order of the nesting.
    ///
    def collectRowVarArity(program: AProgram): List[(RowVar, Int32)] = 
        collectRowVarRelSym(program) |> List.map(match (rv, RelSym.Symbol(_, arity, _)) -> (rv, arity))

    ///
    /// Returns a list of pairs `(RowVar, RelSym)` in `program` in the order of the nesting.
    ///
    def collectRowVarRelSym(program: AProgram): List[(RowVar, RelSym)] = match program {
        case AProgram.Program(stmt, _, _, _) => 
            collectRowVarRelSymStmt(stmt)
    }

    ///
    /// Returns a list of pairs `(RowVar, RelSym)` in `stmt`.
    ///
    def collectRowVarRelSymStmt(stmt: ARamStmt): List[(RowVar, RelSym)] \ r = match stmt {
        case ARamStmt.Insert(op) => collectRowVarRelSymOp(op)
        case ARamStmt.MergeInto(_, _) => Nil
        case ARamStmt.Swap(_, _) => Nil
        case ARamStmt.Purge(_) => Nil
        case ARamStmt.Seq(stmts) => Vector.foldLeft(acc -> x -> List.append(collectRowVarRelSymStmt(x), acc), Nil, stmts)
        case ARamStmt.Par(stmts) => Vector.foldLeft(acc -> x -> List.append(collectRowVarRelSymStmt(x), acc), Nil, stmts)
        case ARamStmt.Until(_, rest) => collectRowVarRelSymStmt(rest)
        case ARamStmt.Comment(_) => Nil
        case ARamStmt.EstimateJoinSize(_, _, _, _) => Nil
    }

    ///
    /// Returns a list of pairs `(RowVar, RelSym)` in `op` in the order of nesting.
    ///
    def collectRowVarRelSymOp(op: ARelOp): List[(RowVar, RelSym)] \ r = match op {
        case ASearch(rv, relSym, rest) => (rv, relSym) :: collectRowVarRelSymOp(rest)
        case AQuery(rv, relSym, _, _, rest) => (rv, relSym) :: collectRowVarRelSymOp(rest)
        case AFunctional(_, _, _, rest, _) => collectRowVarRelSymOp(rest)
        case AProject(_, _) => Nil
        case AIf(_, rest) => collectRowVarRelSymOp(rest)
    }

}

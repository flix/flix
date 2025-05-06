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

mod Fixpoint.Phase.Lowering {
    use Fixpoint.Ast.ExecutableRam.{ConstWrites => EConstWrites}
    use Fixpoint.Boxed.BoxedBool
    use Fixpoint.Ast.Ram.IndexInformation
    use Fixpoint.Ast.Ram.Search
    use Fixpoint.UniqueInts
    use Fixpoint.Ast.ExecutableRam.WriteTuple
    use Fixpoint.Interpreter.Database
    use Fixpoint.Boxing.Boxing;
    use Fixpoint.Interpreter.DatabaseExternal
    use Fixpoint.Ast.Ram.{RamProgram => AProgram, getTermRamId, RamId, toDenotation, arityOf, arityOfNonLat}
    use Fixpoint.Ast.Ram.{RowVar, RamStmt => ARamStmt, RelOp => ARelOp, RelSym, RamTerm => ARamTerm, BoolExp => ABoolExp}
    use Fixpoint.Ast.Ram.RelOp.{Search => ASearch, Query => AQuery, Functional => AFunctional, Project => AProject, If => AIf}
    use Fixpoint.Ast.Ram.BoolExp.{Not => ANot, IsEmpty => AIsEmpty, NotMemberOf => ANotMemberOf, NotBot => ANotBot, Leq => ALeq, Eq => AEq, Guard0 => AGuard0, Guard1 => AGuard1, Guard2 => AGuard2, Guard3 => AGuard3, Guard4 => AGuard4, Guard5 => AGuard5}
    use Fixpoint.Ast.Ram.RamTerm.{Lit => ALit, RowLoad => ARowLoad, Meet => AMeet, App0 => AApp0, App1 => AApp1, App2 => AApp2, App3 => AApp3, App4 => AApp4, App5 => AApp5}


    use Fixpoint.Ast.ExecutableRam.{RamStmt => ERamStmt, RelOp => ERelOp, RamTerm => ERamTerm, BoolExp => EBoolExp}
    use Fixpoint.Ast.ExecutableRam.RelOp.{Search => ESearch, SearchLattice => ESearchLattice, QueryLattice => EQueryLattice, Query => EQuery, Functional => EFunctional, Project => EProject, If => EIf}
    use Fixpoint.Ast.ExecutableRam.BoolExp.{Not => ENot, IsEmpty => EIsEmpty, NotMemberOf => ENotMemberOf, Leq => ELeq, Eq => EEq, Guard0 => EGuard0, Guard1 => EGuard1, Guard2 => EGuard2, Guard3 => EGuard3, Guard4 => EGuard4, Guard5 => EGuard5}
    use Fixpoint.Ast.ExecutableRam.RamTerm.{Lit => ELit, Meet => EMeet, App0 => EApp0, App1 => EApp1, App2 => EApp2, App3 => EApp3, App4 => EApp4, App5 => EApp5, LoadFromTuple => ELoadFromTuple, LoadLatVar => ELoadLatVar}

    use Fixpoint.Ast.Ram
    use Fixpoint.Ast.ExecutableRam.{RamProgram => EProgram}

    use Fixpoint.Ast.Shared.{Denotation, PredSym}

    use Fixpoint.Boxing.{Equality, RamIdToMarshal}
    use Fixpoint.Boxing
    use Fixpoint.Boxed
    use Fixpoint.Debugging

    use Fixpoint.Counter
    use Fixpoint.Counter.Counter

    // Maps RowVars to their memory-position.
    type alias IdToIndex[r: Region] = UniqueInts.IndexState[RowVar, r]
    type alias WriteTos[r: Region] = MutMap[RowVar, List[WriteTuple], r]
    type alias ConstWrites[r: Region] = MutMap[Int32, MutMap[Int32, Int64, r], r]
    type alias MeetWithMap[r: Region] = (List[RowVar], MutDisjointSets[RowVar, r])

    def relSymToDenId(relSym: RelSym, idToMarshall: RamIdToMarshal): Int32 = match relSym {
        case RelSym.Symbol(_, _, Denotation.Relational) => -1
        case RelSym.Symbol(PredSym.PredSym(_, id), arity, Denotation.Latticenal(_, _, _, _)) =>
            unwrap(Map.get(RamId.RelPos(id, arity - 1), idToMarshall))
    }

    @Internal
    pub def lowerProgram(rc: Region[r], program: AProgram): (EProgram, Boxing[r]) \ r = match program { 
        case AProgram.Program(stmt, facts, meta, indexInfo) =>
            let idToIndex = UniqueInts.empty(rc);
            let (marshall, idToMarshall) = Boxing.initialize(rc, program);
            let writeTo = (MutMap.empty(rc), MutMap.empty(rc));
            let loweredStmt = lowerStmt(stmt, rc, idToIndex, (idToMarshall, marshall), writeTo, indexInfo);
            // Lower facts
            let newFacts =
                facts |>
                Map.mapWithKey(k -> values ->
                    let RelSym.Symbol(PredSym.PredSym(_, id), _, den) = k;
                    values |> 
                    Vector.map(vec -> match den {
                        case Denotation.Relational =>
                            let tuple = vec |>
                                Vector.mapWithIndex(i -> v -> {
                                    Boxing.unboxWith(v, unwrap(Map.get(RamId.RelPos(id, i), idToMarshall)), marshall)
                                });
                            (tuple, Boxed.None)
                        case Denotation.Latticenal(_, _, _ ,_) =>
                            let len = Vector.length(vec);
                            let tuple = Array.empty(rc, len - 1);
                            vec |> Vector.forEachWithIndex(i -> term -> {
                                if (i < len - 1)
                                    Array.put(Boxing.unboxWith(term, unwrap(Map.get(RamId.RelPos(id, i), idToMarshall)), marshall), i, tuple)
                                else ()
                            });
                            let latVal = Vector.get(len - 1, vec);
                            (Array.toVector(tuple), latVal)
                    })
                );

            let constWrites = snd(writeTo) |> MutMap.foldWithKey(acc1 -> outerPos -> inner ->
                MutMap.foldLeftWithKey(acc2 -> innerPos -> value -> (outerPos, innerPos, value) :: acc2, acc1, inner)
            , Nil) |> List.toVector;
            // Compute arities
            let arityInformation = Ram.collectRowVarArity(program);
            let numOfArities = UniqueInts.peek(idToIndex);
            let arities = Array.empty(rc, numOfArities);
            arityInformation |> List.forEach(match (rv, arity) -> Array.put(arity, UniqueInts.getIndex(rv, idToIndex), arities));
            (EProgram.Program(loweredStmt, newFacts, meta, indexInfo, (Array.toVector(arities), constWrites), idToMarshall), marshall)
    }

    @Internal
    def lowerStmt(stmt: ARamStmt, rc: Region[r], idToIndex: IdToIndex[r], marshalling: (RamIdToMarshal, Boxing[r]), writeTo: (WriteTos[r], ConstWrites[r]), indexInfo: IndexInformation): ERamStmt \ r =
        let lowerStmtRec = x -> lowerStmt(x, rc, idToIndex, marshalling, writeTo, indexInfo);
        match stmt {
            case ARamStmt.Insert(op) =>
                let newOp = lowerOp(op, rc, idToIndex, marshalling, writeTo, indexInfo, (Nil, MutDisjointSets.empty(rc)));
                ERamStmt.Insert(newOp)
            case ARamStmt.Seq(xs) => Vector.map(x -> lowerStmtRec(x), xs) |> ERamStmt.Seq
            case ARamStmt.Par(xs) => Vector.map(x -> lowerStmtRec(x), xs) |> ERamStmt.Par
            case ARamStmt.Until(test, body) => ERamStmt.Until(Vector.filterMap(x -> lowerBool(x, idToIndex, marshalling, indexInfo, (Nil, MutDisjointSets.empty(rc))), test), lowerStmtRec(body))

            // For all of these we have to do the operation for all indexes created for the relations.
            case ARamStmt.Merge(newRel, deltaRel) =>
                let (constructed, placements) = indexInfo;
                let newRelPos = unwrap(Map.get((newRel, 0), placements));
                let lub = match toDenotation(newRel) {
                    case Denotation.Relational => _ -> _ -> bug!("Using lattice logic on normal relation")
                    case Denotation.Latticenal(_, _, lub, _) => lub
                };
                let idToMarshall = fst(marshalling);
                let lowerToMerge = index -> ERamStmt.Merge(newRelPos, index, lub, relSymToDenId(deltaRel, idToMarshall));
                let deltaRels = Vector.length(unwrap(Map.get(deltaRel, constructed)));
                let merges = Vector.map(index -> lowerToMerge(unwrap(Map.get((deltaRel, index), placements))), Vector.range(0, deltaRels));
                if(Vector.length(merges) == 1) {
                    Vector.get(0, merges)
                } else {
                    ERamStmt.Par(merges)
                }
            case ARamStmt.Swap(newRel, deltaRel) => 
                let (constructed, placements) = indexInfo;
                let newRelPos = unwrap(Map.get((newRel, 0), placements));
                let lub = match toDenotation(newRel) {
                    case Denotation.Relational => _ -> _ -> bug!("Using lattice logic on normal relation")
                    case Denotation.Latticenal(_, _, lub, _) => lub
                };
                let idToMarshall = fst(marshalling);
                let lowerToMerge = index -> ERamStmt.Merge(newRelPos, index, lub, relSymToDenId(deltaRel, idToMarshall));
                let deltaRels = Vector.length(unwrap(Map.get(deltaRel, constructed)));
                let swapPart = ERamStmt.Swap(newRelPos, unwrap(Map.get((deltaRel, 0), placements)));
                if (deltaRels == 1) {
                    swapPart
                } else {
                    let merges = 
                        Vector.range(1, deltaRels) |>
                        Vector.map(index -> lowerToMerge(unwrap(Map.get((deltaRel, index), placements))));
                    if(Vector.length(merges) == 1) {
                        ERamStmt.Seq(Vector#{Vector.get(0, merges), swapPart})
                    } else {
                        ERamStmt.Seq(Vector#{ERamStmt.Par(merges), swapPart})
                    }
                }
            case ARamStmt.Purge(s) =>
                let (constructed, placements) = indexInfo;
                let indexNums = Vector.range(0, Vector.length(unwrap(Map.get(s, constructed))));
                let purges = Vector.map(i -> ERamStmt.Purge(unwrap(Map.get((s, i), placements))), indexNums);
                if (Vector.length(purges) == 1) {
                    Vector.get(0, purges)
                } else {
                    ERamStmt.Seq(purges)
                }
            case ARamStmt.Comment(s) => ERamStmt.Comment(s)
            case ARamStmt.EstimateJoinSize(_, relIndex, estWriteTo, attr) => 
                ERamStmt.EstimateJoinSize(relIndex, estWriteTo, attr)
    }

    def addRowVarToMeetWithMap(rv: RowVar, meetWithMap: MeetWithMap[r]): MeetWithMap[r] \ r =
        let (prevRowVars, meetDisjointSet) = meetWithMap;
        MutDisjointSets.makeSet(rv, meetDisjointSet);
        (rv :: prevRowVars, meetDisjointSet)

    def computeMeetWithPos(
        rv: RowVar,
        relSym: RelSym,
        idToIndex: IdToIndex[r],
        meetWithMap: MeetWithMap[r]
    ): Int32 \ r =
        match toDenotation(relSym) {
            case Denotation.Relational => -1
            case Denotation.Latticenal(_, _, _, _) => computeLatticeMeetPos(rv, idToIndex, meetWithMap)
        }
    
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

    def lowerOp(
        op: ARelOp,
        rc: Region[r],
        idToIndex: IdToIndex[r],
        marshalling: (RamIdToMarshal, Boxing[r]), 
        writeTo: (WriteTos[r], ConstWrites[r]), 
        indexInfo: IndexInformation,
        meetWithMap: MeetWithMap[r]
    ): ERelOp \ r =
        let lowerOpRec: ARelOp -> ERelOp \ r = x -> lowerOp(x, rc, idToIndex, marshalling, writeTo, indexInfo, meetWithMap);
        let lowerOpRecNewRowVar = rv -> x -> lowerOp(x, rc, idToIndex, marshalling, writeTo, indexInfo, addRowVarToMeetWithMap(rv, meetWithMap));
        match op {
            case ASearch(rv, relSym, body) =>
                let den = toDenotation(relSym);
                let loweredBody = lowerOpRecNewRowVar(rv, body);
                let thisWriteTo = getWriteTo(rv, writeTo);
                let placements = snd(indexInfo);
                let relPos = unwrap(Map.get((relSym, 0), placements));
                let oldPos = computeMeetWithPos(rv, relSym, idToIndex, meetWithMap);
                match den {
                    case Denotation.Relational => (
                        ESearch(lowerRowVar(rv, idToIndex), relPos, thisWriteTo, loweredBody)
                    )
                    case Denotation.Latticenal(_, _, _, _) =>
                        ESearchLattice(lowerRowVar(rv, idToIndex), relPos, oldPos, den, thisWriteTo, loweredBody)
                }
            case AFunctional(rv, f, terms, body, arity) =>
                let RowVar.Named(id) = rv;
                let (idToMarshall, _) = marshalling;
                let idToMarhsalled = id_ -> unwrap(Map.get(id_, idToMarshall));
                let loweredBody = lowerOpRec(body);
                let thisWriteTo = getWriteTo(rv, writeTo);
                let to = Vector.map(i -> idToMarhsalled(RamId.TuplePos(id, i)), Vector.range(0, arity));
                EFunctional(
                    lowerRowVar(rv, idToIndex), f, 
                    Vector.map(x -> lowerTerm(x, idToIndex, marshalling, meetWithMap), terms), 
                    thisWriteTo, loweredBody, to
                )
            case AProject(terms, s) => 
                let idToMarshall = fst(marshalling);
                let (_, placements) = indexInfo;
                let newRelPos = unwrap(Map.get((s, 0), placements));
                let (bot, leq, lub) = match toDenotation(s) {
                    case Denotation.Relational => (
                        BoxedBool(false),
                        _ -> _ -> bug!("Using lattice logic on normal relation"),
                        _ -> _ -> bug!("Using lattice logic on normal relation")
                    )
                    case Denotation.Latticenal(bot, leq, lub, _) => (bot, leq, lub)
                };
                let loweredTerms = Vector.map(x -> lowerTerm(x, idToIndex, marshalling, meetWithMap), terms);
                EProject(
                    loweredTerms, newRelPos, bot, leq, lub, relSymToDenId(s, idToMarshall)
                )
            case AIf(boolExps, body) =>
                EIf(Vector.filterMap(x -> lowerBool(x, idToIndex, marshalling, indexInfo, meetWithMap), boolExps), lowerOpRec(body))
            case AQuery(rv, relSym, tests, index, body) =>
                let den = toDenotation(relSym);
                let (idToMarshall, marshall) = marshalling;
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
                        let unboxed = Boxing.unboxWith(val, unwrap(Map.get(id, idToMarshall)), marshall);
                        addConstWriteTo(unboxed, rv, i, idToIndex, writeTo, rc);
                        None
                    case ABoolExp.Eq(ARamTerm.Lit(val, id), ARamTerm.RowLoad(rv1, i, _)) if rv == rv1 =>
                        let unboxed = Boxing.unboxWith(val, unwrap(Map.get(id, idToMarshall)), marshall);
                        addConstWriteTo(unboxed, rv, i, idToIndex, writeTo, rc);
                        None
                    case _ => Some(x)
                }, tests);
                if(Vector.length(otherBools) != 0)
                    unchecked_cast(bug!("Bug in Fixpoint.Lowering: Bools except equality in query") as _ \ r) 
                else ();

                match den {
                    case Denotation.Relational => (
                        EQuery(lowerRowVar(rv, idToIndex), index, thisWriteTo, loweredBody)
                    )
                    case Denotation.Latticenal(_, _, _, _) =>
                        EQueryLattice(lowerRowVar(rv, idToIndex), index, oldPos, den, thisWriteTo, loweredBody)
                }
        }

    def lowerTerm(
        term: ARamTerm, 
        idToIndex: IdToIndex[r], 
        marshalling: (RamIdToMarshal, Boxing[r]), 
        meetWithMap: MeetWithMap[r]
    ): ERamTerm \ r =
        let lowerT = x -> lowerTerm(x, idToIndex, marshalling, meetWithMap);
        let (idToMarshall, marshall) = marshalling;
        let termToM = t -> unwrap(Map.get(getTermRamId(t), idToMarshall));
        match term {
            case ALit(val, id) => ELit(Boxing.unboxWith(val, unwrap(Map.get(id, idToMarshall)), marshall), val)
            case ARowLoad(rv, index, RelSym.Symbol(_, arity, den)) => 
                match den {
                    case Denotation.Relational => ELoadFromTuple(UniqueInts.getIndex(rv, idToIndex), index, termToM(term))
                    case Denotation.Latticenal(_, _, _, _) => 
                        if(index < arity - 1) {
                            ELoadFromTuple(UniqueInts.getIndex(rv, idToIndex), index, termToM(term))
                        } else {
                            ELoadLatVar(UniqueInts.getIndex(rv, idToIndex), termToM(term))
                        }
                }
            case AMeet(_, _, _, _) =>
                let representingRowVar = lowerMeet(term, idToIndex, meetWithMap);
                let pos = computeLatticeMeetPos(representingRowVar, idToIndex, meetWithMap);
                ELoadLatVar(pos, termToM(term))
            case AApp0(f, _)                       => EApp0(f, termToM(term))
            case AApp1(f, t1, _)                   => EApp1(f, lowerT(t1), termToM(term))
            case AApp2(f, t1, t2, _)               => EApp2(f, lowerT(t1), lowerT(t2), termToM(term))
            case AApp3(f, t1, t2, t3, _)           => EApp3(f, lowerT(t1), lowerT(t2), lowerT(t3), termToM(term))
            case AApp4(f, t1, t2, t3, t4, _)       => EApp4(f, lowerT(t1), lowerT(t2), lowerT(t3), lowerT(t4), termToM(term))
            case AApp5(f, t1, t2, t3, t4, t5, _)   => EApp5(f, lowerT(t1), lowerT(t2), lowerT(t3), lowerT(t4), lowerT(t5), termToM(term))
        }

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
            MutDisjointSets.makeSet(rv, meetDisjointSet);
            MutDisjointSets.union(rv, prevRv, meetDisjointSet);
            rv
        case _ => unreachable!()
    }
    
    // NotBot is removed as it is handled separately by the interpreter.
    def lowerBool(bool: ABoolExp, idToIndex: IdToIndex[r], marshalling: (RamIdToMarshal, Boxing[r]), indexInfo: IndexInformation, meetWithMap: MeetWithMap[r]): Option[EBoolExp] \ r = 
        let idToMarshall = fst(marshalling);
        let lowerT = x -> lowerTerm(x, idToIndex, marshalling, meetWithMap);
        match bool {
            case ANot(s) => 
                lowerBool(s, idToIndex, marshalling, indexInfo, meetWithMap) |>
                Option.map(ENot)
            case AIsEmpty(s) =>
                let (_, placements) = indexInfo;
                let sPos = unwrap(Map.get((s, 0), placements));
                Some(EIsEmpty(sPos))
            case ANotMemberOf(terms, s) => 
                let (_, placements) = indexInfo;
                let leq = match toDenotation(s) {
                    case Denotation.Relational => _ -> _ -> bug!("Using lattice logic on normal relation")
                    case Denotation.Latticenal(_, leq, _, _) => leq
                };
                let sPos = unwrap(Map.get((s, 0), placements));
                Some(ENotMemberOf(Vector.map(lowerT, terms), sPos, leq, relSymToDenId(s, idToMarshall)))
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
            case AGuard0(f) => Some(EGuard0(f))
            case AGuard1(f, term1) => Some(EGuard1(f, lowerT(term1)))
            case AGuard2(f, term1, term2) => Some(EGuard2(f, lowerT(term1), lowerT(term2)))
            case AGuard3(f, term1, term2, term3) => Some(EGuard3(f, lowerT(term1), lowerT(term2), lowerT(term3)))
            case AGuard4(f, term1, term2, term3, term4) => Some(EGuard4(f, lowerT(term1), lowerT(term2), lowerT(term3), lowerT(term4)))
            case AGuard5(f, term1, term2, term3, term4, term5) => Some(EGuard5(f, lowerT(term1), lowerT(term2), lowerT(term3), lowerT(term4), lowerT(term5)))
        }

    def lowerRowVar(rowVar: RowVar, idToIndex: IdToIndex[r]): Int32 \ r = 
        UniqueInts.getIndex(rowVar, idToIndex)

    def getWriteTo(rowVar: RowVar, writeTo: (WriteTos[r], ConstWrites[r])): Vector[WriteTuple] \ r = 
        let varWrite = fst(writeTo);
        List.toVector(Option.getWithDefault(Nil, MutMap.get(rowVar, varWrite)))

    def addWriteTo(fromTuple: RowVar, fromIndex: Int32, toTuple: RowVar, toIndex: Int32, idToIndex: IdToIndex[r], writeTo: (WriteTos[r], ConstWrites[r])): Unit \ r =
        let varWrite = fst(writeTo);
        let written = (fromIndex, lowerRowVar(toTuple, idToIndex), toIndex);
        MutMap.putWith(_ -> old -> written :: old, fromTuple, written :: Nil, varWrite)

    def addConstWriteTo(value: Int64, toTuple: RowVar, toIndex: Int32, idToIndex: IdToIndex[r], writeTo: (WriteTos[r], ConstWrites[r]), rc: Region[r]): Unit \ r =
        let constWrite = snd(writeTo);
        let toTuplePos = lowerRowVar(toTuple, idToIndex);
        MutMap.getOrElsePut(toTuplePos, MutMap.empty(rc), constWrite) |>
            MutMap.putWith(v1 -> v2 -> 
            if (v1 != v2) 
                bug!("Bug in Fixpoint.Lowering: Different value for position. Should have been removed by Hoisting")
            else v1, toIndex, value)

    def unwrap(opt: Option[a]): a = match opt {
        case None => bug!("Unwrap None in Lowering")
        case Some(v) => v
    }

}
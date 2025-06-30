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

mod Fixpoint3.JoinProfiler {
    use Fixpoint3.Ast.ExecutableRam.{RamProgram => ERamProgram, RamStmt => ERamStmt}
    use Fixpoint3.Ast.Ram.{RamProgram, RamStmt, RelOp, RelSym, RowVar, Search}
    use Fixpoint3.Counter
    use Fixpoint3.Util.{collectRowVarRelSymOp, getOrCrash, unifyEqualitiesOp}
    use Fixpoint3.Solver

    ///
    /// Contains estimates on relation sizes and the expected number of tuples resulting
    /// from a particular join as a result of program execution. The estimates are generated
    /// by `EstimateJoinSize` statements. Since they can appear in loops, we collect
    /// estimates for each loop iteration.
    ///
    /// The first map maps relations represented by `RowVar`'s to their estimated size in
    /// each iteration.
    ///
    /// The second map maps a particular join to the expected number of resulting tuples.
    /// This is done by projecting the tuples in the relation to the join attributes and
    /// counting the number of duplicates.
    ///
    @Internal
    pub enum JoinProfile {
        case Profile(
            Map[RowVar, Vector[Int64]],
            Map[(RowVar, Vector[Int32]), Vector[Int64]]
        )
    }

    instance ToString[JoinProfile] {
        pub def toString(x: JoinProfile): String = match x {
            case JoinProfile.Profile(sizePerIter, expectedJoinSize) =>
                let sizePerIterStr = sizePerIter |>
                    Map.joinWith(k -> v -> "    ${k} => ${v}", "\n");
                let expectedJoinSizeStr = expectedJoinSize |>
                    Map.joinWith(k -> v -> "    ${k} => ${v}", "\n");
                "JoinProfile(\n  ${sizePerIterStr}\n  ${expectedJoinSizeStr}\n)"
        }
    }

    // Type alias for non-recursive and recursive rules, respectively.
    type alias Rules = (List[RelOp], List[RelOp])

    ///
    /// Returns a profile on `program` by running `program` instrumented with
    /// `EstimateJoinSize` statements. Additionally, returns a map used to infer which
    /// `RowVar`'s bind which attributes in another `RowVar`.
    ///
    @Internal
    pub def profileProgram(program: RamProgram): (JoinProfile, Map[RowVar, Map[RowVar, Set[Int32]]]) = region rc {
        let (instrumentedProgram, attributeMap, rowVarToPos) = instrumentProgram(rc, program);
        let vec = instrumentedProgram |>
            Fixpoint3.Debugging.notifyPreLowering("Join Profiler") |>
            Solver.runJoinProfiler;
        let joinToSizeDup = Map.map(i -> Vector.get(i, vec), rowVarToPos);
        let sizePerIter = joinToSizeDup |>
            Map.foldLeftWithKey(acc -> k1 -> v1 -> {
                let k2 = fst(k1);
                let v2 = Vector.map(fst >> Int32.toInt64, v1);
                Map.insert(k2, v2, acc)
            }, Map#{});
        let expectedJoinSize = joinToSizeDup |>
            Map.map(
                Vector.map(match (size, dup) ->
                    Int32.toInt64(size) / (Int32.toInt64(size) - Int32.toInt64(dup)))
            );
        let profile = JoinProfile.Profile(sizePerIter, expectedJoinSize);
        (profile, attributeMap)
    }

    ///
    /// Adds join attributes to `attributeMap`. Conceptually, this is similar to collecting
    /// the bound variables in the Datalog rule corresponding to the `RelOp` `rule`.
    ///
    /// `vars` is the list of all `RowVar`'s and their `RelSym`'s in `op`.
    ///
    /// `attributeMap` represents a SIP-graph. Given a `RowVar` `rv1`, a lookup returns another
    /// map which, given a different `RowVar` `rv2`, returns the join attributes indexed by
    /// their position in the corresponding relation for `rv1`. Conceptually, this gives us
    /// the attributes that `rv1` binds in `rv2`,
    ///
    def collectBoundVars(
        rc: Region[r],
        vars: List[(RowVar, RelSym)],
        attributeMap: MutMap[RowVar, MutMap[RowVar, MutSet[Int32, r], r], r],
        rule: RelOp
    ): Map[(RowVar, RelSym), List[Vector[Int32]]] \ r =
        let equalities =  MutDisjointSets.empty(rc);
        let constEqualities =  MutMap.empty(rc);
        unifyEqualitiesOp(equalities, constEqualities, rule);
        // equivalentMap is basically a SIP-graph.
        let equivalentMap = MutMap.empty(rc);
        vars |> List.forEach(match (rv, RelSym.Symbol(_, arity, _)) -> {
            Vector.range(0, arity) |>
            Vector.forEach(i ->
                let load = (rv, i);
                let rep = getOrCrash(MutDisjointSets.find(load, equalities));
                MutMap.putWith(_ -> oldVal -> load :: oldVal, rep, load :: Nil, equivalentMap)
            )
        });
        let attributesForRelSym = MutMap.empty(rc);
        vars |> List.forEach(match (rvi, relSym) -> {
            let mapForI = MutMap.getOrElsePut(rvi, MutMap.empty(rc), attributeMap);
            let boundForI = MutSet.empty(rc);
            vars |> List.filter(match (rvj, _) -> rvj != rvi) |>
            List.forEach(aj -> {
                let (rvj, RelSym.Symbol(_, arity, _)) = aj;
                let boundByJ = MutSet.empty(rc);
                Vector.range(0, arity) |>
                Vector.forEach(i -> {
                    let rep = getOrCrash(MutDisjointSets.find((rvj, i), equalities));
                    match MutMap.get(rep, equivalentMap) {
                        case None => ()
                        case Some(v) =>
                            v |> List.forEach(match (rv, index) ->
                                if(rv == rvi) MutSet.add(index, boundByJ)
                                else ()
                            )
                    }
                });
                MutMap.put(rvj, boundByJ, mapForI);
                MutSet.add(MutSet.toList(boundByJ), boundForI)
            });
            let result = MutSet.empty(rc);
            updateWithPowersetOf(List.toVector(MutSet.toList(boundForI)), result);
            let collectedSoFar = MutMap.getOrElsePut((rvi, relSym), MutSet.empty(rc), attributesForRelSym);
            MutSet.addAll(MutSet.toSet(result), collectedSoFar)
        });
        attributesForRelSym |> MutMap.toMap |> Map.map(MutSet.toList >> List.map(List.toVector))

    ///
    /// Updates `setForJ` with all elements in the powerset of `values`.
    ///
    def updateWithPowersetOf(values: Vector[List[Int32]], setForJ: MutSet[List[Int32], r]): Unit \ r =
        def loop(index, val) =
            if(Vector.size(values) == index) {
                let toInsert = Set.foldRight(boundVar -> acc -> boundVar :: acc, Nil, val);
                MutSet.add(toInsert, setForJ)
            } else {
                Vector.range(index + 1, Vector.length(values) + 1) |>
                Vector.forEach(nextIndex -> {
                    let newVal = List.foldLeft(acc -> boundVar ->
                        Set.insert(boundVar, acc), val, Vector.get(index, values)
                    );
                    loop(nextIndex, newVal)
                })
            };
        Vector.range(0, Vector.length(values) + 1) |>
        Vector.forEach(i -> loop(i, Set.empty()))

    ///
    /// Populates `collectFor` with the estimates given by relations and join attributes in
    /// `toEstimate`. A `Search` is used to represent the join attributes, which is
    /// coincidentally also used later to construct an index conforming with that search.
    ///
    def estimateToCollect(
        rc: Region[r],
        toEstimate: Map[(RowVar, RelSym), List[Search]],
        collectFor: MutMap[(RelSym, Search), MutSet[RowVar, r], r]
    ): Unit \ r =
        Map.forEach(match (rowVar, relSym) -> val -> {
            val |> List.forEach(search ->
                MutMap.getOrElsePut((relSym, search), MutSet.empty(rc), collectFor) |>
                MutSet.add(rowVar)
            )
        }, toEstimate)

    ///
    /// Create appropriate `EstimateJoinSize` statements based on write-positions in
    /// `rowVarToPos` and estimate information in `collectFor`.
    ///
    def collectedToEstimateStmts(
        counter: Counter[r],
        rowVarToPos: MutMap[(RowVar, Search), Int32, r],
        collectFor: MutMap[(RelSym, Search), MutSet[RowVar, r], r]
    ): List[RamStmt] \ r =
        collectFor |> MutMap.foldLeftWithKey(acc -> match (relSym, search) -> rowVars -> {
            let savePos = Counter.getAndIncrement(counter);
            MutSet.forEach(rowVar -> MutMap.put((rowVar, search), savePos, rowVarToPos), rowVars);
            RamStmt.EstimateJoinSize(relSym, -1, savePos, search) :: acc
        }, Nil)

    ///
    /// Returns a `Seq` statement containing `EstimateJoinSize` statements created based
    /// on `rowVarToPos` and `collectFor` and places it immediately before `stmt`.
    ///
    def combineStmtWithEstimate(
        counter: Counter[r],
        rowVarToPos: MutMap[(RowVar, Search), Int32, r],
        collectFor: MutMap[(RelSym, Search), MutSet[RowVar, r], r],
        stmt: RamStmt
    ): RamStmt \ r =
        let estimateStmts = collectedToEstimateStmts(counter, rowVarToPos, collectFor);
        if(List.isEmpty(estimateStmts)) stmt
        else RamStmt.Seq(Vector#{RamStmt.Par(List.toVector(estimateStmts)), stmt})

    ///
    /// Instruments `program` with `EstimateJoinSize` statements.
    ///
    def instrumentProgram(
        rc: Region[r],
        program: RamProgram
    ): (RamProgram, Map[RowVar, Map[RowVar, Set[Int32]]], Map[(RowVar, Vector[Int32]), Int32]) \ r = {
        let counter = Counter.fresh(rc);
        let rowVarToPos = MutMap.empty(rc);
        let attributeMap = MutMap.empty(rc);
        def collectUntilBody(collectFor, s) = match s {
            case RamStmt.Insert(op) =>
                let toEstimate = collectBoundVars(rc, collectRowVarRelSymOp(op), attributeMap, op);
                estimateToCollect(rc, toEstimate, collectFor)
            case RamStmt.MergeInto(_, _) => ()
            case RamStmt.Swap(_, _) => ()
            case RamStmt.Purge(_) => ()
            case RamStmt.Seq(stmts) => Vector.forEach(collectUntilBody(collectFor), stmts)
            case RamStmt.Par(stmts) => Vector.forEach(collectUntilBody(collectFor), stmts)
            case RamStmt.Until(_, _) => bug!("bug in Fixpoint.JoinProfiler: Nested until statements should not exist")
            case RamStmt.Comment(_) => ()
            case RamStmt.EstimateJoinSize(_, _, _, _) =>
                bug!("bug in Fixpoint.JoinProfiler: EstimateJoinSize should not exist at this point")
        };

        // Note that estimation can be done in parallel with the expressions using them.
        // Even for statements outside the until blocks they only write to the new versions.
        // This means that we require the compiler to not change any indexes used in a given
        // par statement. This might seem self evident, but it would not necessarily be
        // unsafe to do so from a compilers perspective.
        def instrumentStmt(s: RamStmt): RamStmt = match s {
            case RamStmt.Insert(op) =>
                // We estimate the size immediately before the insert-loop
                let toEstimate = collectBoundVars(rc, collectRowVarRelSymOp(op), attributeMap, op);
                // Collect information for RelSym with search and save it for RowVar.
                let collectFor = MutMap.empty(rc);
                estimateToCollect(rc, toEstimate, collectFor);
                combineStmtWithEstimate(counter, rowVarToPos, collectFor, s)
            case RamStmt.MergeInto(_, _) => s
            case RamStmt.Swap(_, _) => s
            case RamStmt.Purge(_) => s
            case RamStmt.Seq(stmts) => Vector.map(instrumentStmt, stmts) |> RamStmt.Seq
            case RamStmt.Par(stmts) => Vector.map(instrumentStmt, stmts) |> RamStmt.Par
            case RamStmt.Until(conditions, body) =>
                let collectFor = MutMap.empty(rc);
                collectUntilBody(collectFor, body);
                let newBody = combineStmtWithEstimate(counter, rowVarToPos, collectFor, body);
                RamStmt.Until(conditions, newBody)
            case RamStmt.Comment(_) => s
            case RamStmt.EstimateJoinSize(_, _, _, _) => bug!("bug in Fixpoint.JoinProfiler: EstimateJoinSize should not exist at this point")
        };
        let RamProgram.Program(mainStmt, facts, predState, indexes) = program;
        let instrumentedStmt = instrumentStmt(mainStmt);
        let collectedAttributeMap = attributeMap |>
            MutMap.map(rc, innerMap -> innerMap |> MutMap.map(rc, MutSet.toSet) |> MutMap.toMap) |>
            MutMap.toMap;
        let instrumentedProgram = RamProgram.Program(instrumentedStmt, facts, predState, indexes);
        (instrumentedProgram, collectedAttributeMap, rowVarToPos |> MutMap.toMap)
    }

    ///
    /// Returns the number of distinct `EstimateJoinSize` statements in `program`. This is
    /// used by the interpreter to create an array which stores the results of those
    /// statements.
    ///
    @Internal
    pub def getJoinEstimateNum(program: ERamProgram): Int32 =
        let ERamProgram.Program(stmt, _, _, _, _, _) = program;
        getJoinEstimateMaxPos(stmt) + 1

    ///
    /// Returns the highest write-position of `EstimateJoinSize` statements in `s`.
    ///
    def getJoinEstimateMaxPos(s: ERamStmt): Int32 = match s {
        case ERamStmt.Insert(_) => -1
        case ERamStmt.MergeInto(_, _, _) => -1
        case ERamStmt.Swap(_, _) => -1
        case ERamStmt.Purge(_) => -1
        case ERamStmt.Seq(stmts) => Vector.map(getJoinEstimateMaxPos, stmts) |> Vector.foldLeft(Int32.max, -1)
        case ERamStmt.Par(stmts) => Vector.map(getJoinEstimateMaxPos, stmts) |> Vector.foldLeft(Int32.max, -1)
        case ERamStmt.Until(_, stmt) => getJoinEstimateMaxPos(stmt)
        case ERamStmt.Comment(_) => -1
        case ERamStmt.EstimateJoinSize(_, writeTo, _) => writeTo
    }

}

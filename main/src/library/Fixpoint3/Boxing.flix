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
/// The purpose of this file is to represent every `Boxed` value with an `Int64`.
///
/// For all primitive values the `Int64` representation is simply the raw bits,
/// possibly extended to form an `Int64`.
///
/// For `BoxedObject` some `Int64` value is computed to represent it with the
/// promise that equality is maintained with respect to `Order` on the `BoxedObject`.
///
/// Consider the following program:
/// ```
/// A(x, y) :- B(x), C(y).
/// ```
/// We know that A[0] and B[0] should share the same value, and if we project the
/// representing `Int64` from `B` to `A` then we should be able to compute the
/// `Boxed` value. We therefore unify the two, effectively declaring that the types
/// are the same. The same is done for `A[1]` and `C[0]`. On the other hand unifying
/// `A[0]` and `A[1]` is not secure, as it could mix two unrelated types.
///
/// The `initialize` method computes a unification and transforms all `Boxed` facts
/// to `Int64` and returns a `BoxingType.Boxing`.
///
/// See `BoxingType.flix` for data types and an example of how `BoxedObject`s are treated.
///
/// The module operates on `Ram.Program`, so it uses, `Eq(x, y)`, `Meet(x, y)`, `Project(_)`
/// and so on to compute the equality constraints.
///
mod Fixpoint3.Boxing {
    import java.lang.{Float, Double, Object, Character, Runtime}
    use Fixpoint3.Ast.Datalog.Datalog
    use Fixpoint3.Ast.ExecutableRam.{Facts => EFacts}
    use Fixpoint3.Ast.Ram
    use Fixpoint3.Ast.Ram.{arityOf, BoolExp, RamId, RamProgram, RamStmt, RelSym, RamTerm, RelOp, toDenotation}
    use Fixpoint3.Ast.Shared.{PredSym, isRelational, Denotation}
    use Fixpoint3.Boxable
    use Fixpoint3.Boxed
    use Fixpoint3.Boxed.{BoxedBool, BoxedChar, BoxedFloat32, BoxedFloat64, BoxedInt8, BoxedInt16, BoxedInt32, BoxedInt64, BoxedObject}
    use Fixpoint3.BoxingType.{Boxing, getType, RamIdToPos, setType, Types, TypeInfo, UnifiedTypePos}
    use Fixpoint3.Options.usedArity
    use Fixpoint3.Predicate.relSymsOfProgram
    use Fixpoint3.ReadWriteLock
    use Fixpoint3.Util.getOrCrash

    ///
    /// Returns a `(boxing, facts, idToBoxing)` where `Boxing` is information as described above,
    /// `facts` are the EDB of `program` as `Int64`, with respect to `boxing` and `idToBoxing`
    /// described where the boxing information associated with a specific `RamId` is placed,
    /// aka the `UnifiedTypePos`.
    ///
    @Internal
    pub def initialize(rc: Region[r], withProv: Bool, program: RamProgram): (Boxing[r], EFacts[r], RamIdToPos) \ r =
        let mapping = Equality.computeMapping(program, withProv);
        let boxingInfo = initializeInternal(rc, program, mapping, withProv);
        let facts = unsafely IO run initializeFacts(rc, program, boxingInfo, mapping, withProv);
        (boxingInfo, facts, mapping)

    ///
    /// Returns `(facts, boxing)` where `facts` consists of the `Int64` representatives of
    /// the facts in `program`, with respect to `boxing`. Note that the old `boxingInfo` is
    /// invalid for the returned `facts`.
    ///
    def initializeFacts(rc1: Region[r], program: RamProgram, boxingInfo: Boxing[r], mapping: RamIdToPos, withProv: Bool): EFacts[r] \ r + IO =
        let RamProgram.Program(_, facts, _, (indexes, _)) = program;
        let (f1, f2) = facts;
        let newFacts = MutMap.empty(rc1);
        foreach ((relSym, relFacts) <- f1) {
            // Get some search. This means that we have to create one less index in the interpreter.
            let search = getOrCrash(Map.get(relSym, indexes)) |> Vector.get(0);
            let newTree = BPlusTree.emptyWithArityAndSearch(rc1, usedArity(), search);
            mapAllFacts(relSym, relFacts, boxingInfo, mapping, withProv, newTree);
            MutMap.put(relSym, newTree, newFacts)
        };
        foreach ((relSym, relFacts) <- f2) {
            let newTree = MutMap.getOrElsePut(relSym, BPlusTree.empty(rc1), newFacts);
            mapAllFacts(relSym, relFacts, boxingInfo, mapping, withProv, newTree)
        };
        newFacts |> MutMap.toMap

    def mapAllFacts(
        relSym: RelSym,
        facts: BPlusTree[Vector[Boxed], Boxed, Static],
        boxingInfo: Boxing[r],
        mapping: RamIdToPos,
        withProv: Bool,
        newTree: BPlusTree[Vector[Int64], Boxed, r]
    ): Unit \ r + IO = unchecked_cast({
        let relSymId = Ram.toId(relSym);
        let nonExtendedPositions = Vector.range(0, arityOf(relSym)) |>
            Vector.map(i -> getOrCrash(Map.get(RamId.RelPos(relSymId, i), mapping)));
        let positions = if (not withProv) {
            nonExtendedPositions
        } else {
            Vector.append(nonExtendedPositions, Vector#{getOrCrash(Map.get(RamId.Id(-1), mapping)), getOrCrash(Map.get(RamId.Id(-1), mapping))})
        };
        facts |>
            BPlusTree.parForEach(tuple -> lat -> {
                // `facts` and `newTree` must have same the same region. Cast to achieve this.
                unchecked_cast(({
                    let savedVec = tuple |>
                        Vector.mapWithIndex(index -> boxedVal ->
                            let unifiedPos = Vector.get(index, positions);
                            unboxWith(boxedVal, unifiedPos, boxingInfo)
                        );
                    BPlusTree.put(savedVec, lat, newTree)
                }: _ \ r) as _ \ IO)}
            )
    // This is to signal that we indeed modify the region `r`.
    } as _ \ IO + r)

    ///
    /// Construct a `Boxing` from `program` given a map from `RamId` to `UnifiedTypePos`.
    ///
    def initializeInternal(rc: Region[r], program: RamProgram, map: RamIdToPos, withProv: Bool): Boxing[r] \ r =
        let max = 1 + snd(Option.getWithDefault((RamId.Id(-1), -1), Map.maximumValue(map)));
        let intToBox = Vector.init(_ -> MutList.empty(rc), max);
        let boxToInt = Vector.init(_ -> BPlusTree.empty(rc), max);
        let locks = Vector.init(_ -> ReadWriteLock.mkLock(rc), max);
        let posToIndex = Array.init(rc, _ -> Types.TyUnknown, max);
        let info = (intToBox, boxToInt, posToIndex, locks);
        let relSyms = relSymsOfProgram(program);
        if (withProv) {
            setType(Types.TyInt64, getOrCrash(Map.get(RamId.Id(-1), map)), posToIndex)
        } else {
            ()
        };
        // Add bot first to make sure it corresponds to 0.
        foreach (x <- relSyms) {
            match x {
                case RelSym.Symbol(PredSym.PredSym(_, index), arity, Denotation.Latticenal(bot, _, _, _)) =>
                    unboxWith(bot, getOrCrash(Map.get(RamId.RelPos(index, arity - 1), map)), info); ()
                case _ => ()
            }
        };
        info

    ///
    /// Returns the `Boxed` value associated with `value` for the type information
    /// at `index` in `info`.
    ///
    /// Thread safe.
    ///
    def deMarshalObject(value: Int64, index: UnifiedTypePos, info: Boxing[r]): Boxed \ r = {
        let (objectList, _, _, locks) = info;
        let lock = Vector.get(index, locks);
        ReadWriteLock.readLock(lock);
        let res = getOrCrash(MutList.nth(getOrCrash(Int64.tryToInt32(value)), Vector.get(index, objectList)));
        ReadWriteLock.unlockRead(lock);
        res
    }

    ///
    /// Returns the `Int64` value associated with `box` for the type information
    /// at `index` in `info`.
    ///
    /// Thread safe.
    ///
    def marshallObject(box: Boxed, index: UnifiedTypePos, info: Boxing[r]): Int64 \ r = {
        let (objectLists, vecObjectMap, _, locks) = info;
        let objectList = Vector.get(index, objectLists);
        let objectMap = Vector.get(index, vecObjectMap);
        let lock = Vector.get(index, locks);
        BPlusTree.computeIfAbsent(() -> {
            ReadWriteLock.writeLock(lock);
            let id = getNextIndex(objectList);
            MutList.insert(box, id, objectList);
            ReadWriteLock.unlockWrite(lock);
            Int32.toInt64(id)
        }, box, objectMap)
    }

    ///
    /// Return the position that the next value inserted in `list` will get.
    ///
    def getNextIndex(list: MutList[Boxed, r]): Int32 \ r = MutList.size(list)

    ///
    /// Unbox `v` given the type information stored at `typeInfo[index]` in `info`.
    /// Returns `v` unboxed.
    ///
    @Internal
    pub def unboxWith(v: Boxed, index: UnifiedTypePos, info: Boxing[r]): Int64 \ r =
        let (_, _, symInfo, _) = info;
        match v {
            case BoxedBool(x) =>
                setType(Types.TyBool, index, symInfo);
                if (x) 1i64 else 0i64
            case BoxedChar(x) =>
                setType(Types.TyChar, index, symInfo);
                Int32.toInt64(Char.toBmpCodePoint(x))
            case BoxedInt8(x) =>
                setType(Types.TyInt8, index, symInfo);
                Int8.toInt64(x)
            case BoxedInt16(x) =>
                setType(Types.TyInt16, index, symInfo);
                Int16.toInt64(x)
            case BoxedInt32(x) =>
                setType(Types.TyInt32, index, symInfo);
                Int32.toInt64(x)
            case BoxedInt64(x) =>
                setType(Types.TyInt64, index, symInfo);
                x
            case BoxedFloat32(x) =>
                setType(Types.TyFloat32, index, symInfo); Int32.toInt64(Float.floatToRawIntBits(Float32.valueOf(x).floatValue()))
            case BoxedFloat64(x) =>
                setType(Types.TyFloat64, index, symInfo);
                Double.doubleToRawLongBits(Float64.valueOf(x).doubleValue())
            case BoxedObject(_, _) =>
                setType(Types.TyObject, index, symInfo);
                // We remap to low, so this only goes wrong when dealing with more
                // than 2^32 different RelSyms
                marshallObject(v, index, info)
            case Boxed.NoValue => bug!("NoValue should never be in a position to be unboxed unboxed")
        }

    ///
    /// Boxes `v` given its type information at `typeInfo[index]` in `info`.
    /// Returns `v` boxed.
    ///
    @Internal
    pub def boxWith(v: Int64, index: UnifiedTypePos, info: Boxing[r]): Boxed \ r =
        let (_, _, typeInfo, _) = info;
        match getType(index, typeInfo) {
            case Types.TyBool      => Boxed.BoxedBool(not(v == 0i64))
            case Types.TyChar      => BoxedChar(unchecked_cast(Array.get(0, Character.toChars(getOrCrash(Int64.tryToInt32(v)))) as _ \ {}))
            case Types.TyInt8      => BoxedInt8(getOrCrash(Int64.tryToInt8(v)))
            case Types.TyInt16     => BoxedInt16(getOrCrash(Int64.tryToInt16(v)))
            case Types.TyInt32     => BoxedInt32(getOrCrash(Int64.tryToInt32(v)))
            case Types.TyInt64     => BoxedInt64(v)
            case Types.TyFloat32   => BoxedFloat32(Float.intBitsToFloat(getOrCrash(Int64.tryToInt32(v))))
            case Types.TyFloat64   => BoxedFloat64(Double.longBitsToDouble(v))
            case Types.TyObject    => deMarshalObject(v, index, info)
            case Types.TyUnknown   => bug!("Unormalizing value, which has never been normalized")
        }

    ///
    /// Construct a map from `RamId` to `UnifiedTypePos`.
    ///
    /// See the description of the Boxing module.
    ///
    mod Equality {
        use Fixpoint3.Ast.Ram.getTermRamId
        use Fixpoint3.Ast.Ram
        use Fixpoint3.Ast.Ram.{RamId, Predicates, RamStmt, RelOp, RamTerm, BoolExp, RamProgram, RowVar}
        use Fixpoint3.Ast.Ram.{RelSym, RamProgram}
        use Fixpoint3.Ast.Shared.PredSym
        use Fixpoint3.BoxingType.RamIdToPos
        use Fixpoint3.Counter
        use Fixpoint3.Util.getOrCrash
        use Fixpoint3.Predicate.{PredType, idToFullPredId, fullRelSymToType, relSymFromPredType, relSymsOfProgram}

        ///
        /// Returns a map from `RamId` to `UnifiedTypePos` constructed from `program`.
        ///
        @Internal
        pub def computeMapping(program: RamProgram, withProv: Bool): RamIdToPos = region rc {
            // First compute the equivalence relation for `RamId`s in program
            // and afterwards use this to assign unique ints to each
            // equivalence relation.
            let disjointSet = MutDisjointSets.empty(rc);
            let RamProgram.Program(stmt, _, _, _) = program;
            unifyRamIds(program, disjointSet, withProv);
            let relSyms = relSymsOfProgram(program);
            let mutMap = MutMap.empty(rc);
            let counter = Counter.fresh(rc);
            foreach (RelSym.Symbol(PredSym.PredSym(_, id), arity, _) <- relSyms) {
                foreach (i <- Vector.range(0, getProvSafeIdArity(arity, withProv))) {
                    registerRamId(disjointSet, mutMap, counter, RamId.RelPos(id, i))
                }
            };
            // Given the equivalence relation just computed assign unique positions for each
            // equivalence relation.
            computeMappingStmt(disjointSet, mutMap, counter, withProv, stmt);
            if (withProv) {
                registerRamId(disjointSet, mutMap, counter, RamId.Id(-1))
            } else ();
            MutMap.toMap(mutMap)
        }

        ///
        /// Unify all `RamId`s in `program` which can be proven to be equivalent, type-wise.
        ///
        def unifyRamIds(program: RamProgram, set: MutDisjointSets[RamId, r], withProv: Bool): Unit \ r = match program {
            case RamProgram.Program(stmt, _, predicates, _) =>
                let relSyms = relSymsOfProgram(program);
                List.forEach(unifyPredTypes(predicates, set, withProv), relSyms);
                unifyRamIdsStmt(predicates, set, withProv, stmt)
        }

        ///
        /// Unify all `RamId`s in `stmt` which can be proven to be equivalent, type-wise.
        ///
        def unifyRamIdsStmt(predicates: Predicates, set: MutDisjointSets[RamId, r], withProv: Bool, stmt: RamStmt): Unit \ r = match stmt {
            case RamStmt.Insert(body) => unifyRamIdsOp(predicates, set, withProv, body)
            case RamStmt.MergeInto(rel1, rel2) => unifyRelSyms(rel1, rel2, set, withProv)
            case RamStmt.Swap(rel1, rel2) => unifyRelSyms(rel1, rel2, set, withProv)
            case RamStmt.Purge(_) => ()
            case RamStmt.Seq(stmts) => Vector.forEach(unifyRamIdsStmt(predicates, set, withProv), stmts)
            case RamStmt.Par(stmts) => Vector.forEach(unifyRamIdsStmt(predicates, set, withProv), stmts)
            case RamStmt.Until(bools, body) =>
                unifyRamIdsStmt(predicates, set, withProv, body);
                Vector.forEach(unifyRamIdsBool(set), bools)
            case RamStmt.Comment(_) => ()
        }

        ///
        /// Unify all `RamId`s in `op` which can be proven to be equivalent, type-wise.
        ///
        def unifyRamIdsOp(predicates: Predicates, set: MutDisjointSets[RamId, r], withProv: Bool, op: RelOp): Unit \ r = match op {
            case RelOp.Search(rv, RelSym.Symbol(PredSym.PredSym(_, predSym), arity, _), body) =>
                let usedArity = getProvSafeIdArity(arity, withProv);
                foreach (i <- Vector.range(0, usedArity)) {
                    MutDisjointSets.union(RamId.TuplePos(rv, i), RamId.RelPos(predSym, i), set)
                };
                unifyRamIdsOp(predicates, set, withProv, body)
            case RelOp.Query(rv, RelSym.Symbol(PredSym.PredSym(_, predSym), arity, _), bools, _, body) =>
                let usedArity = getProvSafeIdArity(arity, withProv);
                foreach (i <- Vector.range(0, usedArity)) {
                    MutDisjointSets.union(RamId.TuplePos(rv, i), RamId.RelPos(predSym, i), set)
                };
                Vector.forEach(unifyRamIdsBool(set), bools);
                unifyRamIdsOp(predicates, set, withProv, body)
            case RelOp.Functional(RowVar.Named(id), _, inputTerms, body, _) =>
                foreach ((i, curTerm) <- ForEach.withIndex(inputTerms)) {
                    let termID = Ram.getTermRamId(curTerm);
                    MutDisjointSets.union(RamId.InId(id, i), termID, set);
                    unifyRamIdsTerm(set, curTerm)
                };
                unifyRamIdsOp(predicates, set, withProv, body)
            case RelOp.Project(terms, RelSym.Symbol(PredSym.PredSym(_, predSym), _, _), _) =>
                foreach ((i, term) <- ForEach.withIndex(terms)) {
                    unifyRamIdsTerm(set, term);
                    let termID = Ram.getTermRamId(term);
                    MutDisjointSets.union(RamId.RelPos(predSym, i), termID, set)
                }
            case RelOp.If(bools, body) =>
                Vector.forEach(unifyRamIdsBool(set), bools);
                unifyRamIdsOp(predicates, set, withProv, body)
        }

        ///
        /// Unify all `RamId`s in `boolExp` which can be proven to be equivalent, type-wise.
        ///
        def unifyRamIdsBool(set: MutDisjointSets[RamId, r], boolExp: BoolExp): Unit \ r = match boolExp {
            case BoolExp.Not(bexp) => unifyRamIdsBool(set, bexp)
            case BoolExp.IsEmpty(_) => ()
            case BoolExp.NotMemberOf(terms, RelSym.Symbol(PredSym.PredSym(_, id), _, _)) =>
                Vector.forEach(term -> unifyRamIdsTerm(set, term), terms);
                foreach ((i, term) <- ForEach.withIndex(terms)) {
                    MutDisjointSets.union(Ram.getTermRamId(term), RamId.RelPos(id, i), set)
                }
            case BoolExp.NotBot(term, _, _) => unifyRamIdsTerm(set, term)
            case BoolExp.Leq(_, _, _) => ()
            case BoolExp.Eq(term1, term2) =>
                unifyRamIdsTerm(set, term1);
                unifyRamIdsTerm(set, term2);
                MutDisjointSets.union(Ram.getTermRamId(term1), Ram.getTermRamId(term2), set)
            case BoolExp.Guard1(_, term1) =>
                unifyRamIdsTerm(set, term1)
            case BoolExp.Guard2(_, term1, term2) =>
                unifyRamIdsTerm(set, term1);
                unifyRamIdsTerm(set, term2)
            case BoolExp.Guard3(_, term1, term2, term3) =>
                unifyRamIdsTerm(set, term1);
                unifyRamIdsTerm(set, term2);
                unifyRamIdsTerm(set, term3)
            case BoolExp.Guard4(_, term1, term2, term3, term4) =>
                unifyRamIdsTerm(set, term1);
                unifyRamIdsTerm(set, term2);
                unifyRamIdsTerm(set, term3);
                unifyRamIdsTerm(set, term4)
            case BoolExp.Guard5(_, term1, term2, term3, term4, term5) =>
                unifyRamIdsTerm(set, term1);
                unifyRamIdsTerm(set, term2);
                unifyRamIdsTerm(set, term3);
                unifyRamIdsTerm(set, term4);
                unifyRamIdsTerm(set, term5)
        }

        ///
        /// Unify all `RamId`s in `term` which can be proven to be equivalent, type-wise.
        ///
        def unifyRamIdsTerm(set: MutDisjointSets[RamId, r], term: RamTerm): Unit \ r = match term {
            case RamTerm.Lit(_, _) => ()
            case RamTerm.RowLoad(_, _, _) => ()
            case RamTerm.ProvMax(vec) =>
                let id = Ram.getTermRamId(term);
                foreach ((rv, index) <- vec) {
                    MutDisjointSets.union(RamId.TuplePos(rv, index), id, set)
                }
            case RamTerm.Meet(_, t1, (rv, relSym), id) =>
                unifyRamIdsTerm(set, t1);
                let id1 = Ram.getTermRamId(t1);
                let id2 = Ram.getLatVarRamId(rv, relSym);
                MutDisjointSets.union(id1, id, set);
                MutDisjointSets.union(id1, id2, set)
            case RamTerm.App1(_, t1, RamId.Id(id)) =>
                unifyAppTerms(Vector#{t1}, id, set)
            case RamTerm.App2(_, t1, t2, RamId.Id(id)) =>
                unifyAppTerms(Vector#{t1, t2}, id, set)
            case RamTerm.App3(_, t1, t2, t3, RamId.Id(id)) =>
                unifyAppTerms(Vector#{t1, t2, t3}, id, set)
            case RamTerm.App4(_, t1, t2, t3, t4, RamId.Id(id)) =>
                unifyAppTerms(Vector#{t1, t2, t3, t4}, id, set)
            case RamTerm.App5(_, t1, t2, t3, t4, t5, RamId.Id(id)) =>
                unifyAppTerms(Vector#{t1, t2, t3, t4, t4, t5}, id, set)
            case _ => unreachable!()
        }

        ///
        /// Unify all `RamId`s that can be associated with `relSym1` and `relSym2`.
        ///
        /// Concretely, unify `RamId.RelPos(id1, i)` and `RamId.RelPos(id2, i)` for `0 <= i < arity`
        /// for symbols `RelSym(id1, arity, _)` and `RelSym(id2, arity, _)`.
        ///
        def unifyRelSyms(relSym1: RelSym, relSym2: RelSym, set: MutDisjointSets[RamId, r], withProv: Bool): Unit \ r =
            let arity = Ram.arityOf(relSym1);
            let usedArity = getProvSafeIdArity(arity, withProv);
            let id1 = Ram.toId(relSym1);
            let id2 = Ram.toId(relSym2);
            foreach (i <- Vector.range(0, usedArity)) {
                MutDisjointSets.union(RamId.RelPos(id1, i), RamId.RelPos(id2, i), set)
            }

        ///
        /// For `predSym = 'P'` unify all indexes for `(P, ΔP, ΔP')`.
        ///
        def unifyPredTypes(predicates: Predicates, set: MutDisjointSets[RamId, r], withProv: Bool, relSym: RelSym): Unit \ r =
            let fullSymbol = relSymFromPredType(relSym, PredType.Full, predicates);
            let deltaSymbol = fullRelSymToType(fullSymbol, PredType.Delta, predicates);
            let newSymbol = fullRelSymToType(fullSymbol, PredType.New, predicates);
            unifyRelSyms(fullSymbol, deltaSymbol, set, withProv);
            unifyRelSyms(fullSymbol, newSymbol, set, withProv);
            let arity = Ram.arityOf(fullSymbol);
            let id1 = Ram.toId(fullSymbol);
            if (withProv) {
                MutDisjointSets.union(RamId.RelPos(id1, arity), RamId.Id(-1), set);
                MutDisjointSets.union(RamId.RelPos(id1, arity + 1), RamId.Id(-1), set)
            } else ()

        ///
        /// Unify the `i`'th `RamTerm` in `terms` with `RamId.InId(id, i)`.
        ///
        /// This should be understood as 'register that term `i` will be input to
        /// the function at position `i`'.
        ///
        def unifyAppTerms(terms: Vector[RamTerm], id: Int32, set: MutDisjointSets[RamId, r]): Unit \ r =
            foreach ((i, t) <- ForEach.withIndex(terms)) {
                unifyRamIdsTerm(set, t);
                let id1 = Ram.getTermRamId(t);
                MutDisjointSets.union(id1, RamId.InId(id, i), set)
            }

        ///
        /// Based on the equivalence `disjoint` assign all unique `RamId`'s a `Boxing` position.
        ///
        def computeMappingStmt(disjoint: MutDisjointSets[RamId, r], map: MutMap[RamId, Int32, r], counter: Counter[r], withProv: Bool, stmt: RamStmt): Unit \ r = match stmt {
            case RamStmt.Insert(rest) => computeMappingOp(disjoint, map, counter, withProv, rest)
            case RamStmt.MergeInto(rel1, rel2) =>
                insertIndexRelSym(rel1, disjoint, map, counter, withProv);
                insertIndexRelSym(rel2, disjoint, map, counter, withProv)
            case RamStmt.Swap(rel1, rel2) =>
                insertIndexRelSym(rel1, disjoint, map, counter, withProv);
                insertIndexRelSym(rel2, disjoint, map, counter, withProv)
            case RamStmt.Purge(_) => ()
            case RamStmt.Seq(stmts) => Vector.forEach(computeMappingStmt(disjoint, map, counter, withProv), stmts)
            case RamStmt.Par(stmts) => Vector.forEach(computeMappingStmt(disjoint, map, counter, withProv), stmts)
            case RamStmt.Until(bools, body) =>
                computeMappingStmt(disjoint, map, counter, withProv, body);
                Vector.forEach(computeMappingBool(disjoint, map, counter), bools)
            case RamStmt.Comment(_) => ()
        }

        ///
        /// Based on the equivalence `disjoint` assign all unique `RamId`'s a `Boxing` position.
        ///
        def computeMappingOp(set: MutDisjointSets[RamId, r], map: MutMap[RamId, Int32, r], counter: Counter[r], withProv: Bool, op: RelOp): Unit \ r =
            let recurse = computeMappingOp(set, map, counter, withProv);
            let insert = registerRamId(set, map, counter);
            match op {
                case RelOp.Search(rv, RelSym.Symbol(PredSym.PredSym(_, predSym), arity, _), body) =>
                    let usedArity = getProvSafeIdArity(arity, withProv);
                    foreach (i <- Vector.range(0, usedArity)) {
                        insert(RamId.TuplePos(rv, i));
                        insert(RamId.RelPos(predSym, i))
                    };
                    recurse(body)
                case RelOp.Query(rv, RelSym.Symbol(PredSym.PredSym(_, predSym), arity, _), bools, _, body) =>
                    let usedArity = getProvSafeIdArity(arity, withProv);
                    foreach (i <- Vector.range(0, usedArity)) {
                        insert(RamId.TuplePos(rv, i));
                        insert(RamId.RelPos(predSym, i))
                    };
                    Vector.forEach(computeMappingBool(set, map, counter), bools);
                    recurse(body)
                case RelOp.Functional(RowVar.Named(id), _, inputTerms, body, arity) => // equality on the input should be handled elsewhere.
                    Vector.forEach(i -> insert(RamId.TuplePos(RowVar.Named(id), i)), Vector.range(0, arity));
                    Vector.forEach(i -> insert(RamId.InId(id, i)), Vector.range(0, Vector.length(inputTerms)));
                    Vector.forEach(computeMappingTerm(set, map, counter), inputTerms);
                    recurse(body)
                case RelOp.Project(terms, RelSym.Symbol(PredSym.PredSym(_, predSym), _, _), _) =>
                    terms |> Vector.forEachWithIndex(i -> _ -> insert(RamId.RelPos(predSym, i)));
                    terms |> Vector.forEach(computeMappingTerm(set, map, counter))
                case RelOp.If(bools, body) =>
                    recurse(body);
                    Vector.forEach(computeMappingBool(set, map, counter), bools)
            }

        ///
        /// Assign all `RamId`s in `boolExp` `UnifiedTypePos`.
        ///
        def computeMappingBool(
            set: MutDisjointSets[RamId, r],
            map: MutMap[RamId, Int32, r],
            counter: Counter[r],
            boolExp: BoolExp
        ): Unit \ r =
            let insert = registerRamId(set, map, counter);
            let computeMappingTermRec = computeMappingTerm(set, map, counter);
            match boolExp {
                case BoolExp.Not(bexp) => computeMappingBool(set, map, counter, bexp)
                case BoolExp.IsEmpty(_) => ()
                case BoolExp.NotMemberOf(terms, RelSym.Symbol(PredSym.PredSym(_, predSym), arity, _)) =>
                    Vector.forEach(computeMappingTermRec, terms);
                    Vector.forEach(i -> insert(RamId.RelPos(predSym, i)), Vector.range(0, arity))
                case BoolExp.Eq(t1, t2) =>
                    computeMappingTermRec(t1);
                    computeMappingTermRec(t2)
                case BoolExp.NotBot(t1, _, _) =>
                    computeMappingTermRec(t1)
                case BoolExp.Leq(_, _, RelSym.Symbol(PredSym.PredSym(_, predSym), arity, _)) =>
                    insert(RamId.RelPos(predSym, arity - 1))
                case BoolExp.Guard1(_, t1) =>
                    computeMappingTermRec(t1)
                case BoolExp.Guard2(_, t1, t2) =>
                    computeMappingTermRec(t1);
                    computeMappingTermRec(t2)
                case BoolExp.Guard3(_, t1, t2, t3) =>
                    computeMappingTermRec(t1);
                    computeMappingTermRec(t2);
                    computeMappingTermRec(t3)
                case BoolExp.Guard4(_, t1, t2, t3, t4) =>
                    computeMappingTermRec(t1);
                    computeMappingTermRec(t2);
                    computeMappingTermRec(t3);
                    computeMappingTermRec(t4)
                case BoolExp.Guard5(_, t1, t2, t3, t4, t5) =>
                    computeMappingTermRec(t1);
                    computeMappingTermRec(t2);
                    computeMappingTermRec(t3);
                    computeMappingTermRec(t4);
                    computeMappingTermRec(t5)
            }

        ///
        /// Assign all `RamId`s in `term` `UnifiedTypePos`.
        ///
        def computeMappingTerm(
            set: MutDisjointSets[RamId, r],
            map: MutMap[RamId, Int32, r],
            counter: Counter[r],
            term: RamTerm
        ): Unit \ r =
            let registerRamIdRec = registerRamId(set, map, counter);
            let computeMappingTermRec = computeMappingTerm(set, map, counter);
            let computeMappingTermAppRec = computeMappingTermApp(computeMappingTermRec, registerRamIdRec, getTermRamId(term));
            match term {
                case RamTerm.Lit(_, id) => registerRamIdRec(id)
                case RamTerm.RowLoad(_, _, _) => registerRamIdRec(getTermRamId(term))
                case RamTerm.Meet(_, t1, (rv, relSym), RamId.Id(id)) =>
                    computeMappingTermRec(t1);
                    registerRamIdRec(Ram.getLatVarRamId(rv, relSym));
                    registerRamIdRec(RamId.Id(id))
                case RamTerm.App1(_, t1, _) =>
                    Vector#{t1} |> computeMappingTermAppRec
                case RamTerm.App2(_, t1, t2, _) =>
                    Vector#{t1, t2} |> computeMappingTermAppRec
                case RamTerm.App3(_, t1, t2, t3, _) =>
                    Vector#{t1, t2, t3} |> computeMappingTermAppRec
                case RamTerm.App4(_, t1, t2, t3, t4, _) =>
                    Vector#{t1, t2, t3, t4} |> computeMappingTermAppRec
                case RamTerm.App5(_, t1, t2, t3, t4, t5, _) =>
                    Vector#{t1, t2, t3, t4, t5} |> computeMappingTermAppRec
                case RamTerm.ProvMax(loads) =>
                    loads |> Vector.forEach(match (rv, index) -> registerRamIdRec(RamId.TuplePos(rv, index)))
                case RamTerm.Meet(_, _, _, _) => unreachable!()
            }

        ///
        /// Based on the equivalence `disjoint` assign all unique `RamId`'s a `Boxing` position.
        ///
        def computeMappingTermApp(
            computeMappingTermRec: RamTerm -> Unit \ r,
            registerRamIdRec: RamId -> Unit \ r,
            id: RamId,
            terms: Vector[RamTerm]
        ): Unit \ r =
            registerRamIdRec(id);
            let id1 = match id {
                case RamId.Id(i) => i
                case _ => unreachable!()
            };
            foreach ((i, t) <- ForEach.withIndex(terms)) {
                computeMappingTermRec(t);
                registerRamIdRec(RamId.InId(id1, i))
            }

        ///
        /// If `withProv` is true return `arity + 2`. Otherwise return `arity`.
        ///
        def getProvSafeIdArity(arity: Int32, withProv: Bool): Int32 =
            if (withProv) arity + 2
            else arity

        ///
        /// Add all `RamId`'s associated with `relSym` to `map`.
        ///
        /// Concretely, add `RamId.RelPos(id, i)` where `0 <= i <= arity` for `RelSym(id, arity, _)`.
        ///
        def insertIndexRelSym(
            relSym: RelSym,
            disjoint: MutDisjointSets[RamId, r],
            map: MutMap[RamId, Int32, r],
            counter: Counter[r],
            withProv: Bool
        ): Unit \ r =
            let arity = Ram.arityOf(relSym);
            let id = Ram.toId(relSym);
            let usedArity = getProvSafeIdArity(arity, withProv);
            foreach (i <- Vector.range(0, usedArity)) {
                registerRamId(disjoint, map, counter, RamId.RelPos(id, i))
            }

        ///
        /// Add `id` to `map` pointing to the `Int32` representing its boxing-information.
        ///
        /// Concretely, lookup `id` in `disjoint` to get `rep`. If `map[rep] => v` add
        /// [id => v]. Otherwise add `[id => newID]` and `[rep => newID]`.
        ///
        /// `counter` is used to generate new identifiers.
        ///
        def registerRamId(
            disjoint: MutDisjointSets[RamId, r],
            ramIdToBoxingPos: MutMap[RamId, Int32, r],
            counter: Counter[r],
            ramID: RamId
        ): Unit \ r =
            let repID = getOrCrash(MutDisjointSets.find(ramID, disjoint));
            match MutMap.get(repID, ramIdToBoxingPos) {
                case Some(v) => MutMap.put(ramID, v, ramIdToBoxingPos)
                case None =>
                    let newID = Counter.getAndIncrement(counter);
                    MutMap.put(ramID, newID, ramIdToBoxingPos);
                    MutMap.put(repID, newID, ramIdToBoxingPos)
            }
    }

}

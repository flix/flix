/*
 * Copyright 2021 Benjamin Dahse
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

mod Fixpoint.Interpreter {
    use Fixpoint.Ast.ExecutableRam.{RamStmt, RelOp, RamTerm, BoolExp, RamProgram, WriteTuple}
    use Fixpoint.Ast.Ram.{RelSym, toDenotation, arityOfNonLat, toId, RamId}
    use Fixpoint.Ast.Shared.Denotation
    use Fixpoint.Boxed
    use Fixpoint.Boxing.{Boxing, boxWith, unboxWith, RamIdToMarshal}
    use Fixpoint.Counter
    use Fixpoint.Helpers.{unwrap}
    use Fixpoint.JoinProfiler.getJoinEstimateNum
    use Fixpoint.Predicates.{PredSymMapInternal, allFullRelSyms}

    type alias Tuple[r: Region] = Array[Int64, r]

    type alias SearchTuple[r: Region] = Tuple[r]

    pub type alias Index[r: Region] = BPlusTree[r]

    pub type alias Indexes[r: Region] = Array[Index[r], r]

    pub type alias Database[r: Region] = MutMap[RelSym, MutMap[Vector[Int64], Int64, r], r]

    pub type alias DatabaseExternal[r: Region] = MutMap[RelSym, MutMap[Vector[Boxed], Boxed, r], r]

    pub type alias SearchEnv[r: Region] = (
        Vector[SearchTuple[r]], 
        Vector[SearchTuple[r]], 
        Array[Array[Int64, r], r],
        Array[Boxed, r]
    )

    pub type alias Context[r: Region] = (
        Indexes[r],
        SearchEnv[r],
        Boxing[r]
    )
    
    pub def mkIndex(
        rc: Region[r], 
        relSym: RelSym, 
        search: Vector[Int32], 
        tuples: Vector[(Vector[Int64], Boxed)]
    ): Index[r] \ r = 
        let index = BPlusTree.empty(rc, search);
        if(Vector.isEmpty(tuples)) 
            index
        else {
            Vector.forEach(match (nonLat, lat) -> {
                let asArray = unchecked_cast(nonLat as Array[Int64, r]);
                match toDenotation(relSym) {
                    case Denotation.Relational => 
                        BPlusTree.insert(asArray, lat, index)
                    case Denotation.Latticenal(_, _, lub, _) =>
                        BPlusTree.putWith(lub, asArray, lat, index)
                }
            }, tuples);
            index
        }

    @Internal
    pub def estimateJoinSizes(rc: Region[r], input: (RamProgram, Boxing[r])): Array[MutList[(Int32, Int32), r], r] \ r = 
        let expectedJoinSize = Array.empty(rc, getJoinEstimateNum(fst(input)));
        expectedJoinSize |> Array.transform(_ -> MutList.empty(rc));
        let _ = interpretWithInput(rc, Some(expectedJoinSize), input);
        expectedJoinSize        

    @Internal
    pub def interpret(rc: Region[r], input: (RamProgram, Boxing[r])): (DatabaseExternal[r], PredSymMapInternal) \ r = 
        interpretWithInput(rc, None, input)

    @Internal
    pub def interpretWithInput(
        rc: Region[r], 
        expectedJoinSize: Option[Array[MutList[(Int32, Int32), r], r]],
        input: (RamProgram, Boxing[r])
    ): (DatabaseExternal[r], PredSymMapInternal) \ r =
        let (program, boxing) = input;
        let indexes = match program {
            case RamProgram.Program(_, facts, _, (indexMap, posMap), _, _) => 
                let indexNum = match Map.maximumValue(posMap) {
                    case None => 0
                    case Some((_, v)) => v + 1
                };
                let indexes = Array.empty(rc, indexNum);
                indexMap |>
                Map.forEach(relSym -> searches -> {
                    let tuples = Map.getWithDefault(relSym, Vector#{}, facts);
                    Vector.forEachWithIndex(num -> search -> {
                        let pos = match Map.get((relSym, num), posMap) {
                            case Some(v) => v
                            case None => bug!("In Interpreter.interpret: ${relSym} has no index!")
                        };
                        Array.put(mkIndex(rc, relSym, search, tuples), pos, indexes)
                    }, searches)
                });
                indexes
        };
        interpretWithDatabase(rc, indexes, boxing, expectedJoinSize, program)

    @Internal
    def interpretWithDatabase(
        rc: Region[r], 
        db: Indexes[r], 
        boxing: Boxing[r], 
        expectedJoinSize: Option[Array[MutList[(Int32, Int32), r], r]],
        program: RamProgram
    ): (DatabaseExternal[r], PredSymMapInternal) \ r = match program {
        case RamProgram.Program(stmt, _, predState, (_, indexPos), (arities, constWrites), marshallIndexes) =>
            let minEnv = arities |> Vector.map(x -> Array.repeat(rc, x, Int64.minValue()));
            let maxEnv = arities |> Vector.map(x -> Array.repeat(rc, x, Int64.maxValue()));
            constWrites |> Vector.forEach(match (id1, id2, val) -> {
                Array.put(val, id2, Vector.get(id1, minEnv));
                Array.put(val, id2, Vector.get(id1, maxEnv))
            });
            let tupleEnv = Array.repeat(rc, Vector.length(arities), Array.empty(rc, 0));
            let latEnv = arities |> Vector.length |> Array.empty(rc);
            let env = (minEnv, maxEnv, tupleEnv, latEnv);
            let ctxt = (db, env, boxing);
            evalStmt(rc, ctxt, expectedJoinSize, stmt);
            (marshallDb(rc, db, boxing, indexPos, predState, marshallIndexes), predState)
    }


    def marshallDb(rc: Region[r], db: Indexes[r], boxing: Boxing[r], indexPos: Map[(RelSym, Int32), Int32], predTrack: PredSymMapInternal, boxingIndexes: RamIdToMarshal): DatabaseExternal[r] \ r =
        let res = MutMap.empty(rc);
        allFullRelSyms(predTrack) |>
        List.forEach(relSym -> {
            let innerMap = MutMap.empty(rc);
            let curBoxingInfo = Vector.range(0, arityOfNonLat(relSym)) |>
                Vector.map(i ->
                    unwrap(Map.get(RamId.RelPos(toId(relSym), i), boxingIndexes))
                );
            Array.get(unwrap(Map.get((relSym, 0), indexPos)), db) |>
            BPlusTree.forEach(arr -> latticeElem -> {
                let boxedArr = Vector.mapWithIndex(i -> val ->  boxWith(val, Vector.get(i, curBoxingInfo), boxing), Array.toVector(arr));
                MutMap.put(boxedArr, latticeElem, innerMap)
            });
            MutMap.put(relSym, innerMap, res)
        });
        res

    def evalStmt(
        rc: Region[r], 
        ctxt: Context[r], 
        expectedJoinSize: Option[Array[MutList[(Int32, Int32), r], r]],
        stmt: RamStmt
    ): Unit \ r = match stmt {
        case RamStmt.Insert(relOp) =>
            evalOp(rc, ctxt, relOp)
        case RamStmt.Merge(srcIndex, dstIndex, lub, index) =>
            let (db, _, _) = ctxt;
            let src = Array.get(srcIndex, db);
            let dst = Array.get(dstIndex, db);
            if (index == -1) {
                BPlusTree.merge(src, dst)
            } else
                BPlusTree.mergeWith(lub, src, dst)
        case RamStmt.Swap(lhs, rhs) =>
            let (db, _, _) = ctxt;
            let temp = Array.get(lhs, db);
            Array.put(Array.get(rhs, db), lhs, db);
            Array.put(temp, rhs, db)
        case RamStmt.Purge(index) => 
            let (db, _, _) = ctxt;
            let oldIndex = Array.get(index, db);
            let newIndex = BPlusTree.empty(rc, BPlusTree.getSearch(oldIndex));
            Array.put(newIndex, index, db)
        case RamStmt.Seq(stmts) => 
            Vector.forEach(evalStmt(rc, ctxt, expectedJoinSize), stmts)
        case RamStmt.Until(test, body) =>
            if (evalBoolExps(rc, ctxt, test)) {
                ()
            } else {
                evalStmt(rc, ctxt, expectedJoinSize, body);
                evalStmt(rc, ctxt, expectedJoinSize, stmt)
            }
        case RamStmt.Comment(_) => ()
        case RamStmt.Par(stmts) => region rc2 {
            let (db, (minEnv, maxEnv, tupleEnv, latEnv), boxing) = ctxt;
            stmts |> Vector.forEach(innerStmt -> unchecked_cast({
                let _: Unit = spawn unchecked_cast({
                    let threadMinEnv = minEnv |> Vector.map(x -> x);
                    let threadMaxEnv = maxEnv |> Vector.map(x -> x);
                    let threadTupleEnv = tupleEnv |> Array.map(rc, x -> x);
                    let threadLatEnv = latEnv |> Array.map(rc, x -> x); 
                    let threadEnv = (threadMinEnv, threadMaxEnv, threadTupleEnv, threadLatEnv);
                    let threadCtxt = (db, threadEnv, boxing);
                    evalStmt(rc, threadCtxt, expectedJoinSize, innerStmt)
                } as _ \ IO) @ rc2; // spawn may not take an r effect, so we cast it to IO
                ()
            } as _ \ rc2)) // The IO effect is undesirable, so we cast it back to an r effect
        }
        case RamStmt.EstimateJoinSize(relIndex, writeTo, attr) =>
            let prevRef = Ref.fresh(rc, None);
            let dupRef = Counter.fresh(rc);
            let sizeRef = Counter.fresh(rc);
            let (db, _, _) = ctxt;
            let relevantTree = Array.get(relIndex, db);
            relevantTree |> BPlusTree.forEach(curr -> _ -> {
                let isDup = 
                    Ref.get(prevRef) |> 
                    Option.exists(prev -> 
                        attr |> Vector.forAll(i -> Array.get(i, prev) == Array.get(i, curr))
                    );
                if (isDup) {
                    Counter.increment(dupRef)
                } else ();
                Ref.put(Some(curr), prevRef);
                Counter.increment(sizeRef)
            });
            let size = Counter.peek(sizeRef);
            let dup = Counter.peek(dupRef);
            MutList.push((size, dup), Array.get(writeTo, unwrap(expectedJoinSize)))
    }

    def evalOp(rc: Region[r], ctxt: Context[r], op: RelOp): Unit \ r = 
        let (db, env, boxing) = ctxt;
        match op {
            case RelOp.Search(tuplePos, searchIndex, writes, body) =>
                let (minEnv, maxEnv, tupleEnv, _) = env;
                Array.get(searchIndex, db) |>
                BPlusTree.forEach(t -> _ -> {
                    Array.put(t, tuplePos, tupleEnv);
                    writes |> Vector.forEach(match (srcIndex, dst, dstIndex) -> {
                        Array.put(Array.get(srcIndex, t), dstIndex, Vector.get(dst, minEnv));
                        Array.put(Array.get(srcIndex, t), dstIndex, Vector.get(dst, maxEnv))
                    });
                    evalOp(rc, ctxt, body)
                })
            case RelOp.Query(tuplePos, searchIndex, writes, body) =>
                let (minEnv, maxEnv, tupleEnv, _) = env;
                Array.get(searchIndex, db) |>
                BPlusTree.rangeQueryWith(
                    t -> _ -> {
                        Array.put(t, tuplePos, tupleEnv);
                        writes |> Vector.forEach(match (srcIndex, dst, dstIndex) -> {
                            Array.put(Array.get(srcIndex, t), dstIndex, Vector.get(dst, minEnv));
                            Array.put(Array.get(srcIndex, t), dstIndex, Vector.get(dst, maxEnv))
                        });
                        evalOp(rc, ctxt, body)
                    },
                    Vector.get(tuplePos, minEnv),
                    Vector.get(tuplePos, maxEnv)
                )
            case RelOp.SearchLattice(tuplePos, searchIndex, meetWith, Denotation.Latticenal(bot, leq, _, glb), writes, body) => 
                let (minEnv, maxEnv, _, _) = env;
                Array.get(searchIndex, db) |> 
                BPlusTree.forEach(latticeHandlingBodyFunc(rc, tuplePos, meetWith, glb, leq, bot, writes, body, ctxt))
            case RelOp.QueryLattice(tuplePos, searchIndex, meetWith, Denotation.Latticenal(bot, leq, _, glb), writes, body) => 
                let (minEnv, maxEnv, _, _) = env;
                Array.get(searchIndex, db) |>
                BPlusTree.rangeQueryWith(
                    latticeHandlingBodyFunc(rc, tuplePos, meetWith, glb, leq, bot, writes, body, ctxt),
                    Vector.get(tuplePos, minEnv),
                    Vector.get(tuplePos, maxEnv)
                )
            case RelOp.Functional(tupleIndex, f, terms, writes, body, i2) =>
                let args = 
                    terms |> 
                    Vector.mapWithIndex(_ -> x -> evalBoxedTerm(env, boxing, x));
                let result = 
                    f(args)|> 
                    Vector.map(vec ->
                        Vector.mapWithIndex(i -> x -> unboxWith(x, Vector.get(i, i2), boxing), vec)
                    );
                let (minEnv, maxEnv, tupleEnv, _) = env;
                result |> Vector.forEach(t -> {
                    writes |> Vector.forEach(match (srcIndex, dst, dstIndex) -> {
                        Array.put(Vector.get(srcIndex, t), dstIndex, Vector.get(dst, minEnv));
                        Array.put(Vector.get(srcIndex, t), dstIndex, Vector.get(dst, maxEnv))
                    });
                    Array.put(unchecked_cast(t as Array[Int64, r]), tupleIndex, tupleEnv);
                    evalOp(rc, ctxt, body)
                })
            case RelOp.Project(terms, rel, bot, leq, lub, index) =>
                let relIndex = Array.get(rel, db);
                if (index == -1) {
                    let tuple = unchecked_cast(Vector.map(evalTerm(env, boxing), terms) as Array[Int64, r]);
                    BPlusTree.insert(tuple, Boxed.None, relIndex)
                } else {
                    let len = Vector.length(terms);
                    let tuple = evalTermsWithLat(rc, ctxt, terms);
                    let latVal = evalBoxedTerm(env, boxing, Vector.get(len - 1, terms));
                    if (latVal `leq` bot) ()
                    else BPlusTree.putWith(lub, tuple, latVal, relIndex)
                }
            case RelOp.If(test, then) =>
                if (evalBoolExps(rc, ctxt, test))
                    evalOp(rc, ctxt, then)
                else
                    ()
            case _ => ???
        }

    def handleWriteTuples(
        t: Tuple[r],
        writes: Vector[WriteTuple],
        minEnv: Vector[SearchTuple[r]],
        maxEnv: Vector[SearchTuple[r]],
        tuplePos: Int32,
        tupleEnv: Array[Array[Int64, r], r]
    ): Unit \ r =
        writes |> Vector.forEach(match (srcIndex, dst, dstIndex) -> {
            Array.put(Array.get(srcIndex, t), dstIndex, Vector.get(dst, minEnv));
            Array.put(Array.get(srcIndex, t), dstIndex, Vector.get(dst, maxEnv))
        });
        Array.put(t, tuplePos, tupleEnv)

    def latticeHandlingBodyFunc(
        rc: Region[r],
        tuplePos: Int32,
        meetWith: Int32,
        glb: Boxed -> Boxed -> Boxed,
        leq: Boxed -> Boxed -> Bool,
        bot: Boxed,
        writes: Vector[WriteTuple],
        body: RelOp,
        ctxt: Context[r],
        t: Array[Int64, r],
        l: Boxed
    ): Unit \ r =
        let (_, env, _) = ctxt;
        let (minEnv, maxEnv, tupleEnv, latEnv) = env;
        handleWriteTuples(t, writes, minEnv, maxEnv, tuplePos, tupleEnv);
        // Recurse only if the meet is not bottom
        if (meetWith == tuplePos) {
            Array.put(l, tuplePos, latEnv);
            evalOp(rc, ctxt, body)
        } else {
            let meet = l `glb` Array.get(meetWith, latEnv);
            if (meet `leq` bot) ()
            else {
                Array.put(meet, tuplePos, latEnv);
                evalOp(rc, ctxt, body)
            }
        }

    def evalTermsWithLat(rc: Region[r], ctxt: Context[r], terms: Vector[RamTerm]): Array[Int64, r] \ r =
        let (_, env, boxing) = ctxt;
        let evalT = evalTerm(env, boxing);
        let len = Vector.length(terms);
        let tuple = Array.empty(rc, len - 1);
        def loop(i) = if (i < len - 1) {
            Array.put(evalT(Vector.get(i, terms)), i, tuple);
            loop(i + 1)
        } else ();
        loop(0);
        tuple


    def evalBoolExps(rc: Region[r], ctxt: Context[r], exps: Vector[BoolExp]): Bool \ r =
        let (db, env, boxing) = ctxt;
        let evalT = evalTerm(env, boxing);
        let evalBoxedT = evalBoxedTerm(env, boxing);
        def evalBoolExp(exp) = match exp {
            case BoolExp.Not(boolExp) => not evalBoolExp(boolExp)
            case BoolExp.IsEmpty(rel) => BPlusTree.isEmptyThreadUnsafe(Array.get(rel, db))
            case BoolExp.NotMemberOf(terms, rel, leq, index) =>
                let relIndex = Array.get(rel, db);
                if (index == -1)
                    let tuple = Vector.map(evalT, terms);
                    not BPlusTree.memberOfThreadUnsafe(unchecked_cast(tuple as Array[Int64, r]), relIndex)
                else {
                    let len = Vector.length(terms);
                    let tuple = evalTermsWithLat(rc, ctxt, terms);
                    let latVal = evalBoxedTerm(env, boxing, Vector.get(len - 1, terms));
                    let curLatVal = BPlusTree.getWithDefault(tuple, Boxed.None, relIndex);
                    if(curLatVal == Boxed.None)
                        true
                    else
                        not (latVal `leq` curLatVal)
                }
            case BoolExp.Eq(lhs, rhs) => evalTerm(env, boxing, lhs) == evalTerm(env, boxing, rhs)
            case BoolExp.Leq(lhs, leq, index) => 
                let (_, _, _, latEnv) = env;
                let rightSide = Array.get(index, latEnv);
                leq(lhs, rightSide)
            case BoolExp.Guard0(f) => f()
            case BoolExp.Guard1(f, t1) =>
                let boxed1 = evalBoxedT(t1);
                f(boxed1)
            case BoolExp.Guard2(f, t1, t2) =>
                let boxed1 = evalBoxedT(t1);
                let boxed2 = evalBoxedT(t2);
                f(boxed1)(boxed2)
            case BoolExp.Guard3(f, t1, t2, t3) =>
                let boxed1 = evalBoxedT(t1);
                let boxed2 = evalBoxedT(t2);
                let boxed3 = evalBoxedT(t3);
                f(boxed1)(boxed2)(boxed3)
            case BoolExp.Guard4(f, t1, t2, t3, t4) =>
                let boxed1 = evalBoxedT(t1);
                let boxed2 = evalBoxedT(t2);
                let boxed3 = evalBoxedT(t3);
                let boxed4 = evalBoxedT(t4);
                f(boxed1)(boxed2)(boxed3)(boxed4)
            case BoolExp.Guard5(f, t1, t2, t3, t4, t5) =>
                let boxed1 = evalBoxedT(t1);
                let boxed2 = evalBoxedT(t2);
                let boxed3 = evalBoxedT(t3);
                let boxed4 = evalBoxedT(t4);
                let boxed5 = evalBoxedT(t5);
                f(boxed1)(boxed2)(boxed3)(boxed4)(boxed5)
        };
        exps |> Vector.forAll(evalBoolExp)

    def evalBoxedTerm(env: SearchEnv[r], boxing: Boxing[r], term: RamTerm): Boxed \ {r} = 
        let evalT = evalBoxedTerm(env, boxing);
        match term {
            case RamTerm.Lit(_, b) => b
            case RamTerm.LoadFromTuple(tupleIndex, indexInTuple, i) => 
                let (_, _, tupleEnv, _) = env;
                let unboxed = Array.get(indexInTuple, Array.get(tupleIndex, tupleEnv));
                boxWith(unboxed, i, boxing)
            case RamTerm.LoadLatVar(index, _) =>
                let (_, _, _, latEnv) = env;
                Array.get(index, latEnv)
            case RamTerm.Meet(cap, lhs, rhs, _) =>
                let boxed1 = evalT(lhs);
                let boxed2 = evalT(rhs);
                boxed1 `cap` boxed2
            case RamTerm.App0(f, _) => f()
            case RamTerm.App1(f, t, _) =>
                let v1 = evalT(t);
                f(v1)
            case RamTerm.App2(f, t1, t2, _) =>
                let v1 = evalT(t1);
                let v2 = evalT(t2);
                f(v1)(v2)
            case RamTerm.App3(f, t1, t2, t3, _) =>
                let v1 = evalT(t1);
                let v2 = evalT(t2);
                let v3 = evalT(t3);
                f(v1)(v2)(v3)
            case RamTerm.App4(f, t1, t2, t3, t4, _) =>
                let v1 = evalT(t1);
                let v2 = evalT(t2);
                let v3 = evalT(t3);
                let v4 = evalT(t4);
                f(v1)(v2)(v3)(v4)
            case RamTerm.App5(f, t1, t2, t3, t4, t5, _) =>
                let v1 = evalT(t1);
                let v2 = evalT(t2);
                let v3 = evalT(t3);
                let v4 = evalT(t4);
                let v5 = evalT(t5);
                f(v1)(v2)(v3)(v4)(v5)
        }

    def evalTerm(env: SearchEnv[r], boxing: Boxing[r], term: RamTerm): Int64 \ {r} = 
        let evalBoxedT = evalBoxedTerm(env, boxing);
        match term {
            case RamTerm.Lit(v, _) => v
            case RamTerm.LoadFromTuple(tupleIndex, indexInTuple, _) =>
                let (_, _, tupleEnv, _) = env;
                Array.get(indexInTuple, Array.get(tupleIndex, tupleEnv))
            case RamTerm.LoadLatVar(index, boxingPos) => 
                let (_, _, _, latEnv) = env;
                unboxWith(Array.get(index, latEnv), boxingPos, boxing)
            case RamTerm.Meet(cap, lhs, rhs, i) =>
                let boxed1 = evalBoxedT(lhs);
                let boxed2 = evalBoxedT(rhs);
                unboxWith(boxed1 `cap` boxed2, i, boxing)
            case RamTerm.App0(f, i) =>
                unboxWith(f(), i, boxing)
            case RamTerm.App1(f, t, i) =>
                let boxed = evalBoxedT(t);
                unboxWith(f(boxed), i, boxing)
            case RamTerm.App2(f, t1, t2, i) =>
                let boxed1 = evalBoxedT(t1);
                let boxed2 = evalBoxedT(t2);
                unboxWith(f(boxed1)(boxed2), i, boxing)
            case RamTerm.App3(f, t1, t2, t3, i) =>
                let boxed1 = evalBoxedT(t1);
                let boxed2 = evalBoxedT(t2);
                let boxed3 = evalBoxedT(t3);
                unboxWith(f(boxed1)(boxed2)(boxed3), i, boxing)
            case RamTerm.App4(f, t1, t2, t3, t4, i) =>
                let boxed1 = evalBoxedT(t1);
                let boxed2 = evalBoxedT(t2);
                let boxed3 = evalBoxedT(t3);
                let boxed4 = evalBoxedT(t4);
                unboxWith(f(boxed1)(boxed2)(boxed3)(boxed4), i, boxing)
            case RamTerm.App5(f, t1, t2, t3, t4, t5, i) =>
                let boxed1 = evalBoxedT(t1);
                let boxed2 = evalBoxedT(t2);
                let boxed3 = evalBoxedT(t3);
                let boxed4 = evalBoxedT(t4);
                let boxed5 = evalBoxedT(t5);
                unboxWith(f(boxed1)(boxed2)(boxed3)(boxed4)(boxed5), i, boxing)
        }
}

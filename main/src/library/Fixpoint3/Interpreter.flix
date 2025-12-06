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

mod Fixpoint3.Interpreter {
    use Fixpoint3.Ast.ExecutableRam.{BoolExp, RamProgram, RamStmt, RamTerm, RelOp, WriteTuple}
    use Fixpoint3.Ast.Ram.{arityOfNonLat, Predicates, RelSym, Search, toDenotation, toId}
    use Fixpoint3.Ast.Shared.{BoxedDenotation => Denotation}
    use Fixpoint3.Ast.Shared.Denotation.{Latticenal, Relational}
    use Fixpoint3.Boxed
    use Fixpoint3.Boxing.{boxWith, unboxWith}
    use Fixpoint3.BoxingType.Boxing
    use Fixpoint3.Counter
    use Fixpoint3.Options.{usedArity, parLevel}
    use Fixpoint3.Predicate.{allFullRelSyms, getRelSymAsType}
    use Fixpoint3.Predicate.PredType.Full
    use Fixpoint3.TypeInfo.{TypeInformation, getTypeOf}
    use Fixpoint3.Util.getOrCrash

    ///
    /// A tuple/fact.
    ///
    type alias Tuple = Vector[Int64]

    ///
    /// A mutable tuple that can be used to search a relation.
    ///
    /// Consider the program:
    /// ```
    /// search a ∈ A do
    ///     search b ∈ B | b[0] = a[1] do
    ///         ...
    ///     end
    /// end
    /// ```
    /// After each `a` is met, the `SearchTuple` for `b` is updated. Assuming `B` has arity
    /// 2, the `SearchTuple`s for `a[1]=3` would be `[Int64.MinValue, 3]` and
    /// `[Int64.MaxValue, 3]`, indicating that we want tuples where the second value is 3.
    ///
    type alias SearchTuple[r: Region] = Array[Int64, r]

    ///
    /// An index mapping a tuple to its lattice element.
    ///
    type alias Index[r: Region] = BPlusTree[Tuple, Boxed, r]

    ///
    /// The indexes used to store relations in the program.
    ///
    type alias Indexes[r: Region] = Array[Index[r], r]

    ///
    /// The `IDB` or result as it is exposed to the Solver. It maps `RelSym` to an `Index`,
    /// which uses `MutMap` instead of a `BPlusTree`.
    ///
    pub type alias Database[r: Region] = MutMap[RelSym, BPlusTree[Vector[Boxed], Boxed, Static], r]

    ///
    /// A `SearchEnv` contains the dynamic information about the state of the Interpreter.
    ///
    /// The first 2 values contain information about which `SearchTuple` should be used by
    /// `Query` `RelOp`s. The third value contains the current tuple of the bound `RowVar`s.
    /// The fourth contains the current lattice values of the bound `RowVar`s.
    ///
    type alias SearchEnv[r: Region] = (
        Vector[SearchTuple[r]],
        Vector[SearchTuple[r]],
        Array[Tuple, r],
        Array[Boxed, r]
    )

    ///
    /// A `Context` contains all knowledge the Interpreter needs/uses while executing a
    /// RAM program.
    ///
    /// The first value is an array of the current indexes for the relations. For the second
    /// see `SearchEnv`. The third is information relating to translating to/from the
    /// `Int64` used to represent the `Boxed` values.
    ///
    type alias Context[r: Region] = (
        Indexes[r],
        SearchEnv[r],
        Boxing[r]
    )

    ///
    /// Returns the index at position `pos` in `indexes`.
    ///
    def lookupIndex(pos: Int32, indexes: Indexes[r]): Index[r] \ r =
        Array.get(pos, indexes)

    ///
    /// Stores `index` at `pos` in `indexes`.
    ///
    def storeIndex(index: Index[r], pos: Int32, indexes: Indexes[r]): Unit \ r =
        Array.put(index, pos, indexes)

    ///
    /// Returns an `Index` for `relSym` in region `rc` using `search` as sorting order and
    /// initialized with `tuples`.
    ///
    def mkIndex(
        rc: Region[r],
        relSym: RelSym,
        search: Vector[Int32],
        tuples: BPlusTree[Tuple, Boxed, r]
    ): Index[r] \ r =
        if (BPlusTree.search(tuples) == search) tuples // We can simply use `tuples` as it has the correct order.
        else {
            let index = BPlusTree.emptyWithArityAndSearch(rc, usedArity(), search);
            if (BPlusTree.isEmpty(tuples))
                index
            else {
                BPlusTree.forEach(nonLat -> lat -> {
                    match toDenotation(relSym) {
                        case Relational =>
                            BPlusTree.put(nonLat, lat, index)
                        case Latticenal(_, _, lub, _) =>
                            BPlusTree.putWith(lub, nonLat, lat, index)
                    }
                }, tuples);
                index
            }
        }

    ///
    /// Executes the `RamProgram` in `fst(input)` using the `Boxing` in `snd(input)`.
    ///
    pub def interpret(rc: Region[r], input: (RamProgram[r], Boxing[r])): (Database[r], Predicates) \ r =
        let (program, boxing) = input;
        // Create `Indexes` from `program`.
        let RamProgram.Program(_, facts, _, (indexMap, posMap), _, _) = program;
        let indexNum = match Map.maximumValue(posMap) {
            case None         => 0
            case Some((_, v)) => v + 1
        };
        let indexes = Array.empty(rc, indexNum);
        foreach ((relSym, searches) <- indexMap) {
            // We need to ensure that we do not pass a tree with `null` as search.
            let default = match Vector.head(searches) {
                case None => BPlusTree.empty(rc) // The foreach will be empty.
                case Some(search0) => BPlusTree.emptyWithArityAndSearch(rc, usedArity(), search0)
            };
            let tuples = Map.getWithDefault(relSym, default, facts);
            foreach ((num, search) <- ForEach.withIndex(searches)) {
                let pos = match Map.get((relSym, num), posMap) {
                    case Some(v) => v
                    case None    => bug!("In Fixpoint.Interpreter.interpretWithInput: ${relSym} has no index!")
                };
                storeIndex(mkIndex(rc, relSym, search, tuples), pos, indexes)
            }
        };
        interpretWithDatabase(rc, indexes, boxing, program)

    ///
    /// Executes `program` using `boxing` as `Boxing` information and `db` as initial
    /// `Indexes`.
    ///
    def interpretWithDatabase(
        rc: Region[r],
        db: Indexes[r],
        boxing: Boxing[r],
        program: RamProgram[r]
    ): (Database[r], Predicates) \ r = match program {
        case RamProgram.Program(stmt, _, predState, (_, indexPos), (arities, constWrites), typeInfo) =>
            let minEnv = arities |> Vector.map(x -> Array.repeat(rc, x, Int64.minValue()));
            let maxEnv = arities |> Vector.map(x -> Array.repeat(rc, x, Int64.maxValue()));
            foreach ((id1, id2, val) <- constWrites) {
                Array.put(val, id2, Vector.get(id1, minEnv));
                Array.put(val, id2, Vector.get(id1, maxEnv))
            };
            let tupleEnv = Array.repeat(rc, Vector.length(arities), Vector#{});
            let latEnv = arities |> Vector.length |> Array.empty(rc);
            let env = (minEnv, maxEnv, tupleEnv, latEnv);
            let ctx = (db, env, boxing);
            evalStmt(rc, ctx, parLevel(), stmt);
            // Remove the `IO` effect from dealing with static.
            let boxedFacts = unsafe IO { marshallDb(rc, db, boxing, indexPos, predState, typeInfo) };
            (boxedFacts, predState)
    }

    ///
    /// Transforms `db` into `Database`.
    ///
    /// For all `RelSym`, some `Index` for relation is selected and all values are boxed
    /// and copied to a `MutMap`.
    ///
    def marshallDb(
        rc: Region[r],
        db: Indexes[r],
        boxing: Boxing[r],
        indexPos: Map[(RelSym, Int32), Int32],
        predTrack: Predicates,
        typeInfo: TypeInformation
    ): Database[r] \ r + IO =
        let res = MutMap.empty(rc);
        foreach (relSym <- allFullRelSyms(predTrack)) {
            let innerMap = BPlusTree.empty(Static);
            let curBoxingInfo = getTypeOf(toId(relSym), typeInfo);
            Array.get(getOrCrash(Map.get((relSym, 0), indexPos)), db) |>
                BPlusTree.forEach(vec -> latticeElem -> {
                    let boxedVec = vec |>
                        Vector.mapWithIndex(i -> val -> boxWith(val, Vector.get(i, curBoxingInfo), boxing));
                    BPlusTree.put(boxedVec, latticeElem, innerMap)
                });
            MutMap.put(relSym, innerMap, res)
        };
        res

    ///
    /// Evaluate `stmt` under `ctx` with thread spawning only being allowed in the next
    /// `parLevel` `RelOp.Search`s.
    ///
    /// For `ctx = (db, env, boxing)`, the following changes can happen:
    ///
    ///     1. We can discover and record a previously unknown type in `boxing`.
    ///     2. We can add a new binding between a `BoxedObject` and an `Int64` in `boxing`.
    ///     3. We can add a tuple to one of the indexes of `db`
    ///     4. We can bind a tuple to a new value changing `env`.
    ///     5. We can purge or swap indexes in `db`.
    ///
    def evalStmt(
        rc: Region[r],
        ctx: Context[r],
        parLevel: Int32,
        stmt: RamStmt
    ): Unit \ r = match stmt {
        case RamStmt.Insert(relOp) =>
            evalOp(rc, ctx, parLevel, relOp)
        case RamStmt.MergeInto(srcIndex, dstIndex, Relational) =>
            let (db, _, _) = ctx;
            let src = lookupIndex(srcIndex, db);
            let dst = lookupIndex(dstIndex, db);
            BPlusTree.merge(src, dst)
        case RamStmt.MergeInto(srcIndex, dstIndex, Latticenal(_, _, lub, _)) =>
            let (db, _, _) = ctx;
            let src = lookupIndex(srcIndex, db);
            let dst = lookupIndex(dstIndex, db);
            BPlusTree.mergeWith(lub, src, dst)
        case RamStmt.Swap(lhs, rhs) =>
            let (db, _, _) = ctx;
            let temp = lookupIndex(lhs, db);
            storeIndex(lookupIndex(rhs, db), lhs, db);
            storeIndex(temp, rhs, db)
        case RamStmt.Purge(index) =>
            let (db, _, _) = ctx;
            let oldIndex = lookupIndex(index, db);
            let newIndex = BPlusTree.emptyWithArityAndSearch(rc, usedArity(), BPlusTree.search(oldIndex));
            storeIndex(newIndex, index, db)
        case RamStmt.Seq(stmts) =>
            Vector.forEach(evalStmt(rc, ctx, parLevel), stmts)
        case RamStmt.Until(test, body) =>
            if (evalBoolExps(ctx, test)) {
                ()
            } else {
                evalStmt(rc, ctx, parLevel, body);
                evalStmt(rc, ctx, parLevel, stmt)
            }
        case RamStmt.Comment(_) => ()
        // Note: The region construct ensures that all threads spawned in rc2 will have completed before we leave its lexical scope.
        case RamStmt.Par(stmts) => region rc2 {
            foreach (innerStmt <- stmts) {
                unchecked_cast({
                    let _: Unit = spawn unchecked_cast({
                        let threadCtx = copyCtx(rc, ctx);
                        evalStmt(rc, threadCtx, parLevel, innerStmt)
                    } as _ \ IO) @ rc2; // spawn may not take an `r` effect, so we cast it to `IO`
                    ()
                } as _ \ rc2)
            }
        }
    }

    ///
    /// Evaluates `op` under `ctx` with thread spawning only being allowed in the next
    /// `parLevel` `RelOp.Search`s.
    ///
    /// For `ctx = (db, env, boxing)`, the following changes can happen:
    ///
    ///     1. We can discover and record a previously unknown type in `boxing`.
    ///     2. We can add a new binding between a `BoxedObject` and an `Int64` in `boxing`.
    ///     3. We can add a tuple to one of the indexes of `db`
    ///     4. We can bind a tuple to a new value changing `env`.
    ///
    /// Purges and swaps of indexes in `db` cannot happen.
    ///
    def evalOp(rc: Region[r], ctx: Context[r], parLevel: Int32, op: RelOp): Unit \ r =
        let (db, env, boxing) = ctx;
        match op {
            case RelOp.Search(tuplePos, searchIndex, _, Relational, writes, body) =>
                if (parLevel == 0) {
                    // We have exhausted all allowed attempts to spawn threads. Recurse sequentially.
                    lookupIndex(searchIndex, db) |>
                        BPlusTree.forEach(evalSearchOrQueryOp(rc, tuplePos, writes, body, ctx, parLevel))
                } else {
                    // We are allowed to spawn threads. Try to do so.
                    lookupIndex(searchIndex, db) |>
                        BPlusTree.parForEach(parEvalSearchOrQueryOp(rc, tuplePos, writes, body, ctx, parLevel - 1))
                }
            case RelOp.Query(tuplePos, searchIndex, _, Relational, writes, body) =>
                let (minEnv, maxEnv, _, _) = env;
                lookupIndex(searchIndex, db) |>
                    BPlusTree.rangeQueryWith(
                        evalSearchOrQueryOp(rc, tuplePos, writes, body, ctx, parLevel),
                        // We could here use Vector.toArray, but it would create a copy. To avoid memory
                        // allocation we simply cast.
                        unchecked_cast((Vector.get(tuplePos, minEnv): Array[Int64, r]) as Tuple),
                        unchecked_cast((Vector.get(tuplePos, maxEnv): Array[Int64, r]) as Tuple)
                    )
            case RelOp.If(test, then) =>
                if (evalBoolExps(ctx, test))
                    evalOp(rc, ctx, parLevel, then)
                else
                    ()
            case RelOp.Project(terms, rel, den) => match den {
                case Relational =>
                    let relIndex = lookupIndex(rel, db);
                    let tuple = Vector.map(evalTerm(env, boxing), terms);
                    BPlusTree.put(tuple, Boxed.NoValue, relIndex)
                case Latticenal(bot, leq, lub, _) =>
                    // Add the binding `tuple => latVal` to `relIndex` if and only if `latVal is not
                    // `bot`. If `tuple => otherLat` was in `relIndex` replace it with
                    // `tuple => lub(latVal, otherLat)`.

                    // Note that a previous `RelOp.If` should have asserted that `latVal` is not less
                    // than the current value for `tuple` in the full relation.
                    let relIndex = lookupIndex(rel, db);
                    let len = Vector.length(terms);
                    let tuple = evalTermsWithLat(ctx, terms);
                    let latVal = evalBoxedTerm(env, boxing, Vector.get(len - 1, terms));
                    if (latVal `leq` bot) ()
                    else BPlusTree.putWith(lub, tuple, latVal, relIndex)
            }
            case RelOp.Search(tuplePos, searchIndex, meetWith, Latticenal(bot, leq, _, glb), writes, body) =>
                if (parLevel == 0) {
                    // We have exhausted all allowed attempts to spawn threads. Recurse sequentially.
                    lookupIndex(searchIndex, db) |>
                        BPlusTree.forEach(evalSearchOrQueryOpLattice(rc, tuplePos, meetWith, bot, leq, glb, writes, body, ctx, parLevel))
                } else {
                    // We are allowed to spawn threads. Try to do so.
                    lookupIndex(searchIndex, db) |>
                        BPlusTree.parForEach(parEvalSearchOrQueryOpLattice(rc, tuplePos, meetWith, bot, leq, glb, writes, body, ctx, parLevel))
                }
            case RelOp.Query(tuplePos, searchIndex, meetWith, Latticenal(bot, leq, _, glb), writes, body) =>
                let (minEnv, maxEnv, _, _) = env;
                lookupIndex(searchIndex, db) |>
                    BPlusTree.rangeQueryWith(
                        evalSearchOrQueryOpLattice(rc, tuplePos, meetWith, bot, leq, glb, writes, body, ctx, parLevel),
                        // We could here use Vector.toArray, but it would create a copy. To avoid memory
                        // allocation we simply cast.
                        unchecked_cast((Vector.get(tuplePos, minEnv): Array[Int64, r]) as Tuple),
                        unchecked_cast((Vector.get(tuplePos, maxEnv): Array[Int64, r]) as Tuple)
                    )
            case RelOp.Project(terms, rel, Latticenal(bot, leq, lub, _)) =>
                let relIndex = lookupIndex(rel, db);
                let len = Vector.length(terms);
                let tuple = evalTermsWithLat(ctx, terms);
                let latVal = evalBoxedTerm(env, boxing, Vector.get(len - 1, terms));
                if (latVal `leq` bot) ()
                else BPlusTree.putWith(lub, tuple, latVal, relIndex)
            case RelOp.ProvProject(terms, rel, fullRel) =>
                let relIndex = lookupIndex(rel, db);
                let fullIndex = lookupIndex(fullRel, db);
                let tuple = Vector.map(evalTerm(env, boxing), terms);
                let depthPos = Vector.length(terms) - 2;
                let chooseSmallest = k1 -> _v1 -> k2 -> _v2 -> Vector.get(depthPos, k1) < Vector.get(depthPos, k2);
                match BPlusTree.getKeyAndValue(tuple, fullIndex) {
                    case None =>
                        BPlusTree.putIf(chooseSmallest, tuple, Boxed.NoValue, relIndex)
                    case Some((k, v)) if (chooseSmallest(tuple, Boxed.NoValue, k, v)) =>
                        BPlusTree.putIf(chooseSmallest, tuple, Boxed.NoValue, relIndex)
                    case _ => ()
                }
            case RelOp.Functional(tupleIndex, f, terms, writes, body, i2) =>
                let args = Vector.map(evalBoxedTerm(env, boxing), terms);
                let result = f(args)|>
                    Vector.map(vec ->
                        Vector.mapWithIndex(i -> x -> unboxWith(x, Vector.get(i, i2), boxing), vec)
                    );
                let (minEnv, maxEnv, tupleEnv, _) = env;
                foreach (t <- result) {
                    bindTuple(t, writes, minEnv, maxEnv, tupleIndex, tupleEnv);
                    evalOp(rc, ctx, parLevel, body)
                }
        }

    ///
    /// Handle `writes` using `t` as 'from' and writing to `minEnv` and `maxEnv`.
    ///
    /// For `t=[8, 42]` and `writes=[(1, 2, 0)]`, writes `minEnv[2][0] = t[1]`.
    /// The same update is done for `maxEnv`.
    ///
    /// Lastly, `t` is saved at `tuplePos` in `tupleEnv`, i.e. `tupleEnv[tuplePos] = t`.
    ///
    def bindTuple(
        t: Tuple,
        writes: Vector[WriteTuple],
        minEnv: Vector[SearchTuple[r]],
        maxEnv: Vector[SearchTuple[r]],
        tuplePos: Int32,
        tupleEnv: Array[Tuple, r]
    ): Unit \ r =
        foreach ((srcIndex, dst, dstIndex) <- writes) {
            Array.put(Vector.get(srcIndex, t), dstIndex, Vector.get(dst, minEnv));
            Array.put(Vector.get(srcIndex, t), dstIndex, Vector.get(dst, maxEnv))
        };
        Array.put(t, tuplePos, tupleEnv)

    ///
    /// Clones `ctx` for a concurrent thread.
    ///
    /// After copying, only `db` and `boxing` are shared.
    ///
    def copyCtx(rc: Region[r], ctx: Context[r]): Context[r] \ r = {
        let (db, (minEnv, maxEnv, tupleEnv, latEnv), boxing) = ctx;
        let threadMinEnv = Vector.map(Array.copy(rc), minEnv);
        let threadMaxEnv = Vector.map(Array.copy(rc), maxEnv);
        let threadTupleEnv = Array.copy(rc, tupleEnv);
        let threadLatEnv = Array.copy(rc, latEnv);
        let threadEnv = (threadMinEnv, threadMaxEnv, threadTupleEnv, threadLatEnv);
        (db, threadEnv, boxing)
    }

    ///
    /// Binds the `RowVar`, which has been assigned to the index `tuplePos`, to the value
    /// `t` and executes `body`.
    ///
    /// Assumes exclusive access to `ctx`.
    ///
    /// `body` is the body of the `Search` or `Query`. `writes` is a vector of writes to
    /// `SearchTuple`s to be performed. See `bindTuple`.
    ///
    def evalSearchOrQueryOp(
        rc: Region[r],
        tuplePos: Int32,
        writes: Vector[WriteTuple],
        body: RelOp,
        ctx: Context[r],
        parLevel: Int32,
        t: Tuple,
        _: Boxed
    ): Unit \ r =
        let (_, env, _) = ctx;
        let (minEnv, maxEnv, tupleEnv, _) = env;
        bindTuple(t, writes, minEnv, maxEnv, tuplePos, tupleEnv);
        evalOp(rc, ctx, parLevel, body)

    ///
    /// Copies `ctx`, binds the `RowVar` assigned to index `tuplePos` to the value `t`,
    /// and executes `body`.
    ///
    /// Should only be used when new threads are being spawned.
    ///
    /// `body` is the body of the `Search` or `Query`. `writes` is a vector of writes to
    /// `SearchTuple`s to be performed. See `bindTuple`.
    ///
    /// `ignored` is due to the `forEach` method on `BPlusTree`s, which gives key-value
    /// pairs, and we do not care about the `Boxed.NoValue` that non-lattice relations have.
    ///
    def parEvalSearchOrQueryOp(
        rc: Region[r],
        tuplePos: Int32,
        writes: Vector[WriteTuple],
        body: RelOp,
        ctx: Context[r],
        parLevel: Int32,
        t: Tuple,
        ignored: Boxed
    ): Unit \ r =
        evalSearchOrQueryOp(rc, tuplePos, writes, body, copyCtx(rc, ctx), parLevel, t, ignored)

    ///
    /// Binds the `RowVar` assigned to index `tuplePos` to the tuple value `t` and lattice value
    /// `l` and executes `body`.
    ///
    /// Assumes exclusive access to `ctx`.
    ///
    /// `body` is the body of the `Search` or `Query`. `writes` is a vector of writes to
    /// `SearchTuple`s to be performed. See `bindTuple`.
    ///
    def evalSearchOrQueryOpLattice(
        rc: Region[r],
        tuplePos: Int32,
        meetWith: Int32,
        bot: Boxed,
        leq: Boxed -> Boxed -> Bool,
        glb: Boxed -> Boxed -> Boxed,
        writes: Vector[WriteTuple],
        body: RelOp,
        ctx: Context[r],
        parLevel: Int32,
        t: Tuple,
        l: Boxed
    ): Unit \ r =
        let (_, env, _) = ctx;
        let (minEnv, maxEnv, tupleEnv, latEnv) = env;
        bindTuple(t, writes, minEnv, maxEnv, tuplePos, tupleEnv);
        // Recurse only if the meet is not bottom
        if (meetWith == tuplePos) {
            // `l` cannot be `bot`, so just recurse.
            Array.put(l, tuplePos, latEnv);
            evalOp(rc, ctx, parLevel, body)
        } else {
            let meet = l `glb` Array.get(meetWith, latEnv);
            if (meet `leq` bot) ()
            else {
                Array.put(meet, tuplePos, latEnv);
                evalOp(rc, ctx, parLevel, body)
            }
        }

    ///
    /// Copies `ctx`, binds the `RowVar` assigned to index `tuplePos` to the tuple value `t`
    /// and lattice value `l`, and executes `body`.
    ///
    /// Should only be used when new threads are being spawned.
    ///
    /// `body` is the body of the `Search` or `Query`. `writes` is a vector of writes to
    /// `SearchTuple`s to be performed. See `bindTuple`.
    ///
    def parEvalSearchOrQueryOpLattice(
        rc: Region[r],
        tuplePos: Int32,
        meetWith: Int32,
        bot: Boxed,
        leq: Boxed -> Boxed -> Bool,
        glb: Boxed -> Boxed -> Boxed,
        writes: Vector[WriteTuple],
        body: RelOp,
        ctx: Context[r],
        parLevel: Int32,
        t: Tuple,
        l: Boxed
    ): Unit \ r =
        evalSearchOrQueryOpLattice(rc, tuplePos, meetWith, bot, leq, glb, writes, body, copyCtx(rc, ctx), parLevel, t, l)

    ///
    /// Evaluates `terms` except the last term.
    ///
    def evalTermsWithLat(ctx: Context[r], terms: Vector[RamTerm]): Tuple \ r =
        let (_, env, boxing) = ctx;
        let evalT = evalTerm(env, boxing);
        let len = Vector.length(terms);
        Vector.range(0, len - 1) |> Vector.map(i -> evalT(Vector.get(i, terms)))

    ///
    /// Evaluate `boolExp` under context `ctx` and returns true iff `boolExp` is `true`.
    ///
    def evalBoolExp(ctx: Context[r], boolExp: BoolExp): Bool \ r =
        let (db, env, boxing) = ctx;
        let evalT = evalTerm(env, boxing);
        let evalBoxedT = evalBoxedTerm(env, boxing);
        match boolExp {
            case BoolExp.Not(bexp) => not evalBoolExp(ctx, bexp)
            case BoolExp.IsEmpty(rel) => BPlusTree.isEmpty(Array.get(rel, db))
            case BoolExp.NotMemberOf(terms, rel, Relational) =>
                let relIndex = lookupIndex(rel, db);
                let tuple = Vector.map(evalT, terms);
                not BPlusTree.memberOf(tuple, relIndex)
            case BoolExp.NotMemberOf(terms, rel, Latticenal(_, leq, _, _)) =>
                let relIndex = lookupIndex(rel, db);
                let len = Vector.length(terms);
                let tuple = evalTermsWithLat(ctx, terms);
                let latVal = evalBoxedTerm(env, boxing, Vector.get(len - 1, terms));
                match BPlusTree.getWithDefault(tuple, Boxed.NoValue, relIndex) {
                    case Boxed.NoValue => true
                    case curLatVal     => not (latVal `leq` curLatVal)
                }
            case BoolExp.Eq(lhs, rhs) => evalTerm(env, boxing, lhs) == evalTerm(env, boxing, rhs)
            case BoolExp.Leq(lhs, leq, index) =>
                let (_, _, _, latEnv) = env;
                let rightSide = Array.get(index, latEnv);
                leq(lhs, rightSide)
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
        }

    ///
    /// Evaluate `exps` under context `ctx` and returns true iff all `BoolExp` in `exps`
    /// are `true`.
    ///
    def evalBoolExps(ctx: Context[r], exps: Vector[BoolExp]): Bool \ r =
        Vector.forAll(evalBoolExp(ctx), exps)

    ///
    /// Evaluate `term` under `env` using `boxing` to box/unbox values as needed.
    ///
    /// Returns the result as a `Boxed`.
    ///
    def evalBoxedTerm(env: SearchEnv[r], boxing: Boxing[r], term: RamTerm): Boxed \ r =
        let evalT = evalBoxedTerm(env, boxing);
        match term {
            case RamTerm.Lit(_, b) => b
            case RamTerm.LoadFromTuple(tupleIndex, indexInTuple, i) =>
                let (_, _, tupleEnv, _) = env;
                let unboxed = Vector.get(indexInTuple, Array.get(tupleIndex, tupleEnv));
                boxWith(unboxed, i, boxing)
            case RamTerm.ProvMax(_) => Boxed.BoxedInt64(evalTerm(env, boxing, term))
            case RamTerm.LoadLatVar(index, _) =>
                let (_, _, _, latEnv) = env;
                Array.get(index, latEnv)
            case RamTerm.Meet(cap, lhs, rhs, _) =>
                let boxed1 = evalT(lhs);
                let boxed2 = evalT(rhs);
                boxed1 `cap` boxed2
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

    ///
    /// Evaluate `term` under `env` using `boxing` to box/unbox values as needed.
    ///
    /// Returns the result as the `Int64` representing the computed value.
    ///
    def evalTerm(env: SearchEnv[r], boxing: Boxing[r], term: RamTerm): Int64 \ r =
        let evalBoxedT = evalBoxedTerm(env, boxing);
        match term {
            case RamTerm.Lit(v, _) => v
            case RamTerm.LoadFromTuple(tupleIndex, indexInTuple, _) =>
                let (_, _, tupleEnv, _) = env;
                Vector.get(indexInTuple, Array.get(tupleIndex, tupleEnv))
            case RamTerm.LoadLatVar(index, boxingPos) =>
                let (_, _, _, latEnv) = env;
                unboxWith(Array.get(index, latEnv), boxingPos, boxing)
            case RamTerm.ProvMax(vec) =>
                // The depth of the fact is the 1 plus the maximum depth of the facts used to derive it.
                // We therefore loop over all tuples in `vec` which describes the placements of the facts
                // used to derive this fact and max over their depth.
                // For a tuple `(tupleIndex, indexInTuple)` `tupleIndex` describes where the fact is placed
                // and `indexInTuple` described at which of the entries in the fact is the depth.
                let (_, _, tupleEnv, _) = env;
                vec
                    |> Vector.map(match (tupleIndex, indexInTuple) -> 1i64 + Vector.get(indexInTuple, Array.get(tupleIndex, tupleEnv)))
                    |> Vector.maximum
                    |> Option.getWithDefault(1i64)
            case RamTerm.Meet(cap, lhs, rhs, i) =>
                let boxed1 = evalBoxedT(lhs);
                let boxed2 = evalBoxedT(rhs);
                unboxWith(boxed1 `cap` boxed2, i, boxing)
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

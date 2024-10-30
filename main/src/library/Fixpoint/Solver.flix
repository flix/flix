/*
 * Copyright 2021 Benjamin Dahse
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

mod Fixpoint.Solver {

    import dev.flix.runtime.Global

    use Fixpoint.Ast.Datalog.{Datalog, Constraint, HeadTerm}
    use Fixpoint.Ast.Datalog.Datalog.{Datalog, Model, Join}
    use Fixpoint.Ast.Datalog.Constraint.Constraint
    use Fixpoint.Ast.Datalog.HeadPredicate.HeadAtom
    use Fixpoint.Ast.Ram.RamSym
    use Fixpoint.Ast.Shared.Denotation
    use Fixpoint.Ast.Shared.Denotation.{Relational, Latticenal}
    use Fixpoint.Ast.Shared.PredSym
    use Fixpoint.Ast.Shared.PredSym.PredSym
    use Fixpoint.Boxable.{box, unbox}
    use Fixpoint.Boxed
    use Fixpoint.Interpreter.Database

    ///
    /// Returns the minimal model of the given Datalog program `d`.
    /// The minimal model is given by the model semantics for Datalog programs with stratified negation.
    /// The stratification is computed automatically by the solver.
    ///
    @Internal
    pub def run(d: Datalog[v]): Datalog[v] with Order[v] =
        Fixpoint.Phase.Stratifier.stratify(d) |> runWithStratification(d)

    ///
    /// Returns the minimal model of the given Datalog program `d`.
    /// The minimal model is given by the model semantics for Datalog programs with stratified negation.
    /// A stratification of `d` is given by `stf`.
    ///
    @Internal
    def runWithStratification(d0: Datalog[v], stf: Map[PredSym, Int32]): Datalog[v] with Order[v] = {
        let d = Fixpoint.Debugging.notifyPreSolve(d0);
        let compiler = cs ->
            Fixpoint.Phase.Compiler.compile(cs, stf) |>
            Fixpoint.Phase.Simplifier.simplifyStmt |>
            Fixpoint.Phase.IndexSelection.queryStmt |>
            Fixpoint.Phase.VarsToIndices.lowerStmt;
        let model = match d {
            case Datalog(_) => region rc {
                compiler(d) |> Fixpoint.Interpreter.interpret(rc) |> toModel
            }
            case Model(_) => d
            case Join(Model(m), cs) => region rc {
                let db = Map.map(Map.toMutMap(rc), m) |> Map.toMutMap(rc);
                compiler(cs) |> Fixpoint.Interpreter.interpretWithDatabase(rc, db) |> toModel
            }
            case _ => bug!("Datalog normalization bug")
        };
        let m = Fixpoint.Debugging.notifyPostSolve(model);
        m
    }

    ///
    /// Returns the pairwise union of `d1` and `d2`.
    /// I.e. the facts of the union is the union of the facts and likewise for rules.
    /// A fact or rule may occur twice in the Datalog program. This has no effect on its semantics.
    ///
    @Internal
    pub def union(d1: Datalog[v], d2: Datalog[v]): Datalog[v] with Order[v] = match (d1, d2) {
        case (Datalog(edb1, idb1), Datalog(edb2, idb2)) =>
            Datalog(Vector.append(edb1, edb2), Vector.append(idb1, idb2))
        case (Model(db1), Model(db2)) =>
            use Fixpoint.Ast.Ram.toDenotation;
            let union = ramSym -> match toDenotation(ramSym) {
                case Relational => Map.union
                case Latticenal(_, _, lub, _) => Map.unionWith(lub)
            };
            Model(Map.unionWithKey(union, db1, db2))
        case (Model(_), Datalog(_, _)) => Join(d1, d2)
        case (Datalog(_, _), Model(_)) => Join(d2, d1)
        case (Join(model, datalog), Datalog(_, _)) => Join(model, union(datalog, d2))
        case (Datalog(_, _), Join(model, datalog)) => Join(model, union(datalog, d1))
        case (Join(model, datalog), Model(_)) => Join(union(model, d2), datalog)
        case (Model(_), Join(model, datalog)) => Join(union(model, d1), datalog)
        case (Join(x, y), Join(z, w)) => Join(union(x, z), union(y, w))
    }

    ///
    /// Project all facts in the given Datalog program `d` that has `p` in its head.
    ///
    @Internal
    pub def projectSym(p: PredSym, d: Datalog[v]): Datalog[v] with Order[v] = match d {
        case Datalog(facts, _) =>
            let pFacts = Vector.filter(match Constraint(HeadAtom(head, _, _), _) -> head == p, facts);
            Datalog(pFacts, Vector.empty())
        case Model(db) =>
            Map.rangeQuery(ramSym -> match ramSym {
                case RamSym.Full(predSym, _, _) => match predSym <=> p {
                    case Comparison.EqualTo => Comparison.EqualTo
                    case cmp => cmp
                }
                case _ => Comparison.LessThan
            }, db) |>
            List.toMap |>
            Model
        case Join(d1, d2) => union(projectSym(p, d1), projectSym(p, d2))
    }

    ///
    /// Renames every predicate symbol in `d` with a fresh name, except for those in `preds`.
    ///
    @Internal
    pub def rename(preds: List[PredSym], d: Datalog[v]): Datalog[v] with Order[v] = {
        use Fixpoint.PredSymsOf.predSymsOf;
        use Fixpoint.SubstitutePredSym.substitute;

        // Determine all the predicate symbols in `d`.
        let predSyms = predSymsOf(d);

        // Compute a substitition that assignes fresh names to all predicates except those in `preds`.
        def f(macc, predSym) = {
            if (List.memberOf(predSym, preds)) {
                // Case 1: Do not rename. Do not extend the map.
                macc
            } else {
                // Case 2: Introduce a fresh name (index) for the predicate symbol.
                let PredSym(name, _) = predSym;
                let nextId = unchecked_cast((Global.newId() + 1i64) as _ \ {});  // We add one because the counter starts at zero.
                let newPredSym = PredSym(name, nextId);
                Map.insert(predSym, newPredSym, macc)
            }
        };
        let s = Set.foldLeft(f, Map.empty(), predSyms);

        // Apply the computed substitution to the Datalog program `d`.
        substitute(d, s)
    }

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to `#{p(t1). p(t1').}`
    /// given the sequence `(t1), (t1')`.
    ///
    @Internal
    pub def injectInto1(p: PredSym, ts: f[t1]):
        Datalog[Boxed] \ Foldable.Aef[f] with Order[t1], Foldable[f] =
        injectIntoX(match (v1) ->
            box(v1) ::
            Nil
        , p, ts)

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to `#{p(t1, t2). p(t1', t2').}`
    /// given the sequence `(t1, t2), (t1', t2')`.
    ///
    @Internal
    pub def injectInto2(p: PredSym, ts: f[(t1, t2)]):
        Datalog[Boxed] \ Foldable.Aef[f] with Order[t1], Order[t2], Foldable[f] =
        injectIntoX(match (v1, v2) ->
            box(v1) ::
            box(v2) ::
            Nil
        , p, ts)

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to `#{p(t1, t2, t3). p(t1', t2', t3').}`
    /// given the sequence `(t1, t2, t3), (t1', t2', t3')`.
    ///
    @Internal
    pub def injectInto3(p: PredSym, ts: f[(t1, t2, t3)]):
        Datalog[Boxed] \ Foldable.Aef[f] with Order[t1], Order[t2], Order[t3], Foldable[f] =
        injectIntoX(match (v1, v2, v3) ->
            box(v1) ::
            box(v2) ::
            box(v3) ::
            Nil
        , p, ts)

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4). p(t1', t2', t3', t4').}`
    /// given the sequence `(t1, t2, t3, t4), (t1', t2', t3', t4')`.
    ///
    @Internal
    pub def injectInto4(p: PredSym, ts: f[(t1, t2, t3, t4)]):
        Datalog[Boxed] \ Foldable.Aef[f] with Order[t1], Order[t2], Order[t3], Order[t4], Foldable[f] =
        injectIntoX(match (v1, v2, v3, v4) ->
            box(v1) ::
            box(v2) ::
            box(v3) ::
            box(v4) ::
            Nil
        , p, ts)

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4, t5). p(t1', t2', t3', t4', t5').}`
    /// given the sequence `(t1, t2, t3, t4, t5), (t1', t2', t3', t4', t5')`.
    ///
    @Internal
    pub def injectInto5(p: PredSym, ts: f[(t1, t2, t3, t4, t5)]):
        Datalog[Boxed] \ Foldable.Aef[f] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Foldable[f] =
        injectIntoX(match (v1, v2, v3, v4, v5) ->
            box(v1) ::
            box(v2) ::
            box(v3) ::
            box(v4) ::
            box(v5) ::
            Nil
        , p, ts)

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4, t5, t6). p(t1', t2', t3', t4', t5', t6').}`
    /// given the sequence `(t1, t2, t3, t4, t5, t6), (t1', t2', t3', t4', t5', t6')`.
    ///
    @Internal
    pub def injectInto6(p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6)]):
        Datalog[Boxed] \ Foldable.Aef[f] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Foldable[f] =
        injectIntoX(match (v1, v2, v3, v4, v5, v6) ->
            box(v1) ::
            box(v2) ::
            box(v3) ::
            box(v4) ::
            box(v5) ::
            box(v6) ::
            Nil
        , p, ts)

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4, t5, t6, t7). p(t1', t2', t3', t4', t5', t6', t7').}`
    /// given the sequence `(t1, t2, t3, t4, t5, t6, t7), (t1', t2', t3', t4', t5', t6', t7')`.
    ///
    @Internal
    pub def injectInto7(p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7)]):
        Datalog[Boxed] \ Foldable.Aef[f] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Foldable[f] =
        injectIntoX(match (v1, v2, v3, v4, v5, v6, v7) ->
            box(v1) ::
            box(v2) ::
            box(v3) ::
            box(v4) ::
            box(v5) ::
            box(v6) ::
            box(v7) ::
            Nil
        , p, ts)

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4, t5, t6, t7, t8). p(t1', t2', t3', t4', t5', t6', t7', t8').}`
    /// given the sequence
    /// `(t1, t2, t3, t4, t5, t6, t7, t8), (t1', t2', t3', t4', t5', t6', t7', t8')`.
    ///
    @Internal
    pub def injectInto8(p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8)]):
        Datalog[Boxed] \ Foldable.Aef[f] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Foldable[f] =
        injectIntoX(match (v1, v2, v3, v4, v5, v6, v7, v8) ->
            box(v1) ::
            box(v2) ::
            box(v3) ::
            box(v4) ::
            box(v5) ::
            box(v6) ::
            box(v7) ::
            box(v8) ::
            Nil
        , p, ts)

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4, t5, t6, t7, t8, t9). p(t1', t2', t3', t4', t5', t6', t7', t8', t9').}`
    /// given the sequence
    /// `(t1, t2, t3, t4, t5, t6, t7, t8, t9), (t1', t2', t3', t4', t5', t6', t7', t8', t9')`.
    ///
    @Internal
    pub def injectInto9(p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8, t9)]):
        Datalog[Boxed] \ Foldable.Aef[f] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9], Foldable[f] =
        injectIntoX(match (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
            box(v1) ::
            box(v2) ::
            box(v3) ::
            box(v4) ::
            box(v5) ::
            box(v6) ::
            box(v7) ::
            box(v8) ::
            box(v9) ::
            Nil
        , p, ts)

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10).
    ///    p(t1', t2', t3', t4', t5', t6', t7', t8', t9', t10').}`
    /// given the sequence
    /// `(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10),
    ///  (t1', t2', t3', t4', t5', t6', t7', t8', t9', t10')`.
    ///
    @Internal
    pub def injectInto10(p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)]):
        Datalog[Boxed] \ Foldable.Aef[f] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9], Order[t10], Foldable[f] =
        injectIntoX(match (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) ->
            box(v1) ::
            box(v2) ::
            box(v3) ::
            box(v4) ::
            box(v5) ::
            box(v6) ::
            box(v7) ::
            box(v8) ::
            box(v9) ::
            box(v10) ::
            Nil
        , p, ts)

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11).
    ///    p(t1', t2', t3', t4', t5', t6', t7', t8', t9', t10', t11').}`
    /// given the sequence
    /// `(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11),
    ///  (t1', t2', t3', t4', t5', t6', t7', t8', t9', t10', t11')`.
    ///
    @Internal
    pub def injectInto11(p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11)]):
        Datalog[Boxed] \ Foldable.Aef[f] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9], Order[t10], Order[t11], Foldable[f] =
        injectIntoX(match (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11) ->
            box(v1) ::
            box(v2) ::
            box(v3) ::
            box(v4) ::
            box(v5) ::
            box(v6) ::
            box(v7) ::
            box(v8) ::
            box(v9) ::
            box(v10) ::
            box(v11) ::
            Nil
        , p, ts)

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12).
    ///    p(t1', t2', t3', t4', t5', t6', t7', t8', t9', t10', t11', t12').}`
    /// given the sequence
    /// `(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12),
    ///  (t1', t2', t3', t4', t5', t6', t7', t8', t9', t10', t11', t12')`.
    ///
    @Internal
    pub def injectInto12(p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12)]):
        Datalog[Boxed] \ Foldable.Aef[f] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9], Order[t10], Order[t11], Order[t12], Foldable[f] =
        injectIntoX(match (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12) ->
            box(v1) ::
            box(v2) ::
            box(v3) ::
            box(v4) ::
            box(v5) ::
            box(v6) ::
            box(v7) ::
            box(v8) ::
            box(v9) ::
            box(v10) ::
            box(v11) ::
            box(v12) ::
            Nil
        , p, ts)

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13).
    ///    p(t1', t2', t3', t4', t5', t6', t7', t8', t9', t10', t11', t12', t13').}`
    /// given the sequence
    /// `(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13),
    ///  (t1', t2', t3', t4', t5', t6', t7', t8', t9', t10', t11', t12', t13')`.
    ///
    @Internal
    pub def injectInto13(p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13)]):
        Datalog[Boxed] \ Foldable.Aef[f] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9], Order[t10], Order[t11], Order[t12], Order[t13], Foldable[f] =
        injectIntoX(match (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13) ->
            box(v1) ::
            box(v2) ::
            box(v3) ::
            box(v4) ::
            box(v5) ::
            box(v6) ::
            box(v7) ::
            box(v8) ::
            box(v9) ::
            box(v10) ::
            box(v11) ::
            box(v12) ::
            box(v13) ::
            Nil
        , p, ts)

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14).
    ///    p(t1', t2', t3', t4', t5', t6', t7', t8', t9', t10', t11', t12', t13', t14').}`
    /// given the sequence
    /// `(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14),
    ///  (t1', t2', t3', t4', t5', t6', t7', t8', t9', t10', t11', t12', t13', t14')`.
    ///
    @Internal
    pub def injectInto14(p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14)]):
        Datalog[Boxed] \ Foldable.Aef[f] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9], Order[t10], Order[t11], Order[t12], Order[t13], Order[t14], Foldable[f] =
        injectIntoX(match (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14) ->
            box(v1) ::
            box(v2) ::
            box(v3) ::
            box(v4) ::
            box(v5) ::
            box(v6) ::
            box(v7) ::
            box(v8) ::
            box(v9) ::
            box(v10) ::
            box(v11) ::
            box(v12) ::
            box(v13) ::
            box(v14) ::
            Nil
        , p, ts)

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15).
    ///    p(t1', t2', t3', t4', t5', t6', t7', t8', t9', t10', t11', t12', t13', t14', t15').}`
    /// given the sequence
    /// `(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15),
    ///  (t1', t2', t3', t4', t5', t6', t7', t8', t9', t10', t11', t12', t13', t14', t15')`.
    ///
    @Internal
    pub def injectInto15(p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15)]):
        Datalog[Boxed] \ Foldable.Aef[f] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9], Order[t10], Order[t11], Order[t12], Order[t13], Order[t14], Order[t15], Foldable[f] =
        injectIntoX(match (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15) ->
            box(v1) ::
            box(v2) ::
            box(v3) ::
            box(v4) ::
            box(v5) ::
            box(v6) ::
            box(v7) ::
            box(v8) ::
            box(v9) ::
            box(v10) ::
            box(v11) ::
            box(v12) ::
            box(v13) ::
            box(v14) ::
            box(v15) ::
            Nil
        , p, ts)

    def injectIntoX(f: t -> List[Boxed] \ ef, p: PredSym, ts: f[t]): Datalog[Boxed] \ (ef + Foldable.Aef[f]) with Foldable[f] =
        region rc {
            let db = MutMap.empty(rc);
            Foldable.foldLeft(() -> t -> {
                let vs = f(t);
                let arity = List.length(vs);
                let ramSym = RamSym.Full(p, arity, Denotation.Relational);
                let rel = MutMap.getOrElsePut!(ramSym, MutMap.empty(rc), db);
                MutMap.put!(List.toVector(vs), Reflect.default(), rel)
            }, (), ts);
            toModel(db)
        }

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts0(p: PredSym, d: Datalog[Boxed]): Vector[v] with Order[v] =
        // Unsafe cast is required. If a predicate is nullary then its values must be Unit.
        factsOf(_ -> unchecked_cast(() as v), p, d)

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts1(p: PredSym, d: Datalog[Boxed]): Vector[v] with Order[v] =
        let f = terms -> match terms {
            case hd :: _ =>
                unbox(hd)
            case _ => unreachable!()
        };
       factsOf(f, p, d)

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts2(p: PredSym, d: Datalog[Boxed]): Vector[(t1, t2)] with Order[t1], Order[t2] =
        let f = terms -> match terms {
            case v0 :: v1 :: _ =>
                (unbox(v0), unbox(v1))
            case _ => unreachable!()
        };
        factsOf(f, p, d)

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts3(p: PredSym, d: Datalog[Boxed]): Vector[(t1, t2, t3)] with Order[t1], Order[t2], Order[t3] =
        let f = terms -> match terms {
            case v0 :: v1 :: v2 :: _ =>
                (unbox(v0), unbox(v1), unbox(v2))
            case _ => unreachable!()
        };
        factsOf(f, p, d)

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts4(p: PredSym, d: Datalog[Boxed]): Vector[(t1, t2, t3, t4)] with Order[t1], Order[t2], Order[t3], Order[t4] =
        let f = terms -> match terms {
            case v0 :: v1 :: v2 :: v3 :: _ =>
                (unbox(v0), unbox(v1), unbox(v2), unbox(v3))
            case _ => unreachable!()
        };
        factsOf(f, p, d)

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts5(p: PredSym, d: Datalog[Boxed]): Vector[(t1, t2, t3, t4, t5)] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5] =
        let f = terms -> match terms {
            case v0 :: v1 :: v2 :: v3 :: v4 :: _ =>
                (unbox(v0), unbox(v1), unbox(v2), unbox(v3), unbox(v4))
            case _ => unreachable!()
        };
        factsOf(f, p, d)

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts6(p: PredSym, d: Datalog[Boxed]): Vector[(t1, t2, t3, t4, t5, t6)] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6] =
        let f = terms -> match terms {
            case v0 :: v1 :: v2 :: v3 :: v4 :: v5 :: _ =>
                (unbox(v0), unbox(v1), unbox(v2), unbox(v3), unbox(v4), unbox(v5))
            case _ => unreachable!()
        };
        factsOf(f, p, d)

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts7(p: PredSym, d: Datalog[Boxed]): Vector[(t1, t2, t3, t4, t5, t6, t7)] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7] =
        let f = terms -> match terms {
            case v0 :: v1 :: v2 :: v3 :: v4 :: v5 :: v6 :: _ =>
                (unbox(v0), unbox(v1), unbox(v2), unbox(v3), unbox(v4), unbox(v5), unbox(v6))
            case _ => unreachable!()
        };
        factsOf(f, p, d)

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts8(p: PredSym, d: Datalog[Boxed]): Vector[(t1, t2, t3, t4, t5, t6, t7, t8)] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8] =
        let f = terms -> match terms {
            case v0 :: v1 :: v2 :: v3 :: v4 :: v5 :: v6 :: v7 :: _ =>
                (unbox(v0), unbox(v1), unbox(v2), unbox(v3), unbox(v4), unbox(v5), unbox(v6), unbox(v7))
            case _ => unreachable!()
        };
        factsOf(f, p, d)

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts9(p: PredSym, d: Datalog[Boxed]): Vector[(t1, t2, t3, t4, t5, t6, t7, t8, t9)] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9] =
        let f = terms -> match terms {
            case v0 :: v1 :: v2 :: v3 :: v4 :: v5 :: v6 :: v7 :: v8 :: _ =>
                (unbox(v0), unbox(v1), unbox(v2), unbox(v3), unbox(v4), unbox(v5), unbox(v6), unbox(v7), unbox(v8))
            case _ => unreachable!()
        };
        factsOf(f, p, d)

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts10(p: PredSym, d: Datalog[Boxed]): Vector[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9], Order[t10] =
        let f = terms -> match terms {
            case v0 :: v1 :: v2 :: v3 :: v4 :: v5 :: v6 :: v7 :: v8 :: v9 :: _ =>
                (unbox(v0), unbox(v1), unbox(v2), unbox(v3), unbox(v4), unbox(v5), unbox(v6), unbox(v7), unbox(v8), unbox(v9))
            case _ => unreachable!()
        };
        factsOf(f, p, d)

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts11(p: PredSym, d: Datalog[Boxed]): Vector[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11)] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9], Order[t10], Order[t11] =
        let f = terms -> match terms {
            case v0 :: v1 :: v2 :: v3 :: v4 :: v5 :: v6 :: v7 :: v8 :: v9 :: v10 :: _ =>
                (unbox(v0), unbox(v1), unbox(v2), unbox(v3), unbox(v4), unbox(v5), unbox(v6), unbox(v7), unbox(v8), unbox(v9), unbox(v10))
            case _ => unreachable!()
        };
        factsOf(f, p, d)

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts12(p: PredSym, d: Datalog[Boxed]): Vector[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12)] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9], Order[t10], Order[t11], Order[t12] =
        let f = terms -> match terms {
            case v0 :: v1 :: v2 :: v3 :: v4 :: v5 :: v6 :: v7 :: v8 :: v9 :: v10 :: v11 :: _ =>
                (unbox(v0), unbox(v1), unbox(v2), unbox(v3), unbox(v4), unbox(v5), unbox(v6), unbox(v7), unbox(v8), unbox(v9), unbox(v10), unbox(v11))
            case _ => unreachable!()
        };
        factsOf(f, p, d)

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts13(p: PredSym, d: Datalog[Boxed]): Vector[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13)] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9], Order[t10], Order[t11], Order[t12], Order[t13] =
        let f = terms -> match terms {
            case v0 :: v1 :: v2 :: v3 :: v4 :: v5 :: v6 :: v7 :: v8 :: v9 :: v10 :: v11 :: v12 :: _ =>
                (unbox(v0), unbox(v1), unbox(v2), unbox(v3), unbox(v4), unbox(v5), unbox(v6), unbox(v7), unbox(v8), unbox(v9), unbox(v10), unbox(v11), unbox(v12))
            case _ => unreachable!()
        };
        factsOf(f, p, d)

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts14(p: PredSym, d: Datalog[Boxed]): Vector[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14)] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9], Order[t10], Order[t11], Order[t12], Order[t13], Order[t14] =
        let f = terms -> match terms {
            case v0 :: v1 :: v2 :: v3 :: v4 :: v5 :: v6 :: v7 :: v8 :: v9 :: v10 :: v11 :: v12 :: v13 :: _ =>
                (unbox(v0), unbox(v1), unbox(v2), unbox(v3), unbox(v4), unbox(v5), unbox(v6), unbox(v7), unbox(v8), unbox(v9), unbox(v10), unbox(v11), unbox(v12), unbox(v13))
            case _ => unreachable!()
        };
        factsOf(f, p, d)

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts15(p: PredSym, d: Datalog[Boxed]): Vector[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15)] with Order[t1], Order[t2], Order[t3], Order[t4], Order[t5], Order[t6], Order[t7], Order[t8], Order[t9], Order[t10], Order[t11], Order[t12], Order[t13], Order[t14], Order[t15] =
        let f = terms -> match terms {
            case v0 :: v1 :: v2 :: v3 :: v4 :: v5 :: v6 :: v7 :: v8 :: v9 :: v10 :: v11 :: v12 :: v13 :: v14 :: _ =>
                (unbox(v0), unbox(v1), unbox(v2), unbox(v3), unbox(v4), unbox(v5), unbox(v6), unbox(v7), unbox(v8), unbox(v9), unbox(v10), unbox(v11), unbox(v12), unbox(v13), unbox(v14))
            case _ => unreachable!()
        };
        factsOf(f, p, d)

    ///
    /// Returns an array of facts associated with the given predicate symbol `p` in the given Datalog program `d`.
    ///
    def factsOf(f: List[v] -> t \ ef, p: PredSym, d: Datalog[v]): Vector[t] \ ef = match d {
        case Datalog(_, cs) => region rc {
            let pFacts = MutList.empty(rc);
            Vector.forEach(c -> match c {
                case Constraint(HeadAtom(headPred, _, terms), body) =>
                    if (headPred == p and Vector.isEmpty(body))
                        let vs = Vector.map(headTermValue, terms);
                        MutList.push!(f(Vector.toList(vs)), pFacts)
                    else ()
                case _ => ()
            }, cs);
            pFacts |> MutList.toVector
        }
        case Model(db) => region rc {
            use Fixpoint.Ast.Ram.toDenotation;
            let pFacts = MutList.empty(rc);
            let qquery = ramSym -> match ramSym {
                case RamSym.Full(predSym, _, _) => match predSym <=> p {
                    case Comparison.EqualTo => Comparison.EqualTo
                    case cmp => cmp
                }
                case _ => Comparison.LessThan
            };
            Map.rangeQueryWith(qquery, ramSym -> rel -> match toDenotation(ramSym) {
                case Denotation.Relational =>
                    Map.forEach((tuple, _) -> MutList.push!(f(Vector.toList(tuple)), pFacts), rel)
                case Denotation.Latticenal(_) =>
                    Map.forEach((tuple, lat) -> {
                        let it = Iterator.range(rc, 0, Vector.length(tuple) + 1);
                        let vs = it |> Iterator.map(i -> {
                            if (i < Vector.length(tuple))
                                Vector.get(i, tuple)
                            else
                                lat
                        }) |> Iterator.toList;
                        MutList.push!(f(vs), pFacts)
                    }, rel)
            }, db);
            pFacts |> MutList.toVector
        }
        case Join(d1, d2) => Vector.append(factsOf(f, p, d1), factsOf(f, p, d2))
    }

    ///
    /// Returns the value of the given head term `v`.
    ///
    /// Note the term must have been fully evaluated to a literal value.
    ///
    def headTermValue(t: HeadTerm[v]): v = match t {
        case HeadTerm.Lit(v) => v
        case _               => bug!("Unexpected non-value term.")
    }

    ///
    /// Returns the given database `db` as a Datalog value.
    ///
    def toModel(db: Database[v, r]): Datalog[v] \ r =
        MutMap.toMap(db) |>
        Map.map(MutMap.toMap) |>
        Model
}

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

use Boxable.{box, unbox};
use Fixpoint/Ast.{Datalog, Constraint, HeadTerm, Denotation};
use Fixpoint/Ast.Datalog.{Datalog, Model, Join};
use Fixpoint/Ast.Constraint.Constraint;
use Fixpoint/Ast.HeadPredicate.HeadAtom;
use Fixpoint/Ast.Denotation.{Relational, Latticenal};
use Fixpoint/Ram.RamSym;
use Fixpoint/Shared.PredSym;
use Fixpoint/Shared.PredSym.PredSym;

use Fixpoint.Tuple.Tuple;

namespace Fixpoint {

    ///
    /// Returns the minimal model of the given Datalog program `d`.
    /// The minimal model is given by the model semantics for Datalog programs with stratified negation.
    /// The stratification is computed automatically by the solver.
    ///
    @Internal
    pub def solve(d: Datalog[v]): Datalog[v] with Order[v], ToString[v] =
        (stratify(d) |>
        solveWithStratification(d)) as & Pure

    ///
    /// Returns the minimal model of the given Datalog program `d`.
    /// The minimal model is given by the model semantics for Datalog programs with stratified negation.
    /// A stratification of `d` is given by `stf`.
    ///
    @Internal
    def solveWithStratification(d: Datalog[v], stf: Map[PredSym, Int32]): Datalog[v] & Impure with Order[v], ToString[v] =
        notifyPreSolve(d, stf);
        let compiler = cs ->
            compile(cs, stf) |>
            simplifyStmt |>
            queryStmt |>
            lowerStmt;
        let model = match d {
            case Datalog(_) => compiler(d) |> interpret |> toModel
            case Model(_) => d
            case Join(Model(m), cs) =>
                let db = Static |> (Map.map(mm -> Map.toMutMap(mm, Static), m) |> Map.toMutMap);
                compiler(cs) |> interpretWithDatabase(db) |> toModel
            case _ => bug!("Datalog normalization bug")
        };
        notifyPostSolve(model);
        model

    ///
    /// Returns the pairwise union of `d1` and `d2`.
    /// I.e. the facts of the union is the union of the facts and likewise for rules.
    /// A fact or rule may occur twice in the Datalog program. This has no effect on its semantics.
    ///
    @Internal
    pub def union(d1: Datalog[v], d2: Datalog[v]): Datalog[v] with Order[v] = match (d1, d2) {
        case (Datalog(edb1, idb1), Datalog(edb2, idb2)) =>
            Datalog(Array.append(edb1, edb2), Array.append(idb1, idb2))
        case (Model(db1), Model(db2)) =>
            use Fixpoint/Ram.toDenotation;
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
    } as & Pure

    ///
    /// Project all facts in the given Datalog program `d` that has `p` in its head.
    ///
    @Internal
    pub def project(p: PredSym, d: Datalog[v]): Datalog[v] with Order[v] = match d {
        case Datalog(facts, _) =>
            let pFacts = Array.filter(match Constraint(HeadAtom(head, _, _), _) -> head == p, facts);
            Datalog(pFacts, [])
        case Model(db) =>
            Map.query(ramSym -> match ramSym {
                case RamSym.Full(predSym, _, _) => match predSym <=> p {
                    case EqualTo => EqualTo
                    case cmp => cmp
                }
                case _ => LessThan
            }, db) |>
            List.toMap |>
            Model
        case Join(d1, d2) => union(project(p, d1), project(p, d2))
    } as & Pure

    ///
    /// Renames every predicate symbol in `d` with a fresh name, except for those in `preds`.
    ///
    @Internal
    pub def rename(preds: Array[PredSym, Static], d: Datalog[v]): Datalog[v] with Order[v], ToString[v] = {
        use Fixpoint/PredSymsOf.predSymsOf;
        use Fixpoint/SubstitutePredSym.substitute;

        // Import a function that can create new ids.
        import static dev.flix.runtime.Global.newId(): Int64 & Impure;

        /// Determine all the predicate symbols in `d`.
        let predSyms = predSymsOf(d);

        /// Compute a substitition that assignes fresh names to all predicates except those in `preds`.
        def f(macc, predSym) = {
            if (Array.memberOf(predSym, preds)) {
                // Case 1: Do not rename. Do not extend the map.
                macc
            } else {
                // Case 2: Introduce a fresh name (index) for the prediate symbol.
                let PredSym(name, _) = predSym;
                let newPredSym = PredSym(name, newId() + 1i64); // We add one because the counter starts at zero.
                Map.insert(predSym, newPredSym, macc)
            }
        };
        let s = Set.foldLeft(f, Map.empty(), predSyms);

        // Apply the computed substitution to the Datalog program `d`.
        let r = substitute(d, s);

        // Debugging
        if (enableDebugging()) {
            println(s);
            println(d);
            println(r)
        } else ();

        // Return the result.
        r
    } as & Pure

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to `#{p(t1). p(t1').}`
    /// given the sequence `(t1), (t1')`.
    ///
    @Internal
    pub def injectInto1[f: Type -> Type, t1: Type](p: PredSym, ts: f[(t1)]):
        Datalog[Boxed] with Boxable[t1], Foldable[f] =
        injectIntoX(match (v1) -> [box(v1)], p, ts) as & Pure

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to `#{p(t1, t2). p(t1', t2').}`
    /// given the sequence `(t1, t2), (t1', t2')`.
    ///
    @Internal
    pub def injectInto2[f: Type -> Type, t1: Type, t2: Type](p: PredSym, ts: f[(t1, t2)]):
        Datalog[Boxed] with Boxable[t1], Boxable[t2], Foldable[f] =
        injectIntoX(match (v1, v2) ->
            [box(v1),
             box(v2)]
        , p, ts) as & Pure

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to `#{p(t1, t2, t3). p(t1', t2', t3').}`
    /// given the sequence `(t1, t2, t3), (t1', t2', t3')`.
    ///
    @Internal
    pub def injectInto3[f: Type -> Type, t1: Type, t2: Type, t3: Type](p: PredSym, ts: f[(t1, t2, t3)]):
        Datalog[Boxed] with Boxable[t1], Boxable[t2], Boxable[t3], Foldable[f] =
        injectIntoX(match (v1, v2, v3) ->
            [box(v1),
             box(v2),
             box(v3)]
        , p, ts) as & Pure

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4). p(t1', t2', t3', t4').}`
    /// given the sequence `(t1, t2, t3, t4), (t1', t2', t3', t4')`.
    ///
    @Internal
    pub def injectInto4[f: Type -> Type, t1: Type, t2: Type, t3: Type, t4: Type](p: PredSym, ts: f[(t1, t2, t3, t4)]):
        Datalog[Boxed] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Foldable[f] =
        injectIntoX(match (v1, v2, v3, v4) ->
            [box(v1),
             box(v2),
             box(v3),
             box(v4)]
        , p, ts) as & Pure

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4, t5). p(t1', t2', t3', t4', t5').}`
    /// given the sequence `(t1, t2, t3, t4, t5), (t1', t2', t3', t4', t5')`.
    ///
    @Internal
    pub def injectInto5[f: Type -> Type, t1: Type, t2: Type, t3: Type, t4: Type, t5: Type](p: PredSym, ts: f[(t1, t2, t3, t4, t5)]):
        Datalog[Boxed] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Foldable[f] =
        injectIntoX(match (v1, v2, v3, v4, v5) ->
            [box(v1),
             box(v2),
             box(v3),
             box(v4),
             box(v5)]
        , p, ts) as & Pure

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4, t5, t6). p(t1', t2', t3', t4', t5', t6').}`
    /// given the sequence `(t1, t2, t3, t4, t5, t6), (t1', t2', t3', t4', t5', t6')`.
    ///
    @Internal
    pub def injectInto6[f: Type -> Type, t1: Type, t2: Type, t3: Type, t4: Type, t5: Type, t6: Type](p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6)]):
        Datalog[Boxed] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6], Foldable[f] =
        injectIntoX(match (v1, v2, v3, v4, v5, v6) ->
            [box(v1),
             box(v2),
             box(v3),
             box(v4),
             box(v5),
             box(v6)]
        , p, ts) as & Pure

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4, t5, t6, t7). p(t1', t2', t3', t4', t5', t6', t7').}`
    /// given the sequence `(t1, t2, t3, t4, t5, t6, t7), (t1', t2', t3', t4', t5', t6', t7')`.
    ///
    @Internal
    pub def injectInto7[f: Type -> Type, t1: Type, t2: Type, t3: Type, t4: Type, t5: Type, t6: Type, t7: Type](p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7)]):
        Datalog[Boxed] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6], Boxable[t7], Foldable[f] =
        injectIntoX(match (v1, v2, v3, v4, v5, v6, v7) ->
            [box(v1),
             box(v2),
             box(v3),
             box(v4),
             box(v5),
             box(v6),
             box(v7)]
        , p, ts) as & Pure

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4, t5, t6, t7, t8). p(t1', t2', t3', t4', t5', t6', t7', t8').}`
    /// given the sequence
    /// `(t1, t2, t3, t4, t5, t6, t7, t8), (t1', t2', t3', t4', t5', t6', t7', t8')`.
    ///
    @Internal
    pub def injectInto8[f: Type -> Type, t1: Type, t2: Type, t3: Type, t4: Type, t5: Type, t6: Type, t7: Type, t8: Type](p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8)]):
        Datalog[Boxed] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6], Boxable[t7], Boxable[t8], Foldable[f] =
        injectIntoX(match (v1, v2, v3, v4, v5, v6, v7, v8) ->
            [box(v1),
             box(v2),
             box(v3),
             box(v4),
             box(v5),
             box(v6),
             box(v7),
             box(v8)]
        , p, ts) as & Pure

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4, t5, t6, t7, t8, t9). p(t1', t2', t3', t4', t5', t6', t7', t8', t9').}`
    /// given the sequence
    /// `(t1, t2, t3, t4, t5, t6, t7, t8, t9), (t1', t2', t3', t4', t5', t6', t7', t8', t9')`.
    ///
    @Internal
    pub def injectInto9[f: Type -> Type, t1: Type, t2: Type, t3: Type, t4: Type, t5: Type, t6: Type, t7: Type, t8: Type, t9: Type](p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8, t9)]):
        Datalog[Boxed] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6], Boxable[t7], Boxable[t8], Boxable[t9], Foldable[f] =
        injectIntoX(match (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
            [box(v1),
             box(v2),
             box(v3),
             box(v4),
             box(v5),
             box(v6),
             box(v7),
             box(v8),
             box(v9)]
        , p, ts) as & Pure

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
    pub def injectInto10[f: Type -> Type, t1: Type, t2: Type, t3: Type, t4: Type, t5: Type, t6: Type, t7: Type, t8: Type, t9: Type, t10: Type](p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)]):
        Datalog[Boxed] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6], Boxable[t7], Boxable[t8], Boxable[t9], Boxable[t10], Foldable[f] =
        injectIntoX(match (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) ->
            [box(v1),
             box(v2),
             box(v3),
             box(v4),
             box(v5),
             box(v6),
             box(v7),
             box(v8),
             box(v9),
             box(v10)]
        , p, ts) as & Pure

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
    pub def injectInto11[f: Type -> Type, t1: Type, t2: Type, t3: Type, t4: Type, t5: Type, t6: Type, t7: Type, t8: Type, t9: Type, t10: Type, t11: Type](p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11)]):
        Datalog[Boxed] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6], Boxable[t7], Boxable[t8], Boxable[t9], Boxable[t10], Boxable[t11], Foldable[f] =
        injectIntoX(match (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11) ->
            [box(v1),
             box(v2),
             box(v3),
             box(v4),
             box(v5),
             box(v6),
             box(v7),
             box(v8),
             box(v9),
             box(v10),
             box(v11)]
        , p, ts) as & Pure

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
    pub def injectInto12[f: Type -> Type, t1: Type, t2: Type, t3: Type, t4: Type, t5: Type, t6: Type, t7: Type, t8: Type, t9: Type, t10: Type, t11: Type, t12: Type](p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12)]):
        Datalog[Boxed] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6], Boxable[t7], Boxable[t8], Boxable[t9], Boxable[t10], Boxable[t11], Boxable[t12], Foldable[f] =
        injectIntoX(match (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12) ->
            [box(v1),
             box(v2),
             box(v3),
             box(v4),
             box(v5),
             box(v6),
             box(v7),
             box(v8),
             box(v9),
             box(v10),
             box(v11),
             box(v12)]
        , p, ts) as & Pure

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
    pub def injectInto13[f: Type -> Type, t1: Type, t2: Type, t3: Type, t4: Type, t5: Type, t6: Type, t7: Type, t8: Type, t9: Type, t10: Type, t11: Type, t12: Type, t13: Type](p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13)]):
        Datalog[Boxed] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6], Boxable[t7], Boxable[t8], Boxable[t9], Boxable[t10], Boxable[t11], Boxable[t12], Boxable[t13], Foldable[f] =
        injectIntoX(match (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13) ->
            [box(v1),
             box(v2),
             box(v3),
             box(v4),
             box(v5),
             box(v6),
             box(v7),
             box(v8),
             box(v9),
             box(v10),
             box(v11),
             box(v12),
             box(v13)]
        , p, ts) as & Pure

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
    pub def injectInto14[f: Type -> Type, t1: Type, t2: Type, t3: Type, t4: Type, t5: Type, t6: Type, t7: Type, t8: Type, t9: Type, t10: Type, t11: Type, t12: Type, t13: Type, t14: Type](p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14)]):
        Datalog[Boxed] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6], Boxable[t7], Boxable[t8], Boxable[t9], Boxable[t10], Boxable[t11], Boxable[t12], Boxable[t13], Boxable[t14], Foldable[f] =
        injectIntoX(match (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14) ->
            [box(v1),
             box(v2),
             box(v3),
             box(v4),
             box(v5),
             box(v6),
             box(v7),
             box(v8),
             box(v9),
             box(v10),
             box(v11),
             box(v12),
             box(v13),
             box(v14)]
        , p, ts) as & Pure

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
    pub def injectInto15[f: Type -> Type, t1: Type, t2: Type, t3: Type, t4: Type, t5: Type, t6: Type, t7: Type, t8: Type, t9: Type, t10: Type, t11: Type, t12: Type, t13: Type, t14: Type, t15: Type](p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15)]):
        Datalog[Boxed] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6], Boxable[t7], Boxable[t8], Boxable[t9], Boxable[t10], Boxable[t11], Boxable[t12], Boxable[t13], Boxable[t14], Boxable[t15], Foldable[f] =
        injectIntoX(match (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15) ->
            [box(v1),
             box(v2),
             box(v3),
             box(v4),
             box(v5),
             box(v6),
             box(v7),
             box(v8),
             box(v9),
             box(v10),
             box(v11),
             box(v12),
             box(v13),
             box(v14),
             box(v15)]
        , p, ts) as & Pure

    def injectIntoX[f: Type -> Type, t: Type, ef: Bool](f: t -> Array[Boxed, Static] & ef, p: PredSym, ts: f[t]): Datalog[Boxed] & Impure with Foldable[f] =
        let db = new MutMap(Static);
        Foldable.foldLeft(() -> t -> {
            let vs = f(t);
            let arity = Array.length(vs);
            let ramSym = RamSym.Full(p, arity, Denotation.Relational);
            let rel = MutMap.getOrElsePut!(ramSym, new MutMap(Static), db);
            MutMap.put!(Tuple(vs), $DEFAULT$, rel)
        }, (), ts);
        toModel(db)

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts0(p: PredSym, d: Datalog[Boxed]): Array[v, Static] with Boxable[v] =
        factsOf(_ -> () as v, p, d) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts1(p: PredSym, d: Datalog[Boxed]): Array[v, Static] with Boxable[v] =
        let f = terms -> unbox(terms[0]);
        factsOf(f, p, d) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts2(p: PredSym, d: Datalog[Boxed]): Array[(t1, t2), Static] with Boxable[t1], Boxable[t2] =
        let f = terms -> (unbox(terms[0]), unbox(terms[1]));
        factsOf(f, p, d) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts3(p: PredSym, d: Datalog[Boxed]): Array[(t1, t2, t3), Static] with Boxable[t1], Boxable[t2], Boxable[t3] =
        let f = terms -> (unbox(terms[0]), unbox(terms[1]), unbox(terms[2]));
        factsOf(f, p, d) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts4(p: PredSym, d: Datalog[Boxed]): Array[(t1, t2, t3, t4), Static] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4] =
        let f = terms -> (unbox(terms[0]), unbox(terms[1]), unbox(terms[2]), unbox(terms[3]));
        factsOf(f, p, d) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts5(p: PredSym, d: Datalog[Boxed]): Array[(t1, t2, t3, t4, t5), Static] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5] =
        let f = terms -> (unbox(terms[0]), unbox(terms[1]), unbox(terms[2]), unbox(terms[3]), unbox(terms[4]));
        factsOf(f, p, d) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts6(p: PredSym, d: Datalog[Boxed]): Array[(t1, t2, t3, t4, t5, t6), Static] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6] =
        let f = terms -> (unbox(terms[0]), unbox(terms[1]), unbox(terms[2]), unbox(terms[3]), unbox(terms[4]), unbox(terms[5]));
        factsOf(f, p, d) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts7(p: PredSym, d: Datalog[Boxed]): Array[(t1, t2, t3, t4, t5, t6, t7), Static] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6], Boxable[t7] =
        let f = terms -> (unbox(terms[0]), unbox(terms[1]), unbox(terms[2]), unbox(terms[3]), unbox(terms[4]), unbox(terms[5]), unbox(terms[6]));
        factsOf(f, p, d) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts8(p: PredSym, d: Datalog[Boxed]): Array[(t1, t2, t3, t4, t5, t6, t7, t8), Static] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6], Boxable[t7], Boxable[t8] =
        let f = terms -> (unbox(terms[0]), unbox(terms[1]), unbox(terms[2]), unbox(terms[3]), unbox(terms[4]), unbox(terms[5]), unbox(terms[6]), unbox(terms[7]));
        factsOf(f, p, d) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts9(p: PredSym, d: Datalog[Boxed]): Array[(t1, t2, t3, t4, t5, t6, t7, t8, t9), Static] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6], Boxable[t7], Boxable[t8], Boxable[t9] =
        let f = terms -> (unbox(terms[0]), unbox(terms[1]), unbox(terms[2]), unbox(terms[3]), unbox(terms[4]), unbox(terms[5]), unbox(terms[6]), unbox(terms[7]), unbox(terms[8]));
        factsOf(f, p, d) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts10(p: PredSym, d: Datalog[Boxed]): Array[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10), Static] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6], Boxable[t7], Boxable[t8], Boxable[t9], Boxable[t10] =
        let f = terms -> (unbox(terms[0]), unbox(terms[1]), unbox(terms[2]), unbox(terms[3]), unbox(terms[4]), unbox(terms[5]), unbox(terms[6]), unbox(terms[7]), unbox(terms[8]), unbox(terms[9]));
        factsOf(f, p, d) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts11(p: PredSym, d: Datalog[Boxed]): Array[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11), Static] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6], Boxable[t7], Boxable[t8], Boxable[t9], Boxable[t10], Boxable[t11] =
        let f = terms -> (unbox(terms[0]), unbox(terms[1]), unbox(terms[2]), unbox(terms[3]), unbox(terms[4]), unbox(terms[5]), unbox(terms[6]), unbox(terms[7]), unbox(terms[8]), unbox(terms[9]), unbox(terms[10]));
        factsOf(f, p, d) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts12(p: PredSym, d: Datalog[Boxed]): Array[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12), Static] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6], Boxable[t7], Boxable[t8], Boxable[t9], Boxable[t10], Boxable[t11], Boxable[t12] =
        let f = terms -> (unbox(terms[0]), unbox(terms[1]), unbox(terms[2]), unbox(terms[3]), unbox(terms[4]), unbox(terms[5]), unbox(terms[6]), unbox(terms[7]), unbox(terms[8]), unbox(terms[9]), unbox(terms[10]), unbox(terms[11]));
        factsOf(f, p, d) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts13(p: PredSym, d: Datalog[Boxed]): Array[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13), Static] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6], Boxable[t7], Boxable[t8], Boxable[t9], Boxable[t10], Boxable[t11], Boxable[t12], Boxable[t13] =
        let f = terms -> (unbox(terms[0]), unbox(terms[1]), unbox(terms[2]), unbox(terms[3]), unbox(terms[4]), unbox(terms[5]), unbox(terms[6]), unbox(terms[7]), unbox(terms[8]), unbox(terms[9]), unbox(terms[10]), unbox(terms[11]), unbox(terms[12]));
        factsOf(f, p, d) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts14(p: PredSym, d: Datalog[Boxed]): Array[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14), Static] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6], Boxable[t7], Boxable[t8], Boxable[t9], Boxable[t10], Boxable[t11], Boxable[t12], Boxable[t13], Boxable[t14] =
        let f = terms -> (unbox(terms[0]), unbox(terms[1]), unbox(terms[2]), unbox(terms[3]), unbox(terms[4]), unbox(terms[5]), unbox(terms[6]), unbox(terms[7]), unbox(terms[8]), unbox(terms[9]), unbox(terms[10]), unbox(terms[11]), unbox(terms[12]), unbox(terms[13]));
        factsOf(f, p, d) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    @Internal
    pub def facts15(p: PredSym, d: Datalog[Boxed]): Array[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15), Static] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6], Boxable[t7], Boxable[t8], Boxable[t9], Boxable[t10], Boxable[t11], Boxable[t12], Boxable[t13], Boxable[t14], Boxable[t15] =
        let f = terms -> (unbox(terms[0]), unbox(terms[1]), unbox(terms[2]), unbox(terms[3]), unbox(terms[4]), unbox(terms[5]), unbox(terms[6]), unbox(terms[7]), unbox(terms[8]), unbox(terms[9]), unbox(terms[10]), unbox(terms[11]), unbox(terms[12]), unbox(terms[13]), unbox(terms[14]));
        factsOf(f, p, d) as & Pure

    ///
    /// Returns an array of facts associated with the given predicate symbol `p` in the given Datalog program `d`.
    ///
    def factsOf(f: Array[v, Static] -> t & ef, p: PredSym, d: Datalog[v]): Array[t, Static] & Impure = match d {
        case Datalog(_, cs) =>
            let pFacts = new MutList(Static);
            Array.foreach(c -> match c {
                case Constraint(HeadAtom(headPred, _, terms), Nil) =>
                    if (headPred == p)
                        let vs = Array.map(headTermValue, terms);
                        MutList.push!(f(vs), pFacts)
                    else
                        ()
                case _ => ()
            }, cs);
            MutList.toArray(pFacts, Static)
        case Model(db) =>
            use Fixpoint/Ram.toDenotation;
            let pFacts = new MutList(Static);
            let query = ramSym -> match ramSym {
                case RamSym.Full(predSym, _, _) => match predSym <=> p {
                    case EqualTo => EqualTo
                    case cmp => cmp
                }
                case _ => LessThan
            };
            Map.queryWith(query, ramSym -> rel -> match toDenotation(ramSym) {
                case Denotation.Relational =>
                    Map.foreach(match Tuple(tuple) -> _ -> MutList.push!(f(tuple), pFacts), rel)
                case Denotation.Latticenal(_) =>
                    Map.foreach(match Tuple(tuple) -> lat -> {
                        let vs = Array.init(i -> {
                            if (i < Array.length(tuple))
                                tuple[i]
                            else
                                lat
                        }, Array.length(tuple) + 1, Static);
                        MutList.push!(f(vs), pFacts)
                    }, rel)
            }, db);
            MutList.toArray(pFacts, Static)
        case Join(d1, d2) => Array.append(factsOf(f, p, d1), factsOf(f, p, d2))
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
    /// Unsafely casts and prints the given Datalog program `d`.
    ///
    @Internal @Unsafe
    pub def unsafePrint(d: a): a =
        let _ = println(unsafeCast(d)) as & Pure;
        d

    ///
    /// Unsafely casts the given value `d` to a Datalog program.
    ///
    @Internal @Unsafe
    pub def unsafeCast(v: a): Datalog[Boxed] = v as Datalog[Boxed]

    def toModel(db: Database[v]): Datalog[v] & Impure =
        MutMap.toMap(db) |>
        Map.map(MutMap.toMap) |>
        Model
}

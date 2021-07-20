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
use Fixpoint/Ast.{Datalog, Constraint, HeadTerm, PredSym, Denotation};
use Fixpoint/Ast.Datalog.{Datalog, Model, Join};
use Fixpoint/Ast.Constraint.Constraint;
use Fixpoint/Ast.HeadPredicate.HeadAtom;
use Fixpoint/Ast.PredSym.PredSym;
use Fixpoint/Ast.Denotation.{Relational, Latticenal};
use Fixpoint/Ram.RamSym;

namespace Fixpoint {
    ///
    /// Returns the minimal model of the given Datalog program `d`.
    /// The minimal model is given by the model semantics for Datalog programs with stratified negation.
    /// The stratification is computed automatically by the solver.
    ///
    pub def solve(d: Datalog[v]): Datalog[v] with Order[v], ToString[v] =
        stratify(d) |>
        solveWithStratification(d)

    ///
    /// Returns the minimal model of the given Datalog program `d`.
    /// The minimal model is given by the model semantics for Datalog programs with stratified negation.
    /// A stratification of `d` is given by `stf`.
    ///
    pub def solveWithStratification(d: Datalog[v], stf: Map[PredSym, Int32]): Datalog[v] with Order[v], ToString[v] =
        let _ = notifyPreSolve(d, stf);
        let compiler = cs ->
            compile(cs, stf) |>
            simplifyStmt |>
            queryStmt |>
            lowerStmt;
        let model = match d {
            case Datalog(_) => compiler(d) |> interpret |> toModel
            case Model(_) => d
            case Join(Model(m), cs) =>
                let db = Map.map(Map.toMut, m) |> Map.toMut;
                compiler(cs) |> interpretWithDatabase(db) |> toModel
            case _ => bug!("Datalog normalization bug")
        } as & Pure;
        let _ = notifyPostSolve(model);
        model

    ///
    /// Returns the pairwise union of `d1` and `d2`.
    /// I.e. the facts of the union is the union of the facts and likewise for rules.
    /// A fact or rule may occur twice in the Datalog program. This has no effect on its semantics.
    ///
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
    pub def project(p: PredSym, d: Datalog[v]): Datalog[v] with Order[v] = match d {
        case Datalog(facts, _) =>
            let pFacts = Array.filter(match Constraint(HeadAtom(head, _, _), _) -> head == p, facts);
            Datalog(pFacts, [])
        case Model(db) =>
            let PredSym(key) = p;
            Map.query(ramSym -> match ramSym {
                case RamSym.Full(relName, _, _) => match relName <=> key {
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
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to `#{p(t1). p(t1').}`
    /// given the sequence `(t1), (t1')`.
    ///
    pub def projectInto1[f: Type -> Type, t1: Type](p: PredSym, ts: f[(t1)]):
        Datalog[Boxed] with Boxable[t1], Foldable[f] =
        projectIntoX(match (v1) -> [box(v1)] as & Pure, p, ts) as & Pure

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to `#{p(t1, t2). p(t1', t2').}`
    /// given the sequence `(t1, t2), (t1', t2')`.
    ///
    pub def projectInto2[f: Type -> Type, t1: Type, t2: Type](p: PredSym, ts: f[(t1, t2)]):
        Datalog[Boxed] with Boxable[t1], Boxable[t2], Foldable[f] =
        projectIntoX(match (v1, v2) ->
            [box(v1),
             box(v2)] as & Pure
        , p, ts) as & Pure

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to `#{p(t1, t2, t3). p(t1', t2', t3').}`
    /// given the sequence `(t1, t2, t3), (t1', t2', t3')`.
    ///
    pub def projectInto3[f: Type -> Type, t1: Type, t2: Type, t3: Type](p: PredSym, ts: f[(t1, t2, t3)]):
        Datalog[Boxed] with Boxable[t1], Boxable[t2], Boxable[t3], Foldable[f] =
        projectIntoX(match (v1, v2, v3) ->
            [box(v1),
             box(v2),
             box(v3)] as & Pure
        , p, ts) as & Pure

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4). p(t1', t2', t3', t4').}`
    /// given the sequence `(t1, t2, t3, t4), (t1', t2', t3', t4')`.
    ///
    pub def projectInto4[f: Type -> Type, t1: Type, t2: Type, t3: Type, t4: Type](p: PredSym, ts: f[(t1, t2, t3, t4)]):
        Datalog[Boxed] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Foldable[f] =
        projectIntoX(match (v1, v2, v3, v4) ->
            [box(v1),
             box(v2),
             box(v3),
             box(v4)] as & Pure
        , p, ts) as & Pure

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4, t5). p(t1', t2', t3', t4', t5').}`
    /// given the sequence `(t1, t2, t3, t4, t5), (t1', t2', t3', t4', t5')`.
    ///
    pub def projectInto5[f: Type -> Type, t1: Type, t2: Type, t3: Type, t4: Type, t5: Type](p: PredSym, ts: f[(t1, t2, t3, t4, t5)]):
        Datalog[Boxed] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Foldable[f] =
        projectIntoX(match (v1, v2, v3, v4, v5) ->
            [box(v1),
             box(v2),
             box(v3),
             box(v4),
             box(v5)] as & Pure
        , p, ts) as & Pure

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4, t5, t6). p(t1', t2', t3', t4', t5', t6').}`
    /// given the sequence `(t1, t2, t3, t4, t5, t6), (t1', t2', t3', t4', t5', t6')`.
    ///
    pub def projectInto6[f: Type -> Type, t1: Type, t2: Type, t3: Type, t4: Type, t5: Type, t6: Type](p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6)]):
        Datalog[Boxed] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6], Foldable[f] =
        projectIntoX(match (v1, v2, v3, v4, v5, v6) ->
            [box(v1),
             box(v2),
             box(v3),
             box(v4),
             box(v5),
             box(v6)] as & Pure
        , p, ts) as & Pure

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4, t5, t6, t7). p(t1', t2', t3', t4', t5', t6', t7').}`
    /// given the sequence `(t1, t2, t3, t4, t5, t6, t7), (t1', t2', t3', t4', t5', t6', t7')`.
    ///
    pub def projectInto7[f: Type -> Type, t1: Type, t2: Type, t3: Type, t4: Type, t5: Type, t6: Type, t7: Type](p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7)]):
        Datalog[Boxed] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6], Boxable[t7], Foldable[f] =
        projectIntoX(match (v1, v2, v3, v4, v5, v6, v7) ->
            [box(v1),
             box(v2),
             box(v3),
             box(v4),
             box(v5),
             box(v6),
             box(v7)] as & Pure
        , p, ts) as & Pure

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4, t5, t6, t7, t8). p(t1', t2', t3', t4', t5', t6', t7', t8').}`
    /// given the sequence
    /// `(t1, t2, t3, t4, t5, t6, t7, t8), (t1', t2', t3', t4', t5', t6', t7', t8')`.
    ///
    pub def projectInto8[f: Type -> Type, t1: Type, t2: Type, t3: Type, t4: Type, t5: Type, t6: Type, t7: Type, t8: Type](p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8)]):
        Datalog[Boxed] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6], Boxable[t7], Boxable[t8], Foldable[f] =
        projectIntoX(match (v1, v2, v3, v4, v5, v6, v7, v8) ->
            [box(v1),
             box(v2),
             box(v3),
             box(v4),
             box(v5),
             box(v6),
             box(v7),
             box(v8)] as & Pure
        , p, ts) as & Pure

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to
    /// `#{p(t1, t2, t3, t4, t5, t6, t7, t8, t9). p(t1', t2', t3', t4', t5', t6', t7', t8', t9').}`
    /// given the sequence
    /// `(t1, t2, t3, t4, t5, t6, t7, t8, t9), (t1', t2', t3', t4', t5', t6', t7', t8', t9')`.
    ///
    pub def projectInto9[f: Type -> Type, t1: Type, t2: Type, t3: Type, t4: Type, t5: Type, t6: Type, t7: Type, t8: Type, t9: Type](p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8, t9)]):
        Datalog[Boxed] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6], Boxable[t7], Boxable[t8], Boxable[t9], Foldable[f] =
        projectIntoX(match (v1, v2, v3, v4, v5, v6, v7, v8, v9) ->
            [box(v1),
             box(v2),
             box(v3),
             box(v4),
             box(v5),
             box(v6),
             box(v7),
             box(v8),
             box(v9)] as & Pure
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
    pub def projectInto10[f: Type -> Type, t1: Type, t2: Type, t3: Type, t4: Type, t5: Type, t6: Type, t7: Type, t8: Type, t9: Type, t10: Type](p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)]):
        Datalog[Boxed] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6], Boxable[t7], Boxable[t8], Boxable[t9], Boxable[t10], Foldable[f] =
        projectIntoX(match (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10) ->
            [box(v1),
             box(v2),
             box(v3),
             box(v4),
             box(v5),
             box(v6),
             box(v7),
             box(v8),
             box(v9),
             box(v10)] as & Pure
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
    pub def projectInto11[f: Type -> Type, t1: Type, t2: Type, t3: Type, t4: Type, t5: Type, t6: Type, t7: Type, t8: Type, t9: Type, t10: Type, t11: Type](p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11)]):
        Datalog[Boxed] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6], Boxable[t7], Boxable[t8], Boxable[t9], Boxable[t10], Boxable[t11], Foldable[f] =
        projectIntoX(match (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11) ->
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
             box(v11)] as & Pure
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
    pub def projectInto12[f: Type -> Type, t1: Type, t2: Type, t3: Type, t4: Type, t5: Type, t6: Type, t7: Type, t8: Type, t9: Type, t10: Type, t11: Type, t12: Type](p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12)]):
        Datalog[Boxed] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6], Boxable[t7], Boxable[t8], Boxable[t9], Boxable[t10], Boxable[t11], Boxable[t12], Foldable[f] =
        projectIntoX(match (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12) ->
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
             box(v12)] as & Pure
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
    pub def projectInto13[f: Type -> Type, t1: Type, t2: Type, t3: Type, t4: Type, t5: Type, t6: Type, t7: Type, t8: Type, t9: Type, t10: Type, t11: Type, t12: Type, t13: Type](p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13)]):
        Datalog[Boxed] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6], Boxable[t7], Boxable[t8], Boxable[t9], Boxable[t10], Boxable[t11], Boxable[t12], Boxable[t13], Foldable[f] =
        projectIntoX(match (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13) ->
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
             box(v13)] as & Pure
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
    pub def projectInto14[f: Type -> Type, t1: Type, t2: Type, t3: Type, t4: Type, t5: Type, t6: Type, t7: Type, t8: Type, t9: Type, t10: Type, t11: Type, t12: Type, t13: Type, t14: Type](p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14)]):
        Datalog[Boxed] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6], Boxable[t7], Boxable[t8], Boxable[t9], Boxable[t10], Boxable[t11], Boxable[t12], Boxable[t13], Boxable[t14], Foldable[f] =
        projectIntoX(match (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14) ->
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
             box(v14)] as & Pure
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
    pub def projectInto15[f: Type -> Type, t1: Type, t2: Type, t3: Type, t4: Type, t5: Type, t6: Type, t7: Type, t8: Type, t9: Type, t10: Type, t11: Type, t12: Type, t13: Type, t14: Type, t15: Type](p: PredSym, ts: f[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15)]):
        Datalog[Boxed] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6], Boxable[t7], Boxable[t8], Boxable[t9], Boxable[t10], Boxable[t11], Boxable[t12], Boxable[t13], Boxable[t14], Boxable[t15], Foldable[f] =
        projectIntoX(match (v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15) ->
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
             box(v15)] as & Pure
        , p, ts) as & Pure

    def projectIntoX[f: Type -> Type, t: Type](f: t -> Array[Boxed], p: PredSym, ts: f[t]): Datalog[Boxed] & Impure with Foldable[f] =
        let PredSym(relName) = p;
        let db = MutMap.empty();
        Foldable.foldLeft(() -> t -> {
            let vs = f(t);
            let arity = Array.length(vs);
            let ramSym = RamSym.Full(relName, arity, Denotation.Relational);
            let rel = MutMap.getOrElsePut!(ramSym, MutMap.empty(), db);
            MutMap.put!(vs, default, rel)
        }, (), ts);
        toModel(db)

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    pub def facts0(p: PredSym, d: Datalog[Boxed]): Array[v] with Boxable[v] =
        factsOf(_ -> () as v, p, d) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    pub def facts1(p: PredSym, d: Datalog[Boxed]): Array[v] with Boxable[v] =
        let f = terms -> unbox(terms[0]);
        factsOf(f, p, d) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    pub def facts2(p: PredSym, d: Datalog[Boxed]): Array[(t1, t2)] with Boxable[t1], Boxable[t2] =
        let f = terms -> (unbox(terms[0]), unbox(terms[1]));
        factsOf(f, p, d) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    pub def facts3(p: PredSym, d: Datalog[Boxed]): Array[(t1, t2, t3)] with Boxable[t1], Boxable[t2], Boxable[t3] =
        let f = terms -> (unbox(terms[0]), unbox(terms[1]), unbox(terms[2]));
        factsOf(f, p, d) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    pub def facts4(p: PredSym, d: Datalog[Boxed]): Array[(t1, t2, t3, t4)] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4] =
        let f = terms -> (unbox(terms[0]), unbox(terms[1]), unbox(terms[2]), unbox(terms[3]));
        factsOf(f, p, d) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    pub def facts5(p: PredSym, d: Datalog[Boxed]): Array[(t1, t2, t3, t4, t5)] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5] =
        let f = terms -> (unbox(terms[0]), unbox(terms[1]), unbox(terms[2]), unbox(terms[3]), unbox(terms[4]));
        factsOf(f, p, d) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    pub def facts6(p: PredSym, d: Datalog[Boxed]): Array[(t1, t2, t3, t4, t5, t6)] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6] =
        let f = terms -> (unbox(terms[0]), unbox(terms[1]), unbox(terms[2]), unbox(terms[3]), unbox(terms[4]), unbox(terms[5]));
        factsOf(f, p, d) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    pub def facts7(p: PredSym, d: Datalog[Boxed]): Array[(t1, t2, t3, t4, t5, t6, t7)] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6], Boxable[t7] =
        let f = terms -> (unbox(terms[0]), unbox(terms[1]), unbox(terms[2]), unbox(terms[3]), unbox(terms[4]), unbox(terms[5]), unbox(terms[6]));
        factsOf(f, p, d) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    pub def facts8(p: PredSym, d: Datalog[Boxed]): Array[(t1, t2, t3, t4, t5, t6, t7, t8)] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6], Boxable[t7], Boxable[t8] =
        let f = terms -> (unbox(terms[0]), unbox(terms[1]), unbox(terms[2]), unbox(terms[3]), unbox(terms[4]), unbox(terms[5]), unbox(terms[6]), unbox(terms[7]));
        factsOf(f, p, d) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    pub def facts9(p: PredSym, d: Datalog[Boxed]): Array[(t1, t2, t3, t4, t5, t6, t7, t8, t9)] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6], Boxable[t7], Boxable[t8], Boxable[t9] =
        let f = terms -> (unbox(terms[0]), unbox(terms[1]), unbox(terms[2]), unbox(terms[3]), unbox(terms[4]), unbox(terms[5]), unbox(terms[6]), unbox(terms[7]), unbox(terms[8]));
        factsOf(f, p, d) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    pub def facts10(p: PredSym, d: Datalog[Boxed]): Array[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10)] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6], Boxable[t7], Boxable[t8], Boxable[t9], Boxable[t10] =
        let f = terms -> (unbox(terms[0]), unbox(terms[1]), unbox(terms[2]), unbox(terms[3]), unbox(terms[4]), unbox(terms[5]), unbox(terms[6]), unbox(terms[7]), unbox(terms[8]), unbox(terms[9]));
        factsOf(f, p, d) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    pub def facts11(p: PredSym, d: Datalog[Boxed]): Array[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11)] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6], Boxable[t7], Boxable[t8], Boxable[t9], Boxable[t10], Boxable[t11] =
        let f = terms -> (unbox(terms[0]), unbox(terms[1]), unbox(terms[2]), unbox(terms[3]), unbox(terms[4]), unbox(terms[5]), unbox(terms[6]), unbox(terms[7]), unbox(terms[8]), unbox(terms[9]), unbox(terms[10]));
        factsOf(f, p, d) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    pub def facts12(p: PredSym, d: Datalog[Boxed]): Array[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12)] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6], Boxable[t7], Boxable[t8], Boxable[t9], Boxable[t10], Boxable[t11], Boxable[t12] =
        let f = terms -> (unbox(terms[0]), unbox(terms[1]), unbox(terms[2]), unbox(terms[3]), unbox(terms[4]), unbox(terms[5]), unbox(terms[6]), unbox(terms[7]), unbox(terms[8]), unbox(terms[9]), unbox(terms[10]), unbox(terms[11]));
        factsOf(f, p, d) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    pub def facts13(p: PredSym, d: Datalog[Boxed]): Array[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13)] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6], Boxable[t7], Boxable[t8], Boxable[t9], Boxable[t10], Boxable[t11], Boxable[t12], Boxable[t13] =
        let f = terms -> (unbox(terms[0]), unbox(terms[1]), unbox(terms[2]), unbox(terms[3]), unbox(terms[4]), unbox(terms[5]), unbox(terms[6]), unbox(terms[7]), unbox(terms[8]), unbox(terms[9]), unbox(terms[10]), unbox(terms[11]), unbox(terms[12]));
        factsOf(f, p, d) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    pub def facts14(p: PredSym, d: Datalog[Boxed]): Array[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14)] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6], Boxable[t7], Boxable[t8], Boxable[t9], Boxable[t10], Boxable[t11], Boxable[t12], Boxable[t13], Boxable[t14] =
        let f = terms -> (unbox(terms[0]), unbox(terms[1]), unbox(terms[2]), unbox(terms[3]), unbox(terms[4]), unbox(terms[5]), unbox(terms[6]), unbox(terms[7]), unbox(terms[8]), unbox(terms[9]), unbox(terms[10]), unbox(terms[11]), unbox(terms[12]), unbox(terms[13]));
        factsOf(f, p, d) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    pub def facts15(p: PredSym, d: Datalog[Boxed]): Array[(t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15)] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5], Boxable[t6], Boxable[t7], Boxable[t8], Boxable[t9], Boxable[t10], Boxable[t11], Boxable[t12], Boxable[t13], Boxable[t14], Boxable[t15] =
        let f = terms -> (unbox(terms[0]), unbox(terms[1]), unbox(terms[2]), unbox(terms[3]), unbox(terms[4]), unbox(terms[5]), unbox(terms[6]), unbox(terms[7]), unbox(terms[8]), unbox(terms[9]), unbox(terms[10]), unbox(terms[11]), unbox(terms[12]), unbox(terms[13]), unbox(terms[14]));
        factsOf(f, p, d) as & Pure

    ///
    /// Returns an array of facts associated with the given predicate symbol `p` in the given Datalog program `d`.
    ///
    def factsOf(f: Array[v] -> t & e, p: PredSym, d: Datalog[v]): Array[t] & Impure = match d {
        case Datalog(_, cs) =>
            let pFacts = MutList.new();
            Array.foreach(c -> match c {
                case Constraint(HeadAtom(headPred, _, terms), []) =>
                    if (headPred == p)
                        let vs = Array.map(headTermValue, terms);
                        MutList.push!(f(vs), pFacts)
                    else
                        ()
                case _ => ()
            }, cs);
            MutList.toArray(pFacts)
        case Model(db) =>
            use Fixpoint/Ram.toDenotation;
            let pFacts = MutList.new();
            let PredSym(key) = p;
            let query = ramSym -> match ramSym {
                case RamSym.Full(relName, _, _) => match relName <=> key {
                    case EqualTo => EqualTo
                    case cmp => cmp
                }
                case _ => LessThan
            };
            Map.queryWith(query, ramSym -> rel -> match toDenotation(ramSym) {
                case Denotation.Relational =>
                    Map.foreach(tuple -> _ -> MutList.push!(f(tuple), pFacts), rel)
                case Denotation.Latticenal(_) =>
                    Map.foreach(tuple -> lat -> {
                        let vs = Array.init(i -> {
                            if (i < Array.length(tuple))
                                tuple[i]
                            else
                                lat
                        }, Array.length(tuple) + 1);
                        MutList.push!(f(vs), pFacts)
                    }, rel)
            }, db);
            MutList.toArray(pFacts)
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
    pub def unsafePrint(d: a): a =
        let _ = Unsafe.println(unsafeCast(d));
        d

    ///
    /// Unsafely casts the given value `d` to a Datalog program.
    ///
    pub def unsafeCast(v: a): Datalog[Boxed] = v as Datalog[Boxed]

    def toModel(db: Database[v]): Datalog[v] & Impure =
        MutMap.toImm(db) |>
        Map.map(MutMap.toImm) |>
        Model
}

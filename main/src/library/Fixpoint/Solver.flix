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

use Boxable.box;
use Boxable.unbox;
use Fixpoint/Ast.{Datalog, Constraint, HeadTerm, PredSym, Denotation};
use Fixpoint/Ast.Datalog.Datalog;
use Fixpoint/Ast.Constraint.Constraint;
use Fixpoint/Ast.HeadPredicate.HeadAtom;
use Fixpoint/Ast.PredSym.PredSym;
use Fixpoint/Ast.SourceLocation.Unknown;
use Fixpoint/Ram.RamSym;

namespace Fixpoint {
    ///
    /// Returns the minimal model of the given Datalog program `d`.
    /// The minimal model is given by the model semantics for Datalog programs with stratified negation.
    /// The stratification is computed automatically by the solver.
    ///
    pub def solve(d: Datalog[v]): Datalog[v] with Order[v], Eq[v], ToString[v] =
        stratify(d) |>
        solveWithStratification(d)

    ///
    /// Returns the minimal model of the given Datalog program `d`.
    /// The minimal model is given by the model semantics for Datalog programs with stratified negation.
    /// A stratification of `d` is given by `stf`.
    ///
    pub def solveWithStratification(d: Datalog[v], stf: Map[PredSym, Int32]): Datalog[v] with Order[v], Eq[v], ToString[v] =
        let _ = notifyPreSolve(d, stf);
        let model = (compile(d, stf) |>
         queryStmt |>
         lowerStmt |>
         interpret |>
         toAst) as & Pure;
         let _ = notifyPostSolve(model);
         model

    ///
    /// Returns the pairwise union of `d1` and `d2`.
    /// I.e. the facts of the union is the union of the facts and likewise for rules.
    /// A fact or rule may occur twice in the Datalog program. This has no effect on its semantics.
    ///
    pub def union(d1: Datalog[v], d2: Datalog[v]): Datalog[v] = match (d1, d2) {
        case (Datalog(edb1, idb1), Datalog(edb2, idb2)) =>
            Datalog(Array.append(edb1, edb2), Array.append(idb1, idb2)) as & Pure
    }

    ///
    /// Project all facts in the given Datalog program `d` that has `p` in its head.
    ///
    pub def project(p: PredSym, d: Datalog[v]): Datalog[v] = match d {
        case Datalog(facts, rules) => Datalog(restrict(facts, Set#{p}), rules)
    }

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to `#{p(t1). p(t1').}`
    /// given the sequence `(t1), (t1')`.
    ///
    pub def projectInto1[f: Type -> Type, t1: Type](p: PredSym, ts: f[(t1)]):
        Datalog[Boxed] with Boxable[t1], Foldable[f] =
        projectIntoX(match (v1) -> [HeadTerm.Lit(box(v1))] as & Pure, p, ts) as & Pure

    ///
    /// Insert all facts in the given sequence `f` into the given relation `p`.
    ///
    /// I.e. returns a Datalog program corresponding to `#{p(t1, t2). p(t1', t2').}`
    /// given the sequence `(t1, t2), (t1', t2')`.
    ///
    pub def projectInto2[f: Type -> Type, t1: Type, t2: Type](p: PredSym, ts: f[(t1, t2)]):
        Datalog[Boxed] with Boxable[t1], Boxable[t2], Foldable[f] =
        projectIntoX(match (v1, v2) ->
            [HeadTerm.Lit(box(v1)),
             HeadTerm.Lit(box(v2))] as & Pure
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
            [HeadTerm.Lit(box(v1)),
             HeadTerm.Lit(box(v2)),
             HeadTerm.Lit(box(v3))] as & Pure
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
            [HeadTerm.Lit(box(v1)),
             HeadTerm.Lit(box(v2)),
             HeadTerm.Lit(box(v3)),
             HeadTerm.Lit(box(v4))] as & Pure
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
            [HeadTerm.Lit(box(v1)),
             HeadTerm.Lit(box(v2)),
             HeadTerm.Lit(box(v3)),
             HeadTerm.Lit(box(v4)),
             HeadTerm.Lit(box(v5))] as & Pure
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
            [HeadTerm.Lit(box(v1)),
             HeadTerm.Lit(box(v2)),
             HeadTerm.Lit(box(v3)),
             HeadTerm.Lit(box(v4)),
             HeadTerm.Lit(box(v5)),
             HeadTerm.Lit(box(v6))] as & Pure
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
            [HeadTerm.Lit(box(v1)),
             HeadTerm.Lit(box(v2)),
             HeadTerm.Lit(box(v3)),
             HeadTerm.Lit(box(v4)),
             HeadTerm.Lit(box(v5)),
             HeadTerm.Lit(box(v6)),
             HeadTerm.Lit(box(v7))] as & Pure
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
            [HeadTerm.Lit(box(v1)),
             HeadTerm.Lit(box(v2)),
             HeadTerm.Lit(box(v3)),
             HeadTerm.Lit(box(v4)),
             HeadTerm.Lit(box(v5)),
             HeadTerm.Lit(box(v6)),
             HeadTerm.Lit(box(v7)),
             HeadTerm.Lit(box(v8))] as & Pure
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
            [HeadTerm.Lit(box(v1)),
             HeadTerm.Lit(box(v2)),
             HeadTerm.Lit(box(v3)),
             HeadTerm.Lit(box(v4)),
             HeadTerm.Lit(box(v5)),
             HeadTerm.Lit(box(v6)),
             HeadTerm.Lit(box(v7)),
             HeadTerm.Lit(box(v8)),
             HeadTerm.Lit(box(v9))] as & Pure
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
            [HeadTerm.Lit(box(v1)),
             HeadTerm.Lit(box(v2)),
             HeadTerm.Lit(box(v3)),
             HeadTerm.Lit(box(v4)),
             HeadTerm.Lit(box(v5)),
             HeadTerm.Lit(box(v6)),
             HeadTerm.Lit(box(v7)),
             HeadTerm.Lit(box(v8)),
             HeadTerm.Lit(box(v9)),
             HeadTerm.Lit(box(v10))] as & Pure
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
            [HeadTerm.Lit(box(v1)),
             HeadTerm.Lit(box(v2)),
             HeadTerm.Lit(box(v3)),
             HeadTerm.Lit(box(v4)),
             HeadTerm.Lit(box(v5)),
             HeadTerm.Lit(box(v6)),
             HeadTerm.Lit(box(v7)),
             HeadTerm.Lit(box(v8)),
             HeadTerm.Lit(box(v9)),
             HeadTerm.Lit(box(v10)),
             HeadTerm.Lit(box(v11))] as & Pure
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
            [HeadTerm.Lit(box(v1)),
             HeadTerm.Lit(box(v2)),
             HeadTerm.Lit(box(v3)),
             HeadTerm.Lit(box(v4)),
             HeadTerm.Lit(box(v5)),
             HeadTerm.Lit(box(v6)),
             HeadTerm.Lit(box(v7)),
             HeadTerm.Lit(box(v8)),
             HeadTerm.Lit(box(v9)),
             HeadTerm.Lit(box(v10)),
             HeadTerm.Lit(box(v11)),
             HeadTerm.Lit(box(v12))] as & Pure
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
            [HeadTerm.Lit(box(v1)),
             HeadTerm.Lit(box(v2)),
             HeadTerm.Lit(box(v3)),
             HeadTerm.Lit(box(v4)),
             HeadTerm.Lit(box(v5)),
             HeadTerm.Lit(box(v6)),
             HeadTerm.Lit(box(v7)),
             HeadTerm.Lit(box(v8)),
             HeadTerm.Lit(box(v9)),
             HeadTerm.Lit(box(v10)),
             HeadTerm.Lit(box(v11)),
             HeadTerm.Lit(box(v12)),
             HeadTerm.Lit(box(v13))] as & Pure
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
            [HeadTerm.Lit(box(v1)),
             HeadTerm.Lit(box(v2)),
             HeadTerm.Lit(box(v3)),
             HeadTerm.Lit(box(v4)),
             HeadTerm.Lit(box(v5)),
             HeadTerm.Lit(box(v6)),
             HeadTerm.Lit(box(v7)),
             HeadTerm.Lit(box(v8)),
             HeadTerm.Lit(box(v9)),
             HeadTerm.Lit(box(v10)),
             HeadTerm.Lit(box(v11)),
             HeadTerm.Lit(box(v12)),
             HeadTerm.Lit(box(v13)),
             HeadTerm.Lit(box(v14))] as & Pure
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
            [HeadTerm.Lit(box(v1)),
             HeadTerm.Lit(box(v2)),
             HeadTerm.Lit(box(v3)),
             HeadTerm.Lit(box(v4)),
             HeadTerm.Lit(box(v5)),
             HeadTerm.Lit(box(v6)),
             HeadTerm.Lit(box(v7)),
             HeadTerm.Lit(box(v8)),
             HeadTerm.Lit(box(v9)),
             HeadTerm.Lit(box(v10)),
             HeadTerm.Lit(box(v11)),
             HeadTerm.Lit(box(v12)),
             HeadTerm.Lit(box(v13)),
             HeadTerm.Lit(box(v14)),
             HeadTerm.Lit(box(v15))] as & Pure
        , p, ts) as & Pure

    def projectIntoX[f: Type -> Type, t: Type](f: t -> Array[HeadTerm[Boxed]], p: PredSym, ts: f[t]):
        Datalog[Boxed] & Impure with Foldable[f] =
        let facts = MutList.new();
        Foldable.foldLeft(() -> vs -> {            
            let fact = Constraint(HeadAtom(p, Denotation.Relational, f(vs)), []);
            MutList.push!(fact, facts)
        }, (), ts);
        Datalog(MutList.toArray(facts), [])

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    pub def facts0(p: PredSym, d: Datalog[Boxed]): Array[v] with Boxable[v] =
        (factsOf(p, d) |> Array.map(_ -> () as v)) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    pub def facts1(p: PredSym, d: Datalog[Boxed]): Array[v] with Boxable[v] =
        let f = terms -> unbox(terms[0]);
        (factsOf(p, d) |> Array.map(f)) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    pub def facts2(p: PredSym, d: Datalog[Boxed]): Array[(t1, t2)] with Boxable[t1], Boxable[t2] =
        let f = terms -> (unbox(terms[0]), unbox(terms[1]));
        (factsOf(p, d) |> Array.map(f)) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    pub def facts3(p: PredSym, d: Datalog[Boxed]): Array[(t1, t2, t3)] with Boxable[t1], Boxable[t2], Boxable[t3] =
        let f = terms -> (unbox(terms[0]), unbox(terms[1]), unbox(terms[2]));
        (factsOf(p, d) |> Array.map(f)) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    pub def facts4(p: PredSym, d: Datalog[Boxed]): Array[(t1, t2, t3, t4)] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4] =
        let f = terms -> (unbox(terms[0]), unbox(terms[1]), unbox(terms[2]), unbox(terms[3]));
        (factsOf(p, d) |> Array.map(f)) as & Pure

    ///
    /// Returns all facts in `d` associated with the predicate symbol `p`.
    ///
    pub def facts5(p: PredSym, d: Datalog[Boxed]): Array[(t1, t2, t3, t4, t5)] with Boxable[t1], Boxable[t2], Boxable[t3], Boxable[t4], Boxable[t5] =
        let f = terms -> (unbox(terms[0]), unbox(terms[1]), unbox(terms[2]), unbox(terms[3]), unbox(terms[4]));
        (factsOf(p, d) |> Array.map(f)) as & Pure

    ///
    /// Returns an array of facts associated with the given predicate symbol `p` in the given Datalog program `d`.
    ///
    def factsOf(p: PredSym, d: Datalog[v]): Array[Array[v]] & Impure =
        let Datalog(facts, _) = d;
        restrict(facts, Set#{p}) |>
        Array.map(factToTuple)

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


    ///
    /// Convert `fact` to an array of terms.
    /// I.e. `A(t1, t2, ..., tn).` becomes `[t1, t2, ..., tn]`.
    ///
    /// Note that `t1, t2, ..., tn` must have been fully evaluated to literals.
    ///
    def factToTuple(fact: Constraint[v]): Array[v] & Impure = match fact {
        case Constraint(HeadAtom(_, _, terms), _) => Array.map(headTermValue, terms)
    }

    def tupleToFact(ramSym: RamSym[v], tuple: Array[v], lat: v): Constraint[v] & Impure = match ramSym {
        case RamSym.Full(relName, denotation) =>
            let terms = match denotation {
                case Denotation.Relational => Array.map(v -> HeadTerm.Lit(v), tuple)
                case Denotation.Latticenal(_) => Array.init(i -> {
                    if (i < Array.length(tuple))
                        HeadTerm.Lit(tuple[i])
                    else
                        HeadTerm.Lit(lat)
                }, Array.length(tuple) + 1)
            };
            Constraint(HeadAtom(PredSym(relName), denotation, terms), [])
        case _ => ?bug
    }

    def toAst(db: Database[v]): Datalog[v] & Impure =
        // TODO: don't eagerly convert back to AST. Keep it as a database as long as possible.
        let facts = MutList.new();
        MutMap.foreach(ramSym -> rel -> match ramSym {
            case RamSym.Full(_) => MutMap.foreach(t -> l -> {
                MutList.push!(tupleToFact(ramSym, t, l), facts)
            }, rel)
            case _ => ()
        }, db);
        Datalog(MutList.toArray(facts), [])
}
/*
 * Copyright 2025 Jakob Schneider Villumsen
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
package ca.uwaterloo.flix.api

import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.TypedAst.Expr
import ca.uwaterloo.flix.util.ParOps

import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.CollectionHasAsScala

object PackageTrust {

  def run(root: TypedAst.Root)(implicit flix: Flix): List[SuspiciousExpr] = {
    implicit val sctx: SharedContext = SharedContext.mk()
    ParOps.parMap(root.traits.values)(visitTrait)
    ParOps.parMap(root.instances.values)(visitInstance)
    ParOps.parMap(root.sigs.values)(visitSig)
    ParOps.parMap(root.defs.values)(visitDef)
    sctx.exprs.asScala.toList
  }

  private def visitTrait(trait0: TypedAst.Trait)(implicit sctx: SharedContext): Unit = trait0 match {
    case TypedAst.Trait(_, _, _, _, _, _, _, sigs, laws, _) =>
      sigs.foreach(visitSig)
      laws.foreach(visitDef)
  }

  private def visitInstance(instance0: TypedAst.Instance)(implicit sctx: SharedContext): Unit = instance0 match {
    case TypedAst.Instance(_, _, _, _, _, _, _, _, _, defs, _, _) =>
      defs.foreach(visitDef)
  }

  private def visitSig(sig0: TypedAst.Sig)(implicit sctx: SharedContext): Unit = sig0 match {
    case TypedAst.Sig(_, _, exp, _) =>
      exp.foreach(visitExp)
  }

  private def visitDef(def0: TypedAst.Def)(implicit sctx: SharedContext): Unit = def0 match {
    case TypedAst.Def(_, _, exp, _) =>
      visitExp(exp)
  }

  private def visitExp(expr0: TypedAst.Expr)(implicit sctx: SharedContext): Unit = expr0 match {
    case Expr.Cst(_, _, _) =>
      ()

    case Expr.Var(_, _, _) =>
      ()

    case Expr.Hole(_, _, _, _, _) =>
      ()

    case Expr.HoleWithExp(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.OpenAs(_, exp, _, _) =>
      visitExp(exp)

    case Expr.Use(_, _, exp, _) =>
      visitExp(exp)

    case Expr.Lambda(_, exp, _, _) =>
      visitExp(exp)

    case Expr.ApplyClo(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.ApplyDef(_, exps, _, _, _, _, _) =>
      exps.foreach(visitExp)

    case Expr.ApplyLocalDef(_, exps, _, _, _, _) =>
      exps.foreach(visitExp)

    case Expr.ApplyOp(_, exps, _, _, _) =>
      exps.foreach(visitExp)

    case Expr.ApplySig(_, exps, _, _, _, _, _, _) =>
      exps.foreach(visitExp)

    case Expr.Unary(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.Binary(_, exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.Let(_, exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.LocalDef(_, _, exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.Region(_, _) =>
      ()

    case Expr.Scope(_, _, exp, _, _, _) =>
      visitExp(exp)

    case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitExp(exp3)

    case Expr.Stm(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.Discard(exp, _, _) =>
      visitExp(exp)

    case Expr.Match(exp, rules, _, _, _) =>
      // TODO
      visitExp(exp)

    case Expr.TypeMatch(exp, rules, _, _, _) =>
      // TODO
      visitExp(exp)

    case Expr.RestrictableChoose(_, exp, rules, _, _, _) =>
      // TODO
      visitExp(exp)

    case Expr.ExtMatch(exp, rules, _, _, _) =>
      // TODO
      visitExp(exp)

    case Expr.Tag(_, exps, _, _, _) =>
      exps.foreach(visitExp)

    case Expr.RestrictableTag(_, exps, _, _, _) =>
      exps.foreach(visitExp)

    case Expr.ExtTag(_, exps, _, _, _) =>
      exps.foreach(visitExp)

    case Expr.Tuple(exps, _, _, _) =>
      exps.foreach(visitExp)

    case Expr.RecordSelect(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.RecordExtend(_, exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.RecordRestrict(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.ArrayLit(exps, exp, _, _, _) =>
      visitExp(exp)
      exps.foreach(visitExp)

    case Expr.ArrayNew(exp1, exp2, exp3, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitExp(exp3)

    case Expr.ArrayLoad(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.ArrayLength(exp, _, _) =>
      visitExp(exp)

    case Expr.ArrayStore(exp1, exp2, exp3, _, _) =>
      visitExp(exp1)
      visitExp(exp2)
      visitExp(exp3)

    case Expr.StructNew(_, fields, region, _, _, _) =>
      // TODO
      ???

    case Expr.StructGet(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.StructPut(exp1, _, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.VectorLit(exps, _, _, _) =>
      exps.foreach(visitExp)

    case Expr.VectorLoad(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.VectorLength(exp, _) =>
      visitExp(exp)

    case Expr.Ascribe(exp, _, _, _, _, _) =>
      visitExp(exp)

    case Expr.InstanceOf(exp, _, _) =>
      // TODO
      visitExp(exp)

    case Expr.CheckedCast(_, exp, _, _, _) =>
      visitExp(exp)

    case Expr.UncheckedCast(exp, _, _, _, _, _) =>
      // TODO
      visitExp(exp)

    case Expr.Unsafe(exp, _, _, _, _) =>
      // TODO
      visitExp(exp)

    case Expr.Without(exp, _, _, _, _) =>
      visitExp(exp)

    case Expr.TryCatch(exp, rules, _, _, _) =>
      // TODO
      visitExp(exp)

    case Expr.Throw(exp, _, _, _) =>
      visitExp(exp)

    case Expr.Handler(_, rules, _, _, _, _, _) =>
      // TODO
      ???

    case Expr.RunWith(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.InvokeConstructor(_, exps, _, _, _) =>
      // TODO
      exps.foreach(visitExp)

    case Expr.InvokeMethod(_, exp, exps, _, _, _) =>
      // TODO
      visitExp(exp)
      exps.foreach(visitExp)

    case Expr.InvokeStaticMethod(_, exps, _, _, _) =>
      // TODO
      exps.foreach(visitExp)

    case Expr.GetField(_, exp, _, _, _) =>
      // TODO
      visitExp(exp)

    case Expr.PutField(_, exp1, exp2, _, _, _) =>
      // TODO
      visitExp(exp1)
      visitExp(exp2)

    case Expr.GetStaticField(_, _, _, _) =>
      // TODO
      ???

    case Expr.PutStaticField(_, exp, _, _, _) =>
      // TODO
      visitExp(exp)

    case Expr.NewObject(_, _, _, _, methods, _) =>
      // TODO
      ???

    case Expr.NewChannel(exp, _, _, _) =>
      visitExp(exp)

    case Expr.GetChannel(exp, _, _, _) =>
      visitExp(exp)

    case Expr.PutChannel(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.SelectChannel(rules, default, _, _, _) =>
      // TODO
      default.foreach(visitExp)

    case Expr.Spawn(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.ParYield(frags, exp, _, _, _) =>
      // TODO
      visitExp(exp)

    case Expr.Lazy(exp, _, _) =>
      visitExp(exp)

    case Expr.Force(exp, _, _, _) =>
      visitExp(exp)

    case Expr.FixpointConstraintSet(cs, _, _) =>
      // TODO
      ???

    case Expr.FixpointLambda(pparams, exp, _, _, _) =>
      // TODO
      visitExp(exp)

    case Expr.FixpointMerge(exp1, exp2, _, _, _) =>
      visitExp(exp1)
      visitExp(exp2)

    case Expr.FixpointQueryWithProvenance(exps, select, withh, _, _, _) =>
      // TODO
      exps.foreach(visitExp)

    case Expr.FixpointQueryWithSelect(exps, queryExp, selects, from, where, pred, _, _, _) =>
      // TODO
      exps.foreach(visitExp)

    case Expr.FixpointSolveWithProject(exps, optPreds, mode, _, _, _) =>
      // TODO
      exps.foreach(visitExp)

    case Expr.FixpointInjectInto(exps, predsAndArities, _, _, _) =>
      // TODO
      exps.foreach(visitExp)

    case Expr.Error(_, _, _) =>
      ()

  }

  /** Companion object for [[SharedContext]] */
  private object SharedContext {

    /** Returns a fresh shared context. */
    def mk(): SharedContext = new SharedContext(new ConcurrentLinkedQueue())
  }

  /**
    * A globally shared context. Must be thread-safe.
    *
    * @param exprs the [[SuspiciousExpr]]s in the AST, if any.
    */
  private case class SharedContext(exprs: ConcurrentLinkedQueue[SuspiciousExpr])


}

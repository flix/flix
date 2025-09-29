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
    case Expr.Cst(cst, tpe, loc) => ???
    case Expr.Var(sym, tpe, loc) => ???
    case Expr.Hole(sym, scp, tpe, eff, loc) => ???
    case Expr.HoleWithExp(exp, scp, tpe, eff, loc) => ???
    case Expr.OpenAs(symUse, exp, tpe, loc) => ???
    case Expr.Use(sym, alias, exp, loc) => ???
    case Expr.Lambda(fparam, exp, tpe, loc) => ???
    case Expr.ApplyClo(exp1, exp2, tpe, eff, loc) => ???
    case Expr.ApplyDef(symUse, exps, targs, itpe, tpe, eff, loc) => ???
    case Expr.ApplyLocalDef(symUse, exps, arrowTpe, tpe, eff, loc) => ???
    case Expr.ApplyOp(symUse, exps, tpe, eff, loc) => ???
    case Expr.ApplySig(symUse, exps, targ, targs, itpe, tpe, eff, loc) => ???
    case Expr.Unary(sop, exp, tpe, eff, loc) => ???
    case Expr.Binary(sop, exp1, exp2, tpe, eff, loc) => ???
    case Expr.Let(bnd, exp1, exp2, tpe, eff, loc) => ???
    case Expr.LocalDef(bnd, fparams, exp1, exp2, tpe, eff, loc) => ???
    case Expr.Region(tpe, loc) => ???
    case Expr.Scope(bnd, regSym, exp, tpe, eff, loc) => ???
    case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) => ???
    case Expr.Stm(exp1, exp2, tpe, eff, loc) => ???
    case Expr.Discard(exp, eff, loc) => ???
    case Expr.Match(exp, rules, tpe, eff, loc) => ???
    case Expr.TypeMatch(exp, rules, tpe, eff, loc) => ???
    case Expr.RestrictableChoose(star, exp, rules, tpe, eff, loc) => ???
    case Expr.ExtMatch(exp, rules, tpe, eff, loc) => ???
    case Expr.Tag(symUse, exps, tpe, eff, loc) => ???
    case Expr.RestrictableTag(symUse, exps, tpe, eff, loc) => ???
    case Expr.ExtTag(label, exps, tpe, eff, loc) => ???
    case Expr.Tuple(exps, tpe, eff, loc) => ???
    case Expr.RecordSelect(exp, label, tpe, eff, loc) => ???
    case Expr.RecordExtend(label, exp1, exp2, tpe, eff, loc) => ???
    case Expr.RecordRestrict(label, exp, tpe, eff, loc) => ???
    case Expr.ArrayLit(exps, exp, tpe, eff, loc) => ???
    case Expr.ArrayNew(exp1, exp2, exp3, tpe, eff, loc) => ???
    case Expr.ArrayLoad(exp1, exp2, tpe, eff, loc) => ???
    case Expr.ArrayLength(exp, eff, loc) => ???
    case Expr.ArrayStore(exp1, exp2, exp3, eff, loc) => ???
    case Expr.StructNew(sym, fields, region, tpe, eff, loc) => ???
    case Expr.StructGet(exp, symUse, tpe, eff, loc) => ???
    case Expr.StructPut(exp1, symUse, exp2, tpe, eff, loc) => ???
    case Expr.VectorLit(exps, tpe, eff, loc) => ???
    case Expr.VectorLoad(exp1, exp2, tpe, eff, loc) => ???
    case Expr.VectorLength(exp, loc) => ???
    case Expr.Ascribe(exp, expectedType, expectedEff, tpe, eff, loc) => ???
    case Expr.InstanceOf(exp, clazz, loc) => ???
    case Expr.CheckedCast(cast, exp, tpe, eff, loc) => ???
    case Expr.UncheckedCast(exp, declaredType, declaredEff, tpe, eff, loc) => ???
    case Expr.Unsafe(exp, runEff, tpe, eff, loc) => ???
    case Expr.Without(exp, symUse, tpe, eff, loc) => ???
    case Expr.TryCatch(exp, rules, tpe, eff, loc) => ???
    case Expr.Throw(exp, tpe, eff, loc) => ???
    case Expr.Handler(symUse, rules, bodyType, bodyEff, handledEff, tpe, loc) => ???
    case Expr.RunWith(exp1, exp2, tpe, eff, loc) => ???
    case Expr.InvokeConstructor(constructor, exps, tpe, eff, loc) => ???
    case Expr.InvokeMethod(method, exp, exps, tpe, eff, loc) => ???
    case Expr.InvokeStaticMethod(method, exps, tpe, eff, loc) => ???
    case Expr.GetField(field, exp, tpe, eff, loc) => ???
    case Expr.PutField(field, exp1, exp2, tpe, eff, loc) => ???
    case Expr.GetStaticField(field, tpe, eff, loc) => ???
    case Expr.PutStaticField(field, exp, tpe, eff, loc) => ???
    case Expr.NewObject(name, clazz, tpe, eff, methods, loc) => ???
    case Expr.NewChannel(exp, tpe, eff, loc) => ???
    case Expr.GetChannel(exp, tpe, eff, loc) => ???
    case Expr.PutChannel(exp1, exp2, tpe, eff, loc) => ???
    case Expr.SelectChannel(rules, default, tpe, eff, loc) => ???
    case Expr.Spawn(exp1, exp2, tpe, eff, loc) => ???
    case Expr.ParYield(frags, exp, tpe, eff, loc) => ???
    case Expr.Lazy(exp, tpe, loc) => ???
    case Expr.Force(exp, tpe, eff, loc) => ???
    case Expr.FixpointConstraintSet(cs, tpe, loc) => ???
    case Expr.FixpointLambda(pparams, exp, tpe, eff, loc) => ???
    case Expr.FixpointMerge(exp1, exp2, tpe, eff, loc) => ???
    case Expr.FixpointQueryWithProvenance(exps, select, withh, tpe, eff, loc) => ???
    case Expr.FixpointQueryWithSelect(exps, queryExp, selects, from, where, pred, tpe, eff, loc) => ???
    case Expr.FixpointSolveWithProject(exps, optPreds, mode, tpe, eff, loc) => ???
    case Expr.FixpointInjectInto(exps, predsAndArities, tpe, eff, loc) => ???
    case Expr.Error(m, tpe, eff) => ???
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

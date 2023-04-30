/*
 * Copyright 2020-2021 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{AtomicOp, ErasedAst, FinalAst, MonoType}
import ca.uwaterloo.flix.language.phase.jvm.{AnonClassInfo, ClosureInfo}

import scala.collection.mutable

object Eraser {

  def run(root: FinalAst.Root)(implicit flix: Flix): ErasedAst.Root = flix.phase("Eraser") {

    //
    // A mutable set to hold all type information about all closures in the AST.
    //
    implicit val ctx: Context = Context(mutable.Set.empty, mutable.Set.empty)

    val defs = root.defs.map { case (k, v) => k -> visitDef(v) }
    val enums = root.enums.map { case (k, v) => k -> visitEnum(v) }

    ErasedAst.Root(defs, enums, root.entryPoint, root.sources, ctx.closures.toSet, ctx.anonClasses.toSet)
  }

  /**
    * Translates the given enum `enum0` to the ErasedAst.
    */
  private def visitEnum(enum0: FinalAst.Enum): ErasedAst.Enum = {
    val cases = enum0.cases map { case (t, c) => t -> visitCase(c) }
    ErasedAst.Enum(enum0.ann, enum0.mod, enum0.sym, cases, enum0.tpeDeprecated, enum0.loc)
  }

  /**
    * Translates the given case `case0` to the ErasedAst.
    */
  private def visitCase(case0: FinalAst.Case): ErasedAst.Case = {
    ErasedAst.Case(case0.sym, case0.tpeDeprecated, case0.loc)
  }

  /**
    * Translates the given definition `def0` to the ErasedAst.
    */
  private def visitDef(def0: FinalAst.Def)(implicit ctx: Context): ErasedAst.Def = {
    val formals = def0.formals.map(visitFormalParam)
    val exp = visitExp(def0.exp)
    ErasedAst.Def(def0.ann, def0.mod, def0.sym, formals, exp, def0.tpe, def0.loc)
  }

  /**
    * Translates the given formal param `p` to the ErasedAst.
    */
  private def visitFormalParam(p: FinalAst.FormalParam): ErasedAst.FormalParam =
    ErasedAst.FormalParam(p.sym, p.tpe)

  /**
    * Translates the given expression `exp0` to the ErasedAst.
    */
  private def visitExp(exp0: FinalAst.Expression)(implicit ctx: Context): ErasedAst.Expr = exp0 match {
    case FinalAst.Expression.Cst(cst, tpe, loc) =>
      ErasedAst.Expr.Cst(cst, tpe, loc)

    case FinalAst.Expression.Var(sym, tpe, loc) =>
      ErasedAst.Expr.Var(sym, tpe, loc)

    case FinalAst.Expression.ApplyAtomic(op, exps, tpe, loc) =>
      op match {
        case AtomicOp.Closure(sym) =>
          ctx.closures += ClosureInfo(sym, exps.map(_.tpe), tpe)
        case _ => // nop
      }
      val es = exps.map(visitExp)
      ErasedAst.Expr.ApplyAtomic(op, es, tpe, loc)

    case FinalAst.Expression.ApplyClo(exp, exps, tpe, loc) =>
      ErasedAst.Expr.ApplyClo(visitExp(exp), exps.map(visitExp), tpe, loc)

    case FinalAst.Expression.ApplyDef(sym, exps, tpe, loc) =>
      ErasedAst.Expr.ApplyDef(sym, exps.map(visitExp), tpe, loc)

    case FinalAst.Expression.ApplyCloTail(exp, exps, tpe, loc) =>
      ErasedAst.Expr.ApplyCloTail(visitExp(exp), exps.map(visitExp), tpe, loc)

    case FinalAst.Expression.ApplyDefTail(sym, exps, tpe, loc) =>
      ErasedAst.Expr.ApplyDefTail(sym, exps.map(visitExp), tpe, loc)

    case FinalAst.Expression.ApplySelfTail(sym, formals0, exps, tpe, loc) =>
      val formals = formals0.map {
        case FinalAst.FormalParam(formalSym, formalTpe) => ErasedAst.FormalParam(formalSym, formalTpe)
      }
      ErasedAst.Expr.ApplySelfTail(sym, formals, exps.map(visitExp), tpe, loc)

    case FinalAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
      ErasedAst.Expr.IfThenElse(visitExp(exp1), visitExp(exp2), visitExp(exp3), tpe, loc)

    case FinalAst.Expression.Branch(exp, branches0, tpe, loc) =>
      val branches = branches0.map {
        case (branchLabel, branchExp) => (branchLabel, visitExp(branchExp))
      }
      ErasedAst.Expr.Branch(visitExp(exp), branches, tpe, loc)

    case FinalAst.Expression.JumpTo(sym, tpe, loc) =>
      ErasedAst.Expr.JumpTo(sym, tpe, loc)

    case FinalAst.Expression.Let(sym, exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      ErasedAst.Expr.Let(sym, e1, e2, tpe, loc)

    case FinalAst.Expression.LetRec(varSym, index, defSym, exp1, exp2, tpe, loc) =>
      val e1 = visitExp(exp1)
      val e2 = visitExp(exp2)
      ErasedAst.Expr.LetRec(varSym, index, defSym, e1, e2, tpe, loc)

    case FinalAst.Expression.Scope(sym, exp, tpe, loc) =>
      ErasedAst.Expr.Scope(sym, visitExp(exp), tpe, loc)

    case FinalAst.Expression.TryCatch(exp, rules0, tpe, loc) =>
      val rules = rules0.map {
        case FinalAst.CatchRule(catchSym, catchClazz, catchExp) =>
          ErasedAst.CatchRule(catchSym, catchClazz, visitExp(catchExp))
      }
      ErasedAst.Expr.TryCatch(visitExp(exp), rules, tpe, loc)

    case FinalAst.Expression.NewObject(name, clazz, tpe, methods0, loc) =>
      val methods = methods0.map {
        case FinalAst.JvmMethod(ident, fparams, clo, retTpe, loc) =>
          val f = fparams.map(visitFormalParam)
          ErasedAst.JvmMethod(ident, f, visitExp(clo), retTpe, loc)
      }
      ctx.anonClasses += AnonClassInfo(name, clazz, tpe, methods, loc)
      ErasedAst.Expr.NewObject(name, clazz, tpe, methods, loc)

    case FinalAst.Expression.Spawn(exp1, exp2, tpe, loc) =>
      val op = AtomicOp.Spawn
      ErasedAst.Expr.ApplyAtomic(op, List(visitExp(exp1), visitExp(exp2)), tpe, loc)

  }

  private case class Context(closures: mutable.Set[ClosureInfo], anonClasses: mutable.Set[AnonClassInfo])

}

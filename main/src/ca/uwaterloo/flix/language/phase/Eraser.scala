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

    case FinalAst.Expression.Closure(sym, exps, tpe, loc) =>
      ctx.closures += ClosureInfo(sym, exps.map(_.tpe), tpe)
      val op = AtomicOp.Closure(sym)
      ErasedAst.Expr.App(op, exps.map(visitExp), tpe, loc)

    case FinalAst.Expression.ApplyClo(exp, exps, tpe, loc) =>
      val op = AtomicOp.ApplyClo
      ErasedAst.Expr.App(op, visitExp(exp) :: exps.map(visitExp), tpe, loc)

    case FinalAst.Expression.ApplyDef(sym, exps, tpe, loc) =>
      val op = AtomicOp.ApplyDef(sym)
      ErasedAst.Expr.App(op, exps.map(visitExp), tpe, loc)

    case FinalAst.Expression.ApplyCloTail(exp, exps, tpe, loc) =>
      val op = AtomicOp.ApplyCloTail
      ErasedAst.Expr.App(op, visitExp(exp) :: exps.map(visitExp), tpe, loc)

    case FinalAst.Expression.ApplyDefTail(sym, exps, tpe, loc) =>
      val op = AtomicOp.ApplyDefTail(sym)
      ErasedAst.Expr.App(op, exps.map(visitExp), tpe, loc)

    case FinalAst.Expression.ApplySelfTail(sym, formals0, exps, tpe, loc) =>
      val formals = formals0.map {
        case FinalAst.FormalParam(formalSym, formalTpe) => ErasedAst.FormalParam(formalSym, formalTpe)
      }
      val op = AtomicOp.ApplySelfTail(sym, formals)
      ErasedAst.Expr.App(op, exps.map(visitExp), tpe, loc)

    case FinalAst.Expression.Unary(sop, _, exp, tpe, loc) =>
      val op = AtomicOp.Unary(sop)
      ErasedAst.Expr.App(op, List(visitExp(exp)), tpe, loc)

    case FinalAst.Expression.Binary(sop, op, exp1, exp2, tpe, loc) =>
      val op = AtomicOp.Binary(sop)
      ErasedAst.Expr.App(op, List(visitExp(exp1), visitExp(exp2)), tpe, loc)

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

    case FinalAst.Expression.Region(tpe, loc) =>
      val op = AtomicOp.Region
      ErasedAst.Expr.App(op, Nil, tpe, loc)

    case FinalAst.Expression.Scope(sym, exp, tpe, loc) =>
      ErasedAst.Expr.Scope(sym, visitExp(exp), tpe, loc)

    case FinalAst.Expression.ScopeExit(exp1, exp2, tpe, loc) =>
      val op = AtomicOp.ScopeExit
      ErasedAst.Expr.App(op, List(visitExp(exp1), visitExp(exp2)), tpe, loc)

    case FinalAst.Expression.Is(sym, exp, loc) =>
      val op = AtomicOp.Is(sym)
      ErasedAst.Expr.App(op, List(visitExp(exp)), MonoType.Bool, loc)

    case FinalAst.Expression.Tag(sym, exp, tpe, loc) =>
      val op = AtomicOp.Tag(sym)
      ErasedAst.Expr.App(op, List(visitExp(exp)), tpe, loc)

    case FinalAst.Expression.Untag(sym, exp, tpe, loc) =>
      val op = AtomicOp.Untag(sym)
      ErasedAst.Expr.App(op, List(visitExp(exp)), tpe, loc)

    case FinalAst.Expression.Index(base, idx, tpe, loc) =>
      val op = AtomicOp.Index(idx)
      ErasedAst.Expr.App(op, List(visitExp(base)), tpe, loc)

    case FinalAst.Expression.Tuple(exps, tpe, loc) =>
      val op = AtomicOp.Tuple
      ErasedAst.Expr.App(op, exps.map(visitExp), tpe, loc)

    case FinalAst.Expression.RecordEmpty(tpe, loc) =>
      val op = AtomicOp.RecordEmpty
      ErasedAst.Expr.App(op, Nil, tpe, loc)

    case FinalAst.Expression.RecordSelect(exp, field, tpe, loc) =>
      val op = AtomicOp.RecordSelect(field)
      ErasedAst.Expr.App(op, List(visitExp(exp)), tpe, loc)

    case FinalAst.Expression.RecordExtend(field, exp1, exp2, tpe, loc) =>
      val op = AtomicOp.RecordExtend(field)
      ErasedAst.Expr.App(op, List(visitExp(exp1), visitExp(exp2)), tpe, loc)

    case FinalAst.Expression.RecordRestrict(field, exp, tpe, loc) =>
      val op = AtomicOp.RecordRestrict(field)
      ErasedAst.Expr.App(op, List(visitExp(exp)), tpe, loc)

    case FinalAst.Expression.ArrayLit(exps, tpe, loc) =>
      val op = AtomicOp.ArrayLit
      ErasedAst.Expr.App(op, exps.map(visitExp), tpe, loc)

    case FinalAst.Expression.ArrayNew(exp1, exp2, tpe, loc) =>
      val op = AtomicOp.ArrayNew
      ErasedAst.Expr.App(op, List(visitExp(exp1), visitExp(exp2)), tpe, loc)

    case FinalAst.Expression.ArrayLoad(exp1, exp2, tpe, loc) =>
      val op = AtomicOp.ArrayLoad
      ErasedAst.Expr.App(op, List(visitExp(exp1), visitExp(exp2)), tpe, loc)

    case FinalAst.Expression.ArrayStore(exp1, exp2, exp3, tpe, loc) =>
      val op = AtomicOp.ArrayStore
      ErasedAst.Expr.App(op, List(visitExp(exp1), visitExp(exp2), visitExp(exp3)), tpe, loc)

    case FinalAst.Expression.ArrayLength(exp, tpe, loc) =>
      val op = AtomicOp.ArrayLength
      ErasedAst.Expr.App(op, List(visitExp(exp)), tpe, loc)

    case FinalAst.Expression.Ref(exp, tpe, loc) =>
      val op = AtomicOp.Ref
      ErasedAst.Expr.App(op, List(visitExp(exp)), tpe, loc)

    case FinalAst.Expression.Deref(exp, tpe, loc) =>
      val op = AtomicOp.Deref
      ErasedAst.Expr.App(op, List(visitExp(exp)), tpe, loc)

    case FinalAst.Expression.Assign(exp1, exp2, tpe, loc) =>
      val op = AtomicOp.Assign
      ErasedAst.Expr.App(op, List(visitExp(exp1), visitExp(exp2)), tpe, loc)

    case FinalAst.Expression.InstanceOf(exp, clazz, loc) =>
      val op = AtomicOp.InstanceOf(clazz)
      val tpe = MonoType.Bool
      ErasedAst.Expr.App(op, List(visitExp(exp)), tpe, loc)

    case FinalAst.Expression.Cast(exp, tpe, loc) =>
      val op = AtomicOp.Cast
      ErasedAst.Expr.App(op, List(visitExp(exp)), tpe, loc)

    case FinalAst.Expression.TryCatch(exp, rules0, tpe, loc) =>
      val rules = rules0.map {
        case FinalAst.CatchRule(catchSym, catchClazz, catchExp) =>
          ErasedAst.CatchRule(catchSym, catchClazz, visitExp(catchExp))
      }
      ErasedAst.Expr.TryCatch(visitExp(exp), rules, tpe, loc)

    case FinalAst.Expression.InvokeConstructor(constructor, exps, tpe, loc) =>
      val op = AtomicOp.InvokeConstructor(constructor)
      ErasedAst.Expr.App(op, exps.map(visitExp), tpe, loc)

    case FinalAst.Expression.InvokeMethod(method, exp, exps, tpe, loc) =>
      val op = AtomicOp.InvokeMethod(method)
      ErasedAst.Expr.App(op, visitExp(exp) :: exps.map(visitExp), tpe, loc)

    case FinalAst.Expression.InvokeStaticMethod(method, exps, tpe, loc) =>
      val op = AtomicOp.InvokeStaticMethod(method)
      ErasedAst.Expr.App(op, exps.map(visitExp), tpe, loc)

    case FinalAst.Expression.GetField(field, exp, tpe, loc) =>
      val op = AtomicOp.GetField(field)
      ErasedAst.Expr.App(op, List(visitExp(exp)), tpe, loc)

    case FinalAst.Expression.PutField(field, exp1, exp2, tpe, loc) =>
      val op = AtomicOp.PutField(field)
      ErasedAst.Expr.App(op, List(visitExp(exp1), visitExp(exp2)), tpe, loc)

    case FinalAst.Expression.GetStaticField(field, tpe, loc) =>
      val op = AtomicOp.GetStaticField(field)
      ErasedAst.Expr.App(op, Nil, tpe, loc)

    case FinalAst.Expression.PutStaticField(field, exp, tpe, loc) =>
      val op = AtomicOp.PutStaticField(field)
      ErasedAst.Expr.App(op, List(visitExp(exp)), tpe, loc)

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
      ErasedAst.Expr.App(op, List(visitExp(exp1), visitExp(exp2)), tpe, loc)

    case FinalAst.Expression.Lazy(exp, tpe, loc) =>
      val op = AtomicOp.Lazy
      ErasedAst.Expr.App(op, List(visitExp(exp)), tpe, loc)

    case FinalAst.Expression.Force(exp, tpe, loc) =>
      val op = AtomicOp.Force
      ErasedAst.Expr.App(op, List(visitExp(exp)), tpe, loc)

    case FinalAst.Expression.HoleError(sym, tpe, loc) =>
      val op = AtomicOp.HoleError(sym)
      ErasedAst.Expr.App(op, Nil, tpe, loc)

    case FinalAst.Expression.MatchError(tpe, loc) =>
      val op = AtomicOp.MatchError
      ErasedAst.Expr.App(op, Nil, tpe, loc)
  }

  private case class Context(closures: mutable.Set[ClosureInfo], anonClasses: mutable.Set[AnonClassInfo])

}

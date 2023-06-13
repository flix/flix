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
import ca.uwaterloo.flix.language.ast.{AtomicOp, ErasedAst, MonoTypedAst}
import ca.uwaterloo.flix.language.phase.jvm.AnonClassInfo

import scala.collection.mutable

object Eraser {

  def run(root: MonoTypedAst.Root)(implicit flix: Flix): ErasedAst.Root = flix.phase("Eraser") {

    //
    // A mutable set to hold all type information about all closures in the AST.
    //
    implicit val ctx: Context = Context(mutable.Set.empty)

    val defs = root.defs.map { case (k, v) => k -> visitDef(v) }
    val enums = root.enums.map { case (k, v) => k -> visitEnum(v) }

    ErasedAst.Root(defs, enums, root.entryPoint, root.sources, ctx.anonClasses.toSet)
  }

  private def visitEnum(enum0: MonoTypedAst.Enum): ErasedAst.Enum = {
    val cases = enum0.cases map { case (t, c) => t -> visitCase(c) }
    ErasedAst.Enum(enum0.ann, enum0.mod, enum0.sym, cases, enum0.tpe, enum0.loc)
  }

  private def visitDef(def0: MonoTypedAst.Def)(implicit ctx: Context): ErasedAst.Def = {
    val cs = def0.cparams.map(visitFormalParam)
    val fs = def0.fparams.map(visitFormalParam)
    val stmt = visitStmt(def0.stmt)
    ErasedAst.Def(def0.ann, def0.mod, def0.sym, cs, fs, stmt, def0.tpe, def0.loc)
  }

  private def visitExpr(exp0: MonoTypedAst.Expr)(implicit ctx: Context): ErasedAst.Expr = exp0 match {
    case MonoTypedAst.Expr.Cst(cst, tpe, loc) =>
      ErasedAst.Expr.Cst(cst, tpe, loc)

    case MonoTypedAst.Expr.Var(sym, tpe, loc) =>
      ErasedAst.Expr.Var(sym, tpe, loc)

    case MonoTypedAst.Expr.ApplyAtomic(op, exps, tpe, loc) =>
      val es = exps.map(visitExpr)
      ErasedAst.Expr.ApplyAtomic(op, es, tpe, loc)

    case MonoTypedAst.Expr.ApplyClo(exp, exps, ct, tpe, loc) =>
      ErasedAst.Expr.ApplyClo(visitExpr(exp), exps.map(visitExpr), ct, tpe, loc)

    case MonoTypedAst.Expr.ApplyDef(sym, exps, ct, tpe, loc) =>
      ErasedAst.Expr.ApplyDef(sym, exps.map(visitExpr), ct, tpe, loc)

    case MonoTypedAst.Expr.ApplySelfTail(sym, formals0, exps, tpe, loc) =>
      val formals = formals0.map {
        case MonoTypedAst.FormalParam(formalSym, formalTpe) => ErasedAst.FormalParam(formalSym, formalTpe)
      }
      ErasedAst.Expr.ApplySelfTail(sym, formals, exps.map(visitExpr), tpe, loc)

    case MonoTypedAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, loc) =>
      ErasedAst.Expr.IfThenElse(visitExpr(exp1), visitExpr(exp2), visitExpr(exp3), tpe, loc)

    case MonoTypedAst.Expr.Branch(exp, branches0, tpe, loc) =>
      val branches = branches0.map {
        case (branchLabel, branchExp) => (branchLabel, visitExpr(branchExp))
      }
      ErasedAst.Expr.Branch(visitExpr(exp), branches, tpe, loc)

    case MonoTypedAst.Expr.JumpTo(sym, tpe, loc) =>
      ErasedAst.Expr.JumpTo(sym, tpe, loc)

    case MonoTypedAst.Expr.Let(sym, exp1, exp2, tpe, loc) =>
      val e1 = visitExpr(exp1)
      val e2 = visitExpr(exp2)
      ErasedAst.Expr.Let(sym, e1, e2, tpe, loc)

    case MonoTypedAst.Expr.LetRec(varSym, index, defSym, exp1, exp2, tpe, loc) =>
      val e1 = visitExpr(exp1)
      val e2 = visitExpr(exp2)
      ErasedAst.Expr.LetRec(varSym, index, defSym, e1, e2, tpe, loc)

    case MonoTypedAst.Expr.Scope(sym, exp, tpe, loc) =>
      ErasedAst.Expr.Scope(sym, visitExpr(exp), tpe, loc)

    case MonoTypedAst.Expr.TryCatch(exp, rules0, tpe, loc) =>
      val rules = rules0.map {
        case MonoTypedAst.CatchRule(catchSym, catchClazz, catchExp) =>
          ErasedAst.CatchRule(catchSym, catchClazz, visitExpr(catchExp))
      }
      ErasedAst.Expr.TryCatch(visitExpr(exp), rules, tpe, loc)

    case MonoTypedAst.Expr.NewObject(name, clazz, tpe, methods0, loc) =>
      val methods = methods0.map {
        case MonoTypedAst.JvmMethod(ident, fparams, clo, retTpe, loc) =>
          val f = fparams.map(visitFormalParam)
          ErasedAst.JvmMethod(ident, f, visitExpr(clo), retTpe, loc)
      }
      ctx.anonClasses += AnonClassInfo(name, clazz, tpe, methods, loc)
      ErasedAst.Expr.NewObject(name, clazz, tpe, methods, loc)

    case MonoTypedAst.Expr.Spawn(exp1, exp2, tpe, loc) =>
      val op = AtomicOp.Spawn
      ErasedAst.Expr.ApplyAtomic(op, List(visitExpr(exp1), visitExpr(exp2)), tpe, loc)

  }

  private def visitStmt(stmt: MonoTypedAst.Stmt)(implicit ctx: Context): ErasedAst.Stmt = stmt match {
    case MonoTypedAst.Stmt.Ret(expr, tpe, loc) =>
      val e = visitExpr(expr)
      ErasedAst.Stmt.Ret(e, tpe, loc)
  }

  private def visitCase(case0: MonoTypedAst.Case): ErasedAst.Case = {
    ErasedAst.Case(case0.sym, case0.tpe, case0.loc)
  }

  private def visitFormalParam(p: MonoTypedAst.FormalParam): ErasedAst.FormalParam =
    ErasedAst.FormalParam(p.sym, p.tpe)

  private case class Context(anonClasses: mutable.Set[AnonClassInfo])

}

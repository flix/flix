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
import ca.uwaterloo.flix.language.ast.ErasedAst
import ca.uwaterloo.flix.language.ast.ReducedAst

object Eraser {

  def run(root: ReducedAst.Root)(implicit flix: Flix): ErasedAst.Root = flix.phase("Eraser") {

    val defs = root.defs.map { case (k, v) => k -> visitDef(v) }
    val enums = root.enums.map { case (k, v) => k -> visitEnum(v) }

    val anonClasses = root.anonClasses.map {
      case ReducedAst.AnonClass(name, clazz, tpe, methods, loc) =>
        val ms = methods.map {
          case ReducedAst.JvmMethodSpec(ident, fparams, retTpe, _, loc) =>
            val fs = fparams.map(visitFormalParam)
            ErasedAst.JvmMethodSpec(ident, fs, retTpe, loc)
        }
        ErasedAst.AnonClass(name, clazz, tpe, ms, loc)
    }

    ErasedAst.Root(defs, enums, root.entryPoint, root.sources, anonClasses)
  }

  private def visitEnum(enum0: ReducedAst.Enum): ErasedAst.Enum = {
    val cases = enum0.cases map { case (t, c) => t -> visitCase(c) }
    ErasedAst.Enum(enum0.ann, enum0.mod, enum0.sym, cases, enum0.tpe, enum0.loc)
  }

  private def visitDef(def0: ReducedAst.Def): ErasedAst.Def = {
    val cs = def0.cparams.map(visitFormalParam)
    val fs = def0.fparams.map(visitFormalParam)
    val stmt = visitStmt(def0.stmt)
    ErasedAst.Def(def0.ann, def0.mod, def0.sym, cs, fs, stmt, def0.tpe, def0.loc)
  }

  private def visitExpr(exp0: ReducedAst.Expr): ErasedAst.Expr = exp0 match {
    case ReducedAst.Expr.Cst(cst, tpe, loc) =>
      ErasedAst.Expr.Cst(cst, tpe, loc)

    case ReducedAst.Expr.Var(sym, tpe, loc) =>
      ErasedAst.Expr.Var(sym, tpe, loc)

    case ReducedAst.Expr.ApplyAtomic(op, exps, tpe, _, loc) =>
      val es = exps.map(visitExpr)
      ErasedAst.Expr.ApplyAtomic(op, es, tpe, loc)

    case ReducedAst.Expr.ApplyClo(exp, exps, ct, tpe, _, loc) =>
      ErasedAst.Expr.ApplyClo(visitExpr(exp), exps.map(visitExpr), ct, tpe, loc)

    case ReducedAst.Expr.ApplyDef(sym, exps, ct, tpe, _, loc) =>
      ErasedAst.Expr.ApplyDef(sym, exps.map(visitExpr), ct, tpe, loc)

    case ReducedAst.Expr.ApplySelfTail(sym, formals0, exps, tpe, _, loc) =>
      val formals = formals0.map(visitFormalParam)
      ErasedAst.Expr.ApplySelfTail(sym, formals, exps.map(visitExpr), tpe, loc)

    case ReducedAst.Expr.IfThenElse(exp1, exp2, exp3, tpe, _, loc) =>
      ErasedAst.Expr.IfThenElse(visitExpr(exp1), visitExpr(exp2), visitExpr(exp3), tpe, loc)

    case ReducedAst.Expr.Branch(exp, branches0, tpe, _, loc) =>
      val branches = branches0.map {
        case (branchLabel, branchExp) => (branchLabel, visitExpr(branchExp))
      }
      ErasedAst.Expr.Branch(visitExpr(exp), branches, tpe, loc)

    case ReducedAst.Expr.JumpTo(sym, tpe, _, loc) =>
      ErasedAst.Expr.JumpTo(sym, tpe, loc)

    case ReducedAst.Expr.Let(sym, exp1, exp2, tpe, _, loc) =>
      val e1 = visitExpr(exp1)
      val e2 = visitExpr(exp2)
      ErasedAst.Expr.Let(sym, e1, e2, tpe, loc)

    case ReducedAst.Expr.LetRec(varSym, index, defSym, exp1, exp2, tpe, _, loc) =>
      val e1 = visitExpr(exp1)
      val e2 = visitExpr(exp2)
      ErasedAst.Expr.LetRec(varSym, index, defSym, e1, e2, tpe, loc)

    case ReducedAst.Expr.Scope(sym, exp, tpe, _, loc) =>
      ErasedAst.Expr.Scope(sym, visitExpr(exp), tpe, loc)

    case ReducedAst.Expr.TryCatch(exp, rules0, tpe, _, loc) =>
      val rules = rules0.map {
        case ReducedAst.CatchRule(catchSym, catchClazz, catchExp) =>
          ErasedAst.CatchRule(catchSym, catchClazz, visitExpr(catchExp))
      }
      ErasedAst.Expr.TryCatch(visitExpr(exp), rules, tpe, loc)

    case ReducedAst.Expr.NewObject(name, clazz, tpe, _, methods0, loc) =>
      val methods = methods0.map {
        case ReducedAst.JvmMethodImpl(ident, fparams, clo, retTpe, _, loc) =>
          val f = fparams.map(visitFormalParam)
          ErasedAst.JvmMethodImpl(ident, f, visitExpr(clo), retTpe, loc)
      }
      ErasedAst.Expr.NewObject(name, clazz, tpe, methods, loc)
  }

  private def visitStmt(stmt: ReducedAst.Stmt): ErasedAst.Stmt = stmt match {
    case ReducedAst.Stmt.Ret(expr, tpe, loc) =>
      val e = visitExpr(expr)
      ErasedAst.Stmt.Ret(e, tpe, loc)
  }

  private def visitCase(case0: ReducedAst.Case): ErasedAst.Case = {
    ErasedAst.Case(case0.sym, case0.tpe, case0.loc)
  }

  private def visitFormalParam(p: ReducedAst.FormalParam): ErasedAst.FormalParam =
    ErasedAst.FormalParam(p.sym, p.mod, p.tpe, p.loc)

}

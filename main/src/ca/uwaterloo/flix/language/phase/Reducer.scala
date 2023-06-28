/*
 * Copyright 2023 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast._

object Reducer {

  def run(root: LiftedAst.Root)(implicit flix: Flix): ReducedAst.Root = flix.phase("CallByValue") {

    val newDefs = root.defs.map {
      case (sym, d) => sym -> visitDef(d)
    }
    val newEnums = root.enums.map {
      case (sym, d) => sym -> visitEnum(d)
    }

    ReducedAst.Root(newDefs, newEnums, root.entryPoint, root.sources)
  }

  private def visitDef(d: LiftedAst.Def): ReducedAst.Def = d match {
    case LiftedAst.Def(ann, mod, sym, cparams, fparams, exp, tpe, purity, loc) =>
      val cs = cparams.map(visitFormalParam)
      val fs = fparams.map(visitFormalParam)
      val e = visitExpr(exp)
      val stmt = ReducedAst.Stmt.Ret(e, e.tpe, e.loc)
      ReducedAst.Def(ann, mod, sym, cs, fs, stmt, tpe, purity, loc)
  }

  private def visitEnum(d: LiftedAst.Enum): ReducedAst.Enum = d match {
    case LiftedAst.Enum(ann, mod, sym, cases0, tpe, loc) =>
      val cases = cases0.map {
        case (sym, caze) => sym -> visitCase(caze)
      }
      ReducedAst.Enum(ann, mod, sym, cases, tpe, loc)
  }

  private def visitExpr(exp0: LiftedAst.Expression): ReducedAst.Expr = exp0 match {
    case LiftedAst.Expression.Cst(cst, tpe, loc) => ReducedAst.Expr.Cst(cst, tpe, loc)

    case LiftedAst.Expression.Var(sym, tpe, loc) => ReducedAst.Expr.Var(sym, tpe, loc)

    case LiftedAst.Expression.ApplyAtomic(op, exps, tpe, purity, loc) =>
      val es = exps.map(visitExpr)
      ReducedAst.Expr.ApplyAtomic(op, es, tpe, purity, loc)

    case LiftedAst.Expression.ApplyClo(exp, exps, tpe, purity, loc) =>
      val e = visitExpr(exp)
      val es = exps.map(visitExpr)
      ReducedAst.Expr.ApplyClo(e, es, Ast.CallType.NonTailCall, tpe, purity, loc)

    case LiftedAst.Expression.ApplyDef(sym, exps, tpe, purity, loc) =>
      val es = exps.map(visitExpr)
      ReducedAst.Expr.ApplyDef(sym, es, Ast.CallType.NonTailCall, tpe, purity, loc)

    case LiftedAst.Expression.ApplyCloTail(exp, exps, tpe, purity, loc) =>
      val e = visitExpr(exp)
      val es = exps.map(visitExpr)
      ReducedAst.Expr.ApplyClo(e, es, Ast.CallType.TailCall, tpe, purity, loc)

    case LiftedAst.Expression.ApplyDefTail(sym, exps, tpe, purity, loc) =>
      val es = exps.map(visitExpr)
      ReducedAst.Expr.ApplyDef(sym, es, Ast.CallType.TailCall, tpe, purity, loc)

    case LiftedAst.Expression.ApplySelfTail(sym, formals, exps, tpe, purity, loc) =>
      val fs = formals.map(visitFormalParam)
      val as = exps.map(visitExpr)
      ReducedAst.Expr.ApplySelfTail(sym, fs, as, tpe, purity, loc)

    case LiftedAst.Expression.IfThenElse(exp1, exp2, exp3, tpe, purity, loc) =>
      val e1 = visitExpr(exp1)
      val e2 = visitExpr(exp2)
      val e3 = visitExpr(exp3)
      ReducedAst.Expr.IfThenElse(e1, e2, e3, tpe, purity, loc)

    case LiftedAst.Expression.Branch(exp, branches, tpe, purity, loc) =>
      val e = visitExpr(exp)
      val bs = branches map {
        case (label, body) => label -> visitExpr(body)
      }
      ReducedAst.Expr.Branch(e, bs, tpe, purity, loc)

    case LiftedAst.Expression.JumpTo(sym, tpe, purity, loc) =>
      ReducedAst.Expr.JumpTo(sym, tpe, purity, loc)

    case LiftedAst.Expression.Let(sym, exp1, exp2, tpe, purity, loc) =>
      val e1 = visitExpr(exp1)
      val e2 = visitExpr(exp2)
      ReducedAst.Expr.Let(sym, e1, e2, tpe, purity, loc)

    case LiftedAst.Expression.LetRec(varSym, index, defSym, exp1, exp2, tpe, purity, loc) =>
      val e1 = visitExpr(exp1)
      val e2 = visitExpr(exp2)
      ReducedAst.Expr.LetRec(varSym, index, defSym, e1, e2, tpe, purity, loc)

    case LiftedAst.Expression.Scope(sym, exp, tpe, purity, loc) =>
      val e = visitExpr(exp)
      ReducedAst.Expr.Scope(sym, e, tpe, purity, loc)

    case LiftedAst.Expression.TryCatch(exp, rules, tpe, purity, loc) =>
      val e = visitExpr(exp)
      val rs = rules map {
        case LiftedAst.CatchRule(sym, clazz, body) =>
          val b = visitExpr(body)
          ReducedAst.CatchRule(sym, clazz, b)
      }
      ReducedAst.Expr.TryCatch(e, rs, tpe, purity, loc)

    case LiftedAst.Expression.TryWith(exp, effUse, rules, tpe, purity, loc) =>
      // TODO AE erasing to unit for now
      ReducedAst.Expr.Cst(Ast.Constant.Unit, Type.Unit, loc)

    case LiftedAst.Expression.Do(op, exps, tpe, purity, loc) =>
      // TODO AE erasing to unit for now
      ReducedAst.Expr.Cst(Ast.Constant.Unit, Type.Unit, loc)

    case LiftedAst.Expression.Resume(exp, tpe, loc) =>
      // TODO AE erasing to unit for now
      ReducedAst.Expr.Cst(Ast.Constant.Unit, Type.Unit, loc)

    case LiftedAst.Expression.NewObject(name, clazz, tpe, purity, methods, loc) =>
      val ms = methods.map(visitJvmMethod)
      ReducedAst.Expr.NewObject(name, clazz, tpe, purity, ms, loc)

    case LiftedAst.Expression.HoleError(sym, tpe, loc) =>
      val op = AtomicOp.HoleError(sym)
      ReducedAst.Expr.ApplyAtomic(op, Nil, tpe, Purity.Pure, loc)

    case LiftedAst.Expression.MatchError(tpe, loc) =>
      val op = AtomicOp.MatchError
      ReducedAst.Expr.ApplyAtomic(op, Nil, tpe, Purity.Pure, loc)
  }

  private def visitCase(caze: LiftedAst.Case): ReducedAst.Case = caze match {
    case LiftedAst.Case(sym, tpe, loc) => ReducedAst.Case(sym, tpe, loc)
  }

  private def visitFormalParam(fparam: LiftedAst.FormalParam): ReducedAst.FormalParam = fparam match {
    case LiftedAst.FormalParam(sym, mod, tpe, loc) => ReducedAst.FormalParam(sym, mod, tpe, loc)
  }

  private def visitJvmMethod(m: LiftedAst.JvmMethod): ReducedAst.JvmMethod = m match {
    case LiftedAst.JvmMethod(ident, fparams, clo, retTpe, purity, loc) =>
      val c = visitExpr(clo)
      val fs = fparams.map(visitFormalParam)
      ReducedAst.JvmMethod(ident, fs, c, retTpe, purity, loc)
  }

}

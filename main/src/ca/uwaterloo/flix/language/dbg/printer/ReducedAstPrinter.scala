/*
 * Copyright 2023 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.dbg.printer

import ca.uwaterloo.flix.language.ast.ReducedAst.{Expr, Stmt}
import ca.uwaterloo.flix.language.ast.{ReducedAst, Symbol}
import ca.uwaterloo.flix.language.dbg.DocAst
import ca.uwaterloo.flix.util.collection.MapOps

object ReducedAstPrinter {

  /**
    * Returns the [[DocAst.Program]] representation of `root`.
    */
  def print(root: ReducedAst.Root): DocAst.Program = {
    val enums = root.enums.values.map {
      case ReducedAst.Enum(ann, mod, sym, cases0, _, _) =>
        val cases = cases0.values.map {
          case ReducedAst.Case(sym, tpe, _) =>
            DocAst.Case(sym, MonoTypePrinter.print(tpe))
        }.toList
        DocAst.Enum(ann, mod, sym, Nil, cases)
    }.toList
    val defs = root.defs.values.map {
      case ReducedAst.Def(ann, mod, sym, cparams, fparams, stmt, tpe, _, _) =>
        DocAst.Def(
          ann,
          mod,
          sym,
          (cparams ++ fparams).map(printFormalParam),
          MonoTypePrinter.print(tpe),
          print(stmt)
        )
    }.toList
    DocAst.Program(enums, defs)
  }

  /**
    * Returns the [[DocAst.Expression]] representation of `e`.
    */
  def print(e: ReducedAst.Expr): DocAst.Expression = e match {
    case Expr.Cst(cst, _, _) => ConstantPrinter.print(cst)
    case Expr.Var(sym, _, _) => printVarSym(sym)
    case Expr.ApplyAtomic(op, exps, tpe, _, _) => OpPrinter.print(op, exps.map(print), MonoTypePrinter.print(tpe))
    case Expr.ApplyClo(exp, exps, ct, _, _, _) => DocAst.Expression.ApplyClo(print(exp), exps.map(print), Some(ct))
    case Expr.ApplyDef(sym, exps, ct, _, _, _) => DocAst.Expression.ApplyDef(sym, exps.map(print), Some(ct))
    case Expr.ApplySelfTail(sym, _, actuals, _, _, _) => DocAst.Expression.ApplySelfTail(sym, actuals.map(print))
    case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) => DocAst.Expression.IfThenElse(print(exp1), print(exp2), print(exp3))
    case Expr.Branch(exp, branches, _, _, _) => DocAst.Expression.Branch(print(exp), MapOps.mapValues(branches)(print))
    case Expr.JumpTo(sym, _, _, _) => DocAst.Expression.JumpTo(sym)
    case Expr.Let(sym, exp1, exp2, _, _, _) => DocAst.Expression.Let(printVarSym(sym), Some(MonoTypePrinter.print(exp1.tpe)), print(exp1), print(exp2))
    case Expr.LetRec(varSym, _, _, exp1, exp2, _, _, _) => DocAst.Expression.LetRec(printVarSym(varSym), Some(MonoTypePrinter.print(exp1.tpe)), print(exp1), print(exp2))
    case Expr.Scope(sym, exp, _, _, _) => DocAst.Expression.Scope(printVarSym(sym), print(exp))
    case Expr.TryCatch(exp, rules, _, _, _) => DocAst.Expression.TryCatch(print(exp), rules.map {
      case ReducedAst.CatchRule(sym, clazz, exp) => (sym, clazz, print(exp))
    })
    case Expr.NewObject(name, clazz, tpe, _, methods, exps, _) => DocAst.Expression.NewObject(name, clazz, MonoTypePrinter.print(tpe), methods.zip(exps).map(printJvmMethod))
  }

  /**
    * Returns the [[DocAst.Expression]] representation of `stmt`.
    */
  def print(stmt: ReducedAst.Stmt): DocAst.Expression = stmt match {
    case Stmt.Ret(expr, _, _) => DocAst.Expression.Ret(print(expr))
  }

  /**
    * Returns the [[DocAst.Expression.Ascription]] representation of `fp`.
    */
  private def printFormalParam(fp: ReducedAst.FormalParam): DocAst.Expression.Ascription = {
    val ReducedAst.FormalParam(sym, _, tpe, _) = fp
    DocAst.Expression.Ascription(printVarSym(sym), MonoTypePrinter.print(tpe))
  }

  /**
    * Returns the [[DocAst.Expression]] representation of `sym`.
    */
  private def printVarSym(sym: Symbol.VarSym): DocAst.Expression =
    DocAst.Expression.Var(sym)

  /**
    * Returns the [[DocAst.JvmMethod]] representation of `method`.
    */
  private def printJvmMethod(method: (ReducedAst.JvmMethod, ReducedAst.Expr)): DocAst.JvmMethod = method match {
    case (m, clo) => DocAst.JvmMethod(m.ident, m.fparams map printFormalParam, print(clo), MonoTypePrinter.print(m.tpe))
  }
}

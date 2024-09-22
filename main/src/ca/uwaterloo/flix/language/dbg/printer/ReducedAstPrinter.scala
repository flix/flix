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

import ca.uwaterloo.flix.language.ast.ReducedAst.Expr
import ca.uwaterloo.flix.language.ast.{ReducedAst, Symbol}
import ca.uwaterloo.flix.language.dbg.DocAst
import ca.uwaterloo.flix.util.collection.MapOps

object ReducedAstPrinter {

  /**
    * Returns the [[DocAst.Program]] representation of `root`.
    */
  def print(root: ReducedAst.Root): DocAst.Program = {
    val defs = root.defs.values.map {
      case ReducedAst.Def(ann, mod, sym, cparams, fparams, _, _, stmt, tpe, _, _) =>
        DocAst.Def(
          ann,
          mod,
          sym,
          (cparams ++ fparams).map(printFormalParam),
          MonoTypePrinter.print(tpe),
          PurityPrinter.print(stmt.purity),
          print(stmt)
        )
    }.toList
    DocAst.Program(Nil, defs)
  }

  /**
    * Returns the [[DocAst.Expr]] representation of `e`.
    */
  def print(e: ReducedAst.Expr): DocAst.Expr = e match {
    case Expr.Cst(cst, _, _) => ConstantPrinter.print(cst)
    case Expr.Var(sym, _, _) => printVarSym(sym)
    case Expr.ApplyAtomic(op, exps, tpe, _, _) => OpPrinter.print(op, exps.map(print), MonoTypePrinter.print(tpe))
    case Expr.ApplyClo(exp, exps, ct, _, _, _) => DocAst.Expr.ApplyClo(print(exp), exps.map(print), Some(ct))
    case Expr.ApplyDef(sym, exps, ct, _, _, _) => DocAst.Expr.ApplyDef(sym, exps.map(print), Some(ct))
    case Expr.ApplySelfTail(sym, actuals, _, _, _) => DocAst.Expr.ApplySelfTail(sym, actuals.map(print))
    case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) => DocAst.Expr.IfThenElse(print(exp1), print(exp2), print(exp3))
    case Expr.Branch(exp, branches, _, _, _) => DocAst.Expr.Branch(print(exp), MapOps.mapValues(branches)(print))
    case Expr.JumpTo(sym, _, _, _) => DocAst.Expr.JumpTo(sym)
    case Expr.Let(sym, exp1, exp2, _, _, _) => DocAst.Expr.Let(printVarSym(sym), Some(MonoTypePrinter.print(exp1.tpe)), print(exp1), print(exp2))
    case Expr.LetRec(varSym, _, _, exp1, exp2, _, _, _) => DocAst.Expr.LetRec(printVarSym(varSym), Some(MonoTypePrinter.print(exp1.tpe)), print(exp1), print(exp2))
    case Expr.Stmt(exp1, exp2, _, _, _) => DocAst.Expr.Stm(print(exp1), print(exp2))
    case Expr.Scope(sym, exp, _, _, _) => DocAst.Expr.Scope(printVarSym(sym), print(exp))
    case Expr.TryCatch(exp, rules, _, _, _) => DocAst.Expr.TryCatch(print(exp), rules.map {
      case ReducedAst.CatchRule(sym, clazz, exp) => (sym, clazz, print(exp))
    })
    case Expr.TryWith(exp, effUse, rules, _, _, _, _) => DocAst.Expr.TryWith(print(exp), effUse.sym, rules.map {
      case ReducedAst.HandlerRule(op, fparams, exp) =>
        (op.sym, fparams.map(printFormalParam), print(exp))
    })
    case Expr.Do(op, exps, _, _, _) => DocAst.Expr.Do(op.sym, exps.map(print))
    case Expr.NewObject(name, clazz, tpe, _, methods, _) => DocAst.Expr.NewObject(name, clazz, MonoTypePrinter.print(tpe), methods.map(printJvmMethod))
  }

  /**
    * Returns the [[DocAst.Expr.Ascription]] representation of `fp`.
    */
  private def printFormalParam(fp: ReducedAst.FormalParam): DocAst.Expr.Ascription = {
    val ReducedAst.FormalParam(sym, _, tpe, _) = fp
    DocAst.Expr.Ascription(printVarSym(sym), MonoTypePrinter.print(tpe))
  }

  /**
    * Returns the [[DocAst.Expr]] representation of `sym`.
    */
  private def printVarSym(sym: Symbol.VarSym): DocAst.Expr =
    DocAst.Expr.Var(sym)

  /**
    * Returns the [[DocAst.JvmMethod]] representation of `method`.
    */
  private def printJvmMethod(method: ReducedAst.JvmMethod): DocAst.JvmMethod = method match {
    case ReducedAst.JvmMethod(ident, fparams, exp, tpe, _, _) =>
      DocAst.JvmMethod(ident, fparams map printFormalParam, print(exp), MonoTypePrinter.print(tpe))
  }
}

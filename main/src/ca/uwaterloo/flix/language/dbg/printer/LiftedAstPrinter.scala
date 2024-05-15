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

import ca.uwaterloo.flix.language.ast.LiftedAst.Expr._
import ca.uwaterloo.flix.language.ast.{LiftedAst, Symbol}
import ca.uwaterloo.flix.language.dbg.DocAst
import ca.uwaterloo.flix.util.collection.MapOps

object LiftedAstPrinter {

  /**
    * Returns the [[DocAst.Program]] representation of `root`.
    */
  def print(root: LiftedAst.Root): DocAst.Program = {
    val defs = root.defs.values.map {
      case LiftedAst.Def(ann, mod, sym, cparams, fparams, exp, tpe, purity, _) =>
        DocAst.Def(
          ann,
          mod,
          sym,
          (cparams ++ fparams).map(printFormalParam),
          MonoTypePrinter.print(tpe),
          PurityPrinter.print(purity),
          print(exp)
        )
    }.toList
    DocAst.Program(Nil, defs)
  }

  /**
    * Returns the [[DocAst.Expression]] representation of `e`.
    */
  def print(e: LiftedAst.Expr): DocAst.Expression = e match {
    case Cst(cst, _, _) => ConstantPrinter.print(cst)
    case Var(sym, _, _) => printVarSym(sym)
    case ApplyAtomic(op, exps, tpe, _, _) => OpPrinter.print(op, exps.map(print), MonoTypePrinter.print(tpe))
    case ApplyClo(exp, exps, _, _, _) => DocAst.Expression.ApplyClo(print(exp), exps.map(print), None)
    case ApplyDef(sym, args, _, _, _) => DocAst.Expression.ApplyDef(sym, args.map(print), None)
    case IfThenElse(exp1, exp2, exp3, _, _, _) => DocAst.Expression.IfThenElse(print(exp1), print(exp2), print(exp3))
    case Branch(exp, branches, _, _, _) => DocAst.Expression.Branch(print(exp), MapOps.mapValues(branches)(print))
    case JumpTo(sym, _, _, _) => DocAst.Expression.JumpTo(sym)
    case Let(sym, exp1, exp2, _, _, _) => DocAst.Expression.Let(printVarSym(sym), Some(MonoTypePrinter.print(exp1.tpe)), print(exp1), print(exp2))
    case LetRec(varSym, _, _, exp1, exp2, _, _, _) => DocAst.Expression.LetRec(printVarSym(varSym), Some(MonoTypePrinter.print(exp1.tpe)), print(exp1), print(exp2))
    case Stm(exp1, exp2, _, _, _) => DocAst.Expression.Stm(print(exp1), print(exp2))
    case Scope(sym, exp, _, _, _) => DocAst.Expression.Scope(printVarSym(sym), print(exp))
    case TryCatch(exp, rules, _, _, _) => DocAst.Expression.TryCatch(print(exp), rules.map {
      case LiftedAst.CatchRule(sym, clazz, rexp) => (sym, clazz, print(rexp))
    })
    case TryWith(exp, effUse, rules, _, _, _) => DocAst.Expression.TryWith(print(exp), effUse.sym, rules.map {
      case LiftedAst.HandlerRule(op, fparams, exp) =>
        (op.sym, fparams.map(printFormalParam), print(exp))
    })
    case Do(op, exps, _, _, _) => DocAst.Expression.Do(op.sym, exps.map(print))
    case NewObject(name, clazz, tpe, _, methods, _) => DocAst.Expression.NewObject(name, clazz, MonoTypePrinter.print(tpe), methods.map {
      case LiftedAst.JvmMethod(ident, fparams, clo, retTpe, _, _) =>
        DocAst.JvmMethod(ident, fparams.map(printFormalParam), print(clo), MonoTypePrinter.print(retTpe))
    })
  }

  /**
    * Returns the [[DocAst.Expression.Ascription]] representation of `fp`.
    */
  private def printFormalParam(fp: LiftedAst.FormalParam): DocAst.Expression.Ascription = {
    val LiftedAst.FormalParam(sym, _, tpe, _) = fp
    DocAst.Expression.Ascription(printVarSym(sym), MonoTypePrinter.print(tpe))
  }

  /**
    * Returns the [[DocAst.Expression]] representation of `sym`.
    */
  private def printVarSym(sym: Symbol.VarSym): DocAst.Expression =
    DocAst.Expression.Var(sym)

}

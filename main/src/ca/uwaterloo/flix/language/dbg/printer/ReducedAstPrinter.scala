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

import ca.uwaterloo.flix.language.ast.ReducedAst.Exp
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
          SimpleTypePrinter.print(tpe),
          PurityPrinter.print(stmt.purity),
          print(stmt)
        )
    }.toList
    DocAst.Program(Nil, defs, Nil)
  }

  /**
    * Returns the [[DocAst.Exp]] representation of `e`.
    */
  def print(e: ReducedAst.Exp): DocAst.Exp = e match {
    case Exp.Cst(cst, _) => ConstantPrinter.print(cst)
    case Exp.Var(sym, _, _) => printVarSym(sym)
    case Exp.ApplyAtomic(op, exps, tpe, purity, _) => OpPrinter.print(op, exps.map(print), SimpleTypePrinter.print(tpe), PurityPrinter.print(purity))
    case Exp.ApplyClo(exp1, exp2, ct, _, _, _) => DocAst.Exp.ApplyCloWithTail(print(exp1), List(print(exp2)), ct)
    case Exp.ApplyDef(sym, exps, ct, _, _, _) => DocAst.Exp.ApplyDefWithTail(sym, exps.map(print), ct)
    case Exp.ApplyOp(sym, exps, _, _, _) => DocAst.Exp.ApplyOp(sym, exps.map(print))
    case Exp.ApplySelfTail(sym, actuals, _, _, _) => DocAst.Exp.ApplySelfTail(sym, actuals.map(print))
    case Exp.IfThenElse(exp1, exp2, exp3, _, _, _) => DocAst.Exp.IfThenElse(print(exp1), print(exp2), print(exp3))
    case Exp.Branch(exp, branches, _, _, _) => DocAst.Exp.Branch(print(exp), MapOps.mapValues(branches)(print))
    case Exp.JumpTo(sym, _, _, _) => DocAst.Exp.JumpTo(sym)
    case Exp.Let(sym, exp1, exp2, _) => DocAst.Exp.Let(printVarSym(sym), Some(SimpleTypePrinter.print(exp1.tpe)), print(exp1), print(exp2))
    case Exp.Stmt(exp1, exp2, _) => DocAst.Exp.Stm(print(exp1), print(exp2))
    case Exp.Region(sym, exp, _, _, _) => DocAst.Exp.Region(printVarSym(sym), print(exp))
    case Exp.TryCatch(exp, rules, _, _, _) => DocAst.Exp.TryCatch(print(exp), rules.map {
      case ReducedAst.CatchRule(sym, clazz, body) => (sym, clazz, print(body))
    })
    case Exp.RunWith(exp, effUse, rules, _, _, _, _) => DocAst.Exp.RunWithHandler(print(exp), effUse.sym, rules.map {
      case ReducedAst.HandlerRule(op, fparams, body) =>
        (op.sym, fparams.map(printFormalParam), print(body))
    })
    case Exp.NewObject(name, clazz, tpe, _, methods, _) => DocAst.Exp.NewObject(name, clazz, SimpleTypePrinter.print(tpe), methods.map(printJvmMethod))
  }

  /**
    * Returns the [[DocAst.Exp.AscriptionTpe]] representation of `fp`.
    */
  private def printFormalParam(fp: ReducedAst.FormalParam): DocAst.Exp.AscriptionTpe = {
    val ReducedAst.FormalParam(sym, tpe) = fp
    DocAst.Exp.AscriptionTpe(printVarSym(sym), SimpleTypePrinter.print(tpe))
  }

  /**
    * Returns the [[DocAst.Exp]] representation of `sym`.
    */
  private def printVarSym(sym: Symbol.VarSym): DocAst.Exp =
    DocAst.Exp.Var(sym)

  /**
    * Returns the [[DocAst.JvmMethod]] representation of `method`.
    */
  private def printJvmMethod(method: ReducedAst.JvmMethod): DocAst.JvmMethod = method match {
    case ReducedAst.JvmMethod(ident, fparams, exp, tpe, _, _) =>
      DocAst.JvmMethod(ident, fparams map printFormalParam, print(exp), SimpleTypePrinter.print(tpe))
  }
}

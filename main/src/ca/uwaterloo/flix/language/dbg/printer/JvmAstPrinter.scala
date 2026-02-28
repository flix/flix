/*
 * Copyright 2025 Jonathan Lindegaard Starup
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

import ca.uwaterloo.flix.language.ast.JvmAst.Expr
import ca.uwaterloo.flix.language.ast.{JvmAst, Symbol}
import ca.uwaterloo.flix.language.dbg.DocAst
import ca.uwaterloo.flix.util.collection.MapOps

object JvmAstPrinter {

  /** Returns the [[DocAst.Program]] representation of `root`. */
  def print(root: JvmAst.Root): DocAst.Program = {
    val defs = root.defs.values.map {
      case JvmAst.Def(ann, mod, sym, cparams, fparams, _, _, stmt, tpe, _, _) =>
        DocAst.Def(
          ann,
          mod,
          sym,
          (cparams ++ fparams).map(printOffsetFormalParam),
          SimpleTypePrinter.print(tpe),
          PurityPrinter.print(stmt.purity),
          print(stmt)
        )
    }.toList
    DocAst.Program(Nil, defs, Nil)
  }

  /** Returns the [[DocAst.Expr]] representation of `e`. */
  def print(e: JvmAst.Expr): DocAst.Expr = e match {
    case Expr.Cst(cst, _) => ConstantPrinter.print(cst)
    case Expr.Var(sym, _, _, _) => printVarSym(sym)
    case Expr.ApplyAtomic(op, exps, tpe, purity, _) => OpPrinter.print(op, exps.map(print), SimpleTypePrinter.print(tpe), PurityPrinter.print(purity))
    case Expr.ApplyClo(exp1, exp2, ct, _, _, _) => DocAst.Expr.ApplyCloWithTail(print(exp1), List(print(exp2)), ct)
    case Expr.ApplyDef(sym, exps, ct, _, _, _) => DocAst.Expr.ApplyDefWithTail(sym, exps.map(print), ct)
    case Expr.ApplyOp(sym, exps, _, _, _) => DocAst.Expr.ApplyOp(sym, exps.map(print))
    case Expr.ApplySelfTail(sym, actuals, _, _, _) => DocAst.Expr.ApplySelfTail(sym, actuals.map(print))
    case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) => DocAst.Expr.IfThenElse(print(exp1), print(exp2), print(exp3))
    case Expr.Branch(exp, branches, _, _, _) => DocAst.Expr.Branch(print(exp), MapOps.mapValues(branches)(print))
    case Expr.JumpTo(sym, _, _, _) => DocAst.Expr.JumpTo(sym)
    case Expr.Let(sym, _, exp1, exp2, _) => DocAst.Expr.Let(printVarSym(sym), Some(SimpleTypePrinter.print(exp1.tpe)), print(exp1), print(exp2))
    case Expr.Stmt(exp1, exp2, _) => DocAst.Expr.Stm(print(exp1), print(exp2))
    case Expr.Region(sym, _, exp, _, _, _) => DocAst.Expr.Region(printVarSym(sym), print(exp))
    case Expr.TryCatch(exp, rules, _, _, _) => DocAst.Expr.TryCatch(print(exp), rules.map {
      case JvmAst.CatchRule(sym, _, clazz, body) => (sym, clazz, print(body))
    })
    case Expr.RunWith(exp, effUse, rules, _, _, _, _) => DocAst.Expr.RunWithHandler(print(exp), effUse.sym, rules.map {
      case JvmAst.HandlerRule(op, fparams, body) =>
        (op.sym, fparams.map(printFormalParam), print(body))
    })
    case Expr.NewObject(name, clazz, tpe, _, constructors, methods, _) => DocAst.Expr.NewObject(name, clazz, SimpleTypePrinter.print(tpe), constructors.map(printJvmConstructor), methods.map(printJvmMethod))
  }

  /** Returns the [[DocAst.Expr.AscriptionTpe]] representation of `fp`. */
  private def printFormalParam(fp: JvmAst.FormalParam): DocAst.Expr.AscriptionTpe = {
    val JvmAst.FormalParam(sym, tpe) = fp
    DocAst.Expr.AscriptionTpe(printVarSym(sym), SimpleTypePrinter.print(tpe))
  }

  /** Returns the [[DocAst.Expr.AscriptionTpe]] representation of `fp`. */
  private def printOffsetFormalParam(fp: JvmAst.OffsetFormalParam): DocAst.Expr.AscriptionTpe = {
    val JvmAst.OffsetFormalParam(sym, _, tpe) = fp
    DocAst.Expr.AscriptionTpe(printVarSym(sym), SimpleTypePrinter.print(tpe))
  }

  /** Returns the [[DocAst.Expr]] representation of `sym`. */
  private def printVarSym(sym: Symbol.VarSym): DocAst.Expr =
    DocAst.Expr.Var(sym)

  /** Returns the [[DocAst.JvmConstructor]] representation of `constructor`. */
  private def printJvmConstructor(constructor: JvmAst.JvmConstructor): DocAst.JvmConstructor = constructor match {
    case JvmAst.JvmConstructor(exp, tpe, _, _) =>
      DocAst.JvmConstructor(print(exp), SimpleTypePrinter.print(tpe))
  }

  /** Returns the [[DocAst.JvmMethod]] representation of `method`. */
  private def printJvmMethod(method: JvmAst.JvmMethod): DocAst.JvmMethod = method match {
    case JvmAst.JvmMethod(_, ident, fparams, exp, tpe, _, _) =>
      DocAst.JvmMethod(ident, fparams map printFormalParam, print(exp), SimpleTypePrinter.print(tpe))
  }
}

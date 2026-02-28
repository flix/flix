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

import ca.uwaterloo.flix.language.ast.SimplifiedAst.Expr.*
import ca.uwaterloo.flix.language.ast.{SimplifiedAst, Symbol}
import ca.uwaterloo.flix.language.dbg.DocAst
import ca.uwaterloo.flix.util.collection.MapOps

object SimplifiedAstPrinter {

  /**
    * Returns the [[DocAst.Program]] representation of `root`.
    */
  def print(root: SimplifiedAst.Root): DocAst.Program = {
    val defs = root.defs.values.map {
      case SimplifiedAst.Def(ann, mod, sym, formals, exp, tpe, _, _) =>
        DocAst.Def(
          ann,
          mod,
          sym,
          formals.map(printFormalParam),
          SimpleTypePrinter.print(tpe),
          PurityPrinter.print(exp.purity),
          print(exp)
        )
    }.toList
    DocAst.Program(Nil, defs, Nil)
  }

  /**
    * Returns the [[DocAst.Expr]] representation of `e`.
    */
  def print(e: SimplifiedAst.Expr): DocAst.Expr = e match {
    case Cst(cst, _, _) => ConstantPrinter.print(cst)
    case Var(sym, _, _) => printVarSym(sym)
    case Lambda(fparams, exp, _, _) => DocAst.Expr.Lambda(fparams.map(printFormalParam), print(exp))
    case LambdaClosure(cparams, fparams, _, exp, _, _) => DocAst.Expr.Lambda((cparams ++ fparams).map(printFormalParam), print(exp))
    case ApplyAtomic(op, exps, tpe, purity, _) => OpPrinter.print(op, exps.map(print), SimpleTypePrinter.print(tpe), PurityPrinter.print(purity))
    case ApplyClo(exp1, exp2, _, _, _) => DocAst.Expr.ApplyClo(print(exp1), List(print(exp2)))
    case ApplyDef(sym, exps, _, _, _) => DocAst.Expr.ApplyDef(sym, exps.map(print))
    case ApplyOp(sym, exps, _, _, _) => DocAst.Expr.ApplyOp(sym, exps.map(print))
    case ApplyLocalDef(sym, exps, _, _, _) => DocAst.Expr.ApplyClo(printVarSym(sym), exps.map(print))
    case IfThenElse(exp1, exp2, exp3, _, _, _) => DocAst.Expr.IfThenElse(print(exp1), print(exp2), print(exp3))
    case Stm(exp1, exp2, _, _, _) => DocAst.Expr.Stm(print(exp1), print(exp2))
    case Branch(exp, branches, _, _, _) => DocAst.Expr.Branch(print(exp), MapOps.mapValues(branches)(print))
    case JumpTo(sym, _, _, _) => DocAst.Expr.JumpTo(sym)
    case Let(sym, exp1, exp2, _, _, _) => DocAst.Expr.Let(printVarSym(sym), Some(SimpleTypePrinter.print(exp1.tpe)), print(exp1), print(exp2))
    case LocalDef(sym, fparams, exp1, exp2, tpe, eff, _) => DocAst.Expr.LocalDef(printVarSym(sym), fparams.map(printFormalParam), Some(SimpleTypePrinter.print(tpe)), Some(PurityPrinter.print(eff)), print(exp1), print(exp2))
    case Region(sym, exp, _, _, _) => DocAst.Expr.Region(printVarSym(sym), print(exp))
    case TryCatch(exp, rules, _, _, _) => DocAst.Expr.TryCatch(print(exp), rules.map {
      case SimplifiedAst.CatchRule(sym, clazz, body) =>
        (sym, clazz, print(body))
    })
    case RunWith(exp, effUse, rules, _, _, _) => DocAst.Expr.RunWithHandler(print(exp), effUse.sym, rules.map {
      case SimplifiedAst.HandlerRule(op, fparams, body) =>
        (op.sym, fparams.map(printFormalParam), print(body))
    })
    case NewObject(name, clazz, tpe, _, constructors, methods, _) =>
      val cs = constructors.map {
        case SimplifiedAst.JvmConstructor(exp, retTpe, _, _) =>
          DocAst.JvmConstructor(print(exp), SimpleTypePrinter.print(retTpe))
      }
      val ms = methods.map {
        case SimplifiedAst.JvmMethod(ann, ident, fparams, exp, retTpe, _, _) =>
          DocAst.JvmMethod(ann.map(_.clazz.getSimpleName), ident, fparams.map(printFormalParam), print(exp), SimpleTypePrinter.print(retTpe))
      }
      DocAst.Expr.NewObject(name, clazz, SimpleTypePrinter.print(tpe), cs, ms)
  }

  /**
    * Returns the [[DocAst.Expr.AscriptionTpe]] representation of `fp`.
    */
  private def printFormalParam(fp: SimplifiedAst.FormalParam): DocAst.Expr.AscriptionTpe = {
    val SimplifiedAst.FormalParam(sym, tpe, _) = fp
    DocAst.Expr.AscriptionTpe(printVarSym(sym), SimpleTypePrinter.print(tpe))
  }

  /**
    * Returns the [[DocAst.Expr]] representation of `sym`.
    */
  private def printVarSym(sym: Symbol.VarSym): DocAst.Expr =
    DocAst.Expr.Var(sym)

}

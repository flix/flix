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
          MonoTypePrinter.print(tpe),
          PurityPrinter.print(exp.purity),
          print(exp)
        )
    }.toList
    DocAst.Program(Nil, defs)
  }

  /**
    * Returns the [[DocAst.Expr]] representation of `e`.
    */
  def print(e: SimplifiedAst.Expr): DocAst.Expr = e match {
    case Cst(cst, _, _) => ConstantPrinter.print(cst)
    case Var(sym, _, _) => printVarSym(sym)
    case Lambda(fparams, exp, _, _) => DocAst.Expr.Lambda(fparams.map(printFormalParam), print(exp))
    case LambdaClosure(cparams, fparams, _, exp, _, _) => DocAst.Expr.Lambda((cparams ++ fparams).map(printFormalParam), print(exp))
    case ApplyAtomic(op, exps, tpe, _, _) => OpPrinter.print(op, exps.map(print), MonoTypePrinter.print(tpe))
    case ApplyClo(exp, args, _, _, _) => DocAst.Expr.ApplyClo(print(exp), args.map(print), None)
    case ApplyDef(sym, exps, _, _, _) => DocAst.Expr.ApplyDef(sym, exps.map(print), None)
    case ApplyLocalDef(sym, exps, _, _, _) => DocAst.Expr.ApplyClo(printVarSym(sym), exps.map(print), None)
    case IfThenElse(exp1, exp2, exp3, _, _, _) => DocAst.Expr.IfThenElse(print(exp1), print(exp2), print(exp3))
    case Stm(exp1, exp2, _, _, _) => DocAst.Expr.Stm(print(exp1), print(exp2))
    case Branch(exp, branches, _, _, _) => DocAst.Expr.Branch(print(exp), MapOps.mapValues(branches)(print))
    case JumpTo(sym, _, _, _) => DocAst.Expr.JumpTo(sym)
    case Let(sym, exp1, exp2, _, _, _) => DocAst.Expr.Let(printVarSym(sym), Some(MonoTypePrinter.print(exp1.tpe)), print(exp1), print(exp2))
    case LocalDef(sym, _, exp1, exp2, _, _, _) => DocAst.Expr.LetRec(printVarSym(sym), Some(MonoTypePrinter.print(exp1.tpe)), print(exp1), print(exp2))
    case Scope(sym, exp, _, _, _) => DocAst.Expr.Scope(printVarSym(sym), print(exp))
    case TryCatch(exp, rules, _, _, _) => DocAst.Expr.TryCatch(print(exp), rules.map {
      case SimplifiedAst.CatchRule(sym, clazz, exp) =>
        (sym, clazz, print(exp))
    })
    case TryWith(exp, effUse, rules, _, _, _) => DocAst.Expr.TryWith(print(exp), effUse.sym, rules.map {
      case SimplifiedAst.HandlerRule(op, fparams, exp) =>
        (op.sym, fparams.map(printFormalParam), print(exp))
    })
    case Do(op, exps, _, _, _) => DocAst.Expr.Do(op.sym, exps.map(print))
    case NewObject(name, clazz, tpe, _, methods, _) => DocAst.Expr.NewObject(name, clazz, MonoTypePrinter.print(tpe), methods.map {
      case SimplifiedAst.JvmMethod(ident, fparams, exp, retTpe, _, _) =>
        DocAst.JvmMethod(ident, fparams.map(printFormalParam), print(exp), MonoTypePrinter.print(retTpe))
    })
  }

  /**
    * Returns the [[DocAst.Expr.Ascription]] representation of `fp`.
    */
  private def printFormalParam(fp: SimplifiedAst.FormalParam): DocAst.Expr.Ascription = {
    val SimplifiedAst.FormalParam(sym, _, tpe, _) = fp
    DocAst.Expr.Ascription(printVarSym(sym), MonoTypePrinter.print(tpe))
  }

  /**
    * Returns the [[DocAst.Expr]] representation of `sym`.
    */
  private def printVarSym(sym: Symbol.VarSym): DocAst.Expr =
    DocAst.Expr.Var(sym)

}

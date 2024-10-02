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

import ca.uwaterloo.flix.language.ast.LiftedAst.Expr.*
import ca.uwaterloo.flix.language.ast.{LiftedAst, Symbol}
import ca.uwaterloo.flix.language.dbg.DocAst
import ca.uwaterloo.flix.util.collection.MapOps

object LiftedAstPrinter {

  /**
    * Returns the [[DocAst.Program]] representation of `root`.
    */
  def print(root: LiftedAst.Root): DocAst.Program = {
    val defs = root.defs.values.map {
      case LiftedAst.Def(ann, mod, sym, cparams, fparams, exp, tpe, _) =>
        DocAst.Def(
          ann,
          mod,
          sym,
          (cparams ++ fparams).map(printFormalParam),
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
  def print(e: LiftedAst.Expr): DocAst.Expr = e match {
    case Cst(cst, _, _) => ConstantPrinter.print(cst)
    case Var(sym, _, _) => printVarSym(sym)
    case ApplyAtomic(op, exps, tpe, _, _) => OpPrinter.print(op, exps.map(print), MonoTypePrinter.print(tpe))
    case ApplyClo(exp, exps, _, _, _) => DocAst.Expr.ApplyClo(print(exp), exps.map(print), None)
    case ApplyDef(sym, args, _, _, _) => DocAst.Expr.ApplyDef(sym, args.map(print), None)
    case IfThenElse(exp1, exp2, exp3, _, _, _) => DocAst.Expr.IfThenElse(print(exp1), print(exp2), print(exp3))
    case Branch(exp, branches, _, _, _) => DocAst.Expr.Branch(print(exp), MapOps.mapValues(branches)(print))
    case JumpTo(sym, _, _, _) => DocAst.Expr.JumpTo(sym)
    case Let(sym, exp1, exp2, _, _, _) => DocAst.Expr.Let(printVarSym(sym), Some(MonoTypePrinter.print(exp1.tpe)), print(exp1), print(exp2))
    case LetRec(varSym, _, _, exp1, exp2, _, _, _) => DocAst.Expr.LetRec(printVarSym(varSym), Some(MonoTypePrinter.print(exp1.tpe)), print(exp1), print(exp2))
    case Stm(exp1, exp2, _, _, _) => DocAst.Expr.Stm(print(exp1), print(exp2))
    case Scope(sym, exp, _, _, _) => DocAst.Expr.Scope(printVarSym(sym), print(exp))
    case TryCatch(exp, rules, _, _, _) => DocAst.Expr.TryCatch(print(exp), rules.map {
      case LiftedAst.CatchRule(sym, clazz, rexp) => (sym, clazz, print(rexp))
    })
    case TryWith(exp, effUse, rules, _, _, _) => DocAst.Expr.TryWith(print(exp), effUse.sym, rules.map {
      case LiftedAst.HandlerRule(op, fparams, exp) =>
        (op.sym, fparams.map(printFormalParam), print(exp))
    })
    case Do(op, exps, _, _, _) => DocAst.Expr.Do(op.sym, exps.map(print))
    case NewObject(name, clazz, tpe, _, methods, _) => DocAst.Expr.NewObject(name, clazz, MonoTypePrinter.print(tpe), methods.map {
      case LiftedAst.JvmMethod(ident, fparams, clo, retTpe, _, _) =>
        DocAst.JvmMethod(ident, fparams.map(printFormalParam), print(clo), MonoTypePrinter.print(retTpe))
    })
  }

  /**
    * Returns the [[DocAst.Expr.Ascription]] representation of `fp`.
    */
  private def printFormalParam(fp: LiftedAst.FormalParam): DocAst.Expr.Ascription = {
    val LiftedAst.FormalParam(sym, _, tpe, _) = fp
    DocAst.Expr.Ascription(printVarSym(sym), MonoTypePrinter.print(tpe))
  }

  /**
    * Returns the [[DocAst.Expr]] representation of `sym`.
    */
  private def printVarSym(sym: Symbol.VarSym): DocAst.Expr =
    DocAst.Expr.Var(sym)

}

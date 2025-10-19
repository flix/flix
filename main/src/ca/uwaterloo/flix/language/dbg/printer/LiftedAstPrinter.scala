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

import ca.uwaterloo.flix.language.ast.LiftedAst.Exp.*
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
          SimpleTypePrinter.print(tpe),
          PurityPrinter.print(exp.purity),
          print(exp)
        )
    }.toList
    DocAst.Program(Nil, defs, Nil)
  }

  /**
    * Returns the [[DocAst.Exp]] representation of `e`.
    */
  def print(e: LiftedAst.Exp): DocAst.Exp = e match {
    case Cst(cst, _, _) => ConstantPrinter.print(cst)
    case Var(sym, _, _) => printVarSym(sym)
    case ApplyAtomic(op, exps, tpe, purity, _) => OpPrinter.print(op, exps.map(print), SimpleTypePrinter.print(tpe), PurityPrinter.print(purity))
    case ApplyClo(exp1, exp2, _, _, _) => DocAst.Exp.ApplyClo(print(exp1), List(print(exp2)))
    case ApplyDef(sym, args, _, _, _) => DocAst.Exp.ApplyDef(sym, args.map(print))
    case ApplyOp(sym, exps, _, _, _) => DocAst.Exp.ApplyOp(sym, exps.map(print))
    case IfThenElse(exp1, exp2, exp3, _, _, _) => DocAst.Exp.IfThenElse(print(exp1), print(exp2), print(exp3))
    case Branch(exp, branches, _, _, _) => DocAst.Exp.Branch(print(exp), MapOps.mapValues(branches)(print))
    case JumpTo(sym, _, _, _) => DocAst.Exp.JumpTo(sym)
    case Let(sym, exp1, exp2, _, _, _) => DocAst.Exp.Let(printVarSym(sym), Some(SimpleTypePrinter.print(exp1.tpe)), print(exp1), print(exp2))
    case Stm(exp1, exp2, _, _, _) => DocAst.Exp.Stm(print(exp1), print(exp2))
    case Region(sym, exp, _, _, _) => DocAst.Exp.Region(printVarSym(sym), print(exp))
    case TryCatch(exp, rules, _, _, _) => DocAst.Exp.TryCatch(print(exp), rules.map {
      case LiftedAst.CatchRule(sym, clazz, rexp) => (sym, clazz, print(rexp))
    })
    case RunWith(exp, effUse, rules, _, _, _) => DocAst.Exp.RunWithHandler(print(exp), effUse.sym, rules.map {
      case LiftedAst.HandlerRule(symUse, fparams, body) =>
        (symUse.sym, fparams.map(printFormalParam), print(body))
    })
    case NewObject(name, clazz, tpe, _, methods, _) => DocAst.Exp.NewObject(name, clazz, SimpleTypePrinter.print(tpe), methods.map {
      case LiftedAst.JvmMethod(ident, fparams, clo, retTpe, _, _) =>
        DocAst.JvmMethod(ident, fparams.map(printFormalParam), print(clo), SimpleTypePrinter.print(retTpe))
    })
  }

  /**
    * Returns the [[DocAst.Exp.AscriptionTpe]] representation of `fp`.
    */
  private def printFormalParam(fp: LiftedAst.FormalParam): DocAst.Exp.AscriptionTpe = {
    val LiftedAst.FormalParam(sym, tpe, _) = fp
    DocAst.Exp.AscriptionTpe(printVarSym(sym), SimpleTypePrinter.print(tpe))
  }

  /**
    * Returns the [[DocAst.Exp]] representation of `sym`.
    */
  private def printVarSym(sym: Symbol.VarSym): DocAst.Exp =
    DocAst.Exp.Var(sym)

}

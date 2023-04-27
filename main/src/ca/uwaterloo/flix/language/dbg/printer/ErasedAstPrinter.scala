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

import ca.uwaterloo.flix.language.ast.{ErasedAst, Symbol}
import ca.uwaterloo.flix.language.ast.ErasedAst.Expr._
import ca.uwaterloo.flix.language.ast.ErasedAst._
import ca.uwaterloo.flix.language.dbg.DocAst

object ErasedAstPrinter {

  /**
    * Returns the [[DocAst.Program]] representation of `root`.
    */
  def print(root: ErasedAst.Root): DocAst.Program = {
    val enums = root.enums.values.map {
      case ErasedAst.Enum(ann, mod, sym, cases0, _, _) =>
        val cases = cases0.values.map {
          case ErasedAst.Case(sym, _, _) => DocAst.Case(sym)
        }.toList
        DocAst.Enum(ann, mod, sym, cases)
    }.toList
    val defs = root.defs.values.map {
      case ErasedAst.Def(ann, mod, sym, formals, exp, tpe, _) =>
        DocAst.Def(
          ann,
          mod,
          sym,
          formals.map(printFormalParam),
          MonoTypePrinter.print(tpe),
          print(exp)
        )
    }.toList
    DocAst.Program(enums, defs)
  }

  /**
    * Returns the [[DocAst.Expression]] representation of `e`.
    */
  def print(e: ErasedAst.Expr): DocAst.Expression = e match {
    case Cst(cst, _, _) => ConstantPrinter.print(cst)
    case Var(sym, _, _) => printVarSym(sym)
    case Binary(sop, exp1, exp2, _, _) => DocAst.Expression.Binary(print(exp1), OperatorPrinter.print(sop), print(exp2))
    case IfThenElse(exp1, exp2, exp3, _, _) => DocAst.Expression.IfThenElse(print(exp1), print(exp2), print(exp3))
    case Branch(exp, branches, _, _) => DocAst.Expression.Branch(print(exp), branches.view.mapValues(print).toMap)
    case JumpTo(sym, _, _) => DocAst.Expression.JumpTo(sym)
    case Let(sym, exp1, exp2, _, _) => DocAst.Expression.Let(printVarSym(sym), None, print(exp1), print(exp2))
    case LetRec(varSym, _, _, exp1, exp2, _, _) => DocAst.Expression.LetRec(printVarSym(varSym), None, print(exp1), print(exp2))
    case Scope(sym, exp, _, _) => DocAst.Expression.Scope(printVarSym(sym), print(exp))
    case TryCatch(exp, rules, _, _) => DocAst.Expression.TryCatch(print(exp), rules.map { case ErasedAst.CatchRule(sym, clazz, rexp) =>
      (sym, clazz, print(rexp))
    })
    case NewObject(name, clazz, tpe, methods, _) =>
      DocAst.Expression.NewObject(name, clazz, MonoTypePrinter.print(tpe), methods.map {
        case JvmMethod(ident, fparams, clo, retTpe, _) =>
          DocAst.JvmMethod(ident, fparams.map(printFormalParam), print(clo), MonoTypePrinter.print(retTpe))
      })
    case Intrinsic1(op, exp, tpe, _) => IntrinsicOperatorPrinter.print(op, print(exp), MonoTypePrinter.print(tpe))
    case App(op, exps, _, _) => IntrinsicOperatorPrinter.print(op, exps.map(print))
    case Intrinsic1N(op, exp, exps, _, _) => IntrinsicOperatorPrinter.print(op, print(exp), exps.map(print))
  }

  /**
    * Returns the [[DocAst.Expression.Ascription]] representation of `fp`.
    */
  private def printFormalParam(fp: ErasedAst.FormalParam): DocAst.Expression.Ascription = {
    val ErasedAst.FormalParam(sym, tpe) = fp
    DocAst.Expression.Ascription(printVarSym(sym), MonoTypePrinter.print(tpe))
  }

  /**
    * Returns the [[DocAst.Expression]] representation of `sym`.
    */
  private def printVarSym(sym: Symbol.VarSym): DocAst.Expression =
    DocAst.Expression.VarWithOffset(sym)

}

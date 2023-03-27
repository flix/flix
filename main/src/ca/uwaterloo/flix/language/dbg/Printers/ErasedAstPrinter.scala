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

package ca.uwaterloo.flix.language.dbg.Printers

import ca.uwaterloo.flix.language.ast.ErasedAst
import ca.uwaterloo.flix.language.ast.ErasedAst.Expression._
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
  def print(e: ErasedAst.Expression): DocAst.Expression = e match {
    case Var(sym, _, _) => DocAst.Expression.VarWithOffset(sym)
    case Unary(sop, exp, _, _) => DocAst.Expression.Unary(OperatorPrinter.print(sop), print(exp))
    case Binary(sop, _, exp1, exp2, _, _) => DocAst.Expression.Binary(print(exp1), OperatorPrinter.print(sop), print(exp2))
    case IfThenElse(exp1, exp2, exp3, _, _) => DocAst.Expression.IfThenElse(print(exp1), print(exp2), print(exp3))
    case Branch(exp, branches, _, _) => DocAst.Expression.Branch(print(exp), branches.view.mapValues(print).toMap)
    case JumpTo(sym, _, _) => DocAst.Expression.JumpTo(sym)
    case Let(sym, exp1, exp2, _, _) => DocAst.Expression.Let(DocAst.Expression.VarWithOffset(sym), None, print(exp1), print(exp2))
    case LetRec(varSym, _, _, exp1, exp2, _, _) => DocAst.Expression.LetRec(DocAst.Expression.VarWithOffset(varSym), None, print(exp1), print(exp2))
    case Scope(sym, exp, _, _) => DocAst.Expression.Scope(DocAst.Expression.VarWithOffset(sym), print(exp))
    case ScopeExit(exp1, exp2, _, _) => DocAst.Expression.ScopeExit(print(exp1), print(exp2))
    case TryCatch(exp, rules, _, _) => DocAst.Expression.TryCatch(print(exp), rules.map(r => (r.sym, r.clazz, print(r.exp))))
    case NewObject(name, clazz, tpe, methods, _) =>
      DocAst.Expression.NewObject(name, clazz, MonoTypePrinter.print(tpe), methods.map {
        case JvmMethod(ident, fparams, clo, retTpe, _) =>
          DocAst.Expression.JvmMethod(ident, fparams.map(printFormalParam), print(clo), MonoTypePrinter.print(retTpe))
      })
    case Intrinsic0(op, _, _) => IntrinsicOperatorPrinter.print(op)
    case Intrinsic1(op, exp, tpe, _) => IntrinsicOperatorPrinter.print(op, print(exp), MonoTypePrinter.print(tpe))
    case Intrinsic2(op, exp1, exp2, _, _) => IntrinsicOperatorPrinter.print(op, print(exp1), print(exp2))
    case Intrinsic3(op, exp1, exp2, exp3, _, _) => IntrinsicOperatorPrinter.print(op, print(exp1), print(exp2), print(exp3))
    case IntrinsicN(op, exps, _, _) => IntrinsicOperatorPrinter.print(op, exps.map(print))
    case Intrinsic1N(op, exp, exps, _, _) => IntrinsicOperatorPrinter.print(op, print(exp), exps.map(print))
  }

  /**
    * Returns the [[DocAst.Expression.Ascription]] representation of `fp`.
    */
  private def printFormalParam(fp: ErasedAst.FormalParam): DocAst.Expression.Ascription = {
    val ErasedAst.FormalParam(sym, tpe) = fp
    DocAst.Expression.Ascription(DocAst.Expression.VarWithOffset(sym), MonoTypePrinter.print(tpe))
  }

}

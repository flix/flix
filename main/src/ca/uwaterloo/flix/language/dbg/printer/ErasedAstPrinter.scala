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

import ca.uwaterloo.flix.language.ast.Ast.CallType
import ca.uwaterloo.flix.language.ast.{ErasedAst, Symbol}
import ca.uwaterloo.flix.language.ast.ErasedAst.Expr._
import ca.uwaterloo.flix.language.ast.ErasedAst._
import ca.uwaterloo.flix.language.dbg.DocAst
import ca.uwaterloo.flix.util.collection.MapOps

object ErasedAstPrinter {

  /**
    * Returns the [[DocAst.Program]] representation of `root`.
    */
  def print(root: ErasedAst.Root): DocAst.Program = {
    val enums = root.enums.values.map {
      case ErasedAst.Enum(ann, mod, sym, cases0, _, _) =>
        val cases = cases0.values.map {
          case ErasedAst.Case(sym, tpe, _) =>
            DocAst.Case(sym, MonoTypePrinter.print(tpe))
        }.toList
        DocAst.Enum(ann, mod, sym, Nil, cases)
    }.toList
    val defs = root.defs.values.map {
      case ErasedAst.Def(ann, mod, _, sym, cparams, fparams, exp, tpe, _) =>
        DocAst.Def(
          ann,
          mod,
          sym,
          (cparams ++ fparams).map(printFormalParam),
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
    case ApplyClo(exp, exps, ct, _, _) => ct match {
      case CallType.TailCall => DocAst.Expression.ApplyCloTail(print(exp), exps.map(print))
      case CallType.NonTailCall => DocAst.Expression.ApplyClo(print(exp), exps.map(print))
    }
    case ApplyDef(sym, exps, ct, _, _) => ct match {
      case CallType.TailCall => DocAst.Expression.ApplyDefTail(sym, exps.map(print))
      case CallType.NonTailCall => DocAst.Expression.ApplyDef(sym, exps.map(print))
    }
    case ApplySelfTail(sym, _, exps, _, _) => DocAst.Expression.ApplySelfTail(sym, exps.map(print))
    case ApplyAtomic(op, exps, tpe, _) => OperatorPrinter.print(op, exps.map(print), MonoTypePrinter.print(tpe))
    case IfThenElse(exp1, exp2, exp3, _, _) => DocAst.Expression.IfThenElse(print(exp1), print(exp2), print(exp3))
    case Branch(exp, branches, _, _) => DocAst.Expression.Branch(print(exp), MapOps.mapValues(branches)(print))
    case JumpTo(sym, _, _) => DocAst.Expression.JumpTo(sym)
    case Let(sym, exp1, exp2, _, _) => DocAst.Expression.Let(printVarSym(sym), Some(MonoTypePrinter.print(exp1.tpe)), print(exp1), print(exp2))
    case LetRec(varSym, _, _, exp1, exp2, _, _) => DocAst.Expression.LetRec(printVarSym(varSym), Some(MonoTypePrinter.print(exp1.tpe)), print(exp1), print(exp2))
    case Scope(sym, exp, _, _) => DocAst.Expression.Scope(printVarSym(sym), print(exp))
    case TryCatch(exp, rules, _, _) => DocAst.Expression.TryCatch(print(exp), rules.map { case ErasedAst.CatchRule(sym, clazz, rexp) =>
      (sym, clazz, print(rexp))
    })
    case NewObject(name, clazz, tpe, methods, _) =>
      DocAst.Expression.NewObject(name, clazz, MonoTypePrinter.print(tpe), methods.map {
        case JvmMethod(ident, fparams, clo, retTpe, _) =>
          DocAst.JvmMethod(ident, fparams.map(printFormalParam), print(clo), MonoTypePrinter.print(retTpe))
      })
  }

  /**
    * Returns the [[DocAst.Expression]] representation of `s`.
    */
  def print(s: ErasedAst.Stmt): DocAst.Expression = s match {
    case ErasedAst.Stmt.Ret(e, _, _) => print(e)
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

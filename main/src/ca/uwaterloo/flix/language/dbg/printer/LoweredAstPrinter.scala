/*
 * Copyright 2023 Matthew Lutze
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

import ca.uwaterloo.flix.language.ast.LoweredAst
import ca.uwaterloo.flix.language.ast.LoweredAst.{Expr, Pattern}
import ca.uwaterloo.flix.language.dbg.DocAst

object LoweredAstPrinter {

  /**
    * Returns the [[DocAst.Program]] representation of `root`.
    */
  def print(root: LoweredAst.Root): DocAst.Program = {
    val enums = root.enums.values.map {
      case LoweredAst.Enum(_, ann, mod, sym, tparams, _, cases0, _) =>
        val cases = cases0.values.map {
          case LoweredAst.Case(caseSym, tpes, _, _) =>
            DocAst.Case(caseSym, tpes.map(TypePrinter.print))
        }.toList
        DocAst.Enum(ann, mod, sym, tparams.map(printTypeParam), cases)
    }.toList
    val defs = root.defs.values.map {
      case LoweredAst.Def(sym, LoweredAst.Spec(_, ann, mod, _, fparams, _, retTpe, eff, _), exp, _) =>
        DocAst.Def(
          ann,
          mod,
          sym,
          fparams.map(printFormalParam),
          TypePrinter.print(retTpe),
          TypePrinter.print(eff),
          print(exp)
        )
    }.toList
    DocAst.Program(enums, defs, Nil)
  }

  /**
    * Returns the [[DocAst.Expr]] representation of `e`.
    */
  def print(e: LoweredAst.Expr): DocAst.Expr = e match {
    case Expr.Cst(cst, _, _) => ConstantPrinter.print(cst)
    case Expr.Var(sym, _, _) => DocAst.Expr.Var(sym)
    case Expr.Lambda(fparam, exp, _, _) => DocAst.Expr.Lambda(List(printFormalParam(fparam)), print(exp))
    case Expr.ApplyClo(exp1, exp2, _, _, _) => DocAst.Expr.ApplyClo(print(exp1), List(print(exp2)))
    case Expr.ApplyDef(sym, exps, _, _, _, _) => DocAst.Expr.ApplyDef(sym, exps.map(print))
    case Expr.ApplyLocalDef(sym, exps, _, _, _) => DocAst.Expr.ApplyClo(DocAst.Expr.Var(sym), exps.map(print))
    case Expr.ApplySig(sym, exps, _, _, _, _) => DocAst.Expr.ApplyClo(DocAst.Expr.Sig(sym), exps.map(print))
    case Expr.ApplyAtomic(op, exps, tpe, eff, _) => OpPrinter.print(op, exps.map(print), TypePrinter.print(tpe), TypePrinter.print(eff))
    case Expr.Let(sym, exp1, exp2, _, _, _) => DocAst.Expr.Let(DocAst.Expr.Var(sym), None, print(exp1), print(exp2))
    case Expr.LocalDef(sym, fparams, exp1, exp2, tpe, eff, _) => DocAst.Expr.LocalDef(DocAst.Expr.Var(sym), fparams.map(printFormalParam), Some(TypePrinter.print(tpe)), Some(TypePrinter.print(eff)), print(exp1), print(exp2))
    case Expr.Scope(sym, _, exp, _, _, _) => DocAst.Expr.Scope(DocAst.Expr.Var(sym), print(exp))
    case Expr.IfThenElse(exp1, exp2, exp3, _, _, _) => DocAst.Expr.IfThenElse(print(exp1), print(exp2), print(exp3))
    case Expr.Stm(exp1, exp2, _, _, _) => DocAst.Expr.Stm(print(exp1), print(exp2))
    case Expr.Discard(exp, _, _) => DocAst.Expr.Discard(print(exp))
    case Expr.Match(exp, rules, _, _, _) =>
      val expD = print(exp)
      val rulesD = rules.map {
        case LoweredAst.MatchRule(pat, guard, body) =>
          val patD = printPattern(pat)
          val guardD = guard.map(print)
          val bodyD = print(body)
          (patD, guardD, bodyD)
      }
      DocAst.Expr.Match(expD, rulesD)
    case Expr.ExtensibleMatch(_, _, _, _, _, _, _, _, _) => DocAst.Expr.Unknown
    case Expr.TypeMatch(exp, rules, _, _, _) =>
      val expD = print(exp)
      val rulesD = rules.map {
        case LoweredAst.TypeMatchRule(sym, tpe, body) =>
          val patD = DocAst.Expr.Var(sym)
          val tpeD = TypePrinter.print(tpe)
          val bodyD = print(body)
          (patD, tpeD, bodyD)
      }
      DocAst.Expr.TypeMatch(expD, rulesD)
    case Expr.VectorLit(exps, _, _, _) => DocAst.Expr.VectorLit(exps.map(print))
    case Expr.VectorLoad(exp1, exp2, _, _, _) => DocAst.Expr.VectorLoad(print(exp1), print(exp2))
    case Expr.VectorLength(exp, _) => DocAst.Expr.ArrayLength(print(exp))
    case Expr.Ascribe(exp, tpe, _, _) => DocAst.Expr.AscriptionTpe(print(exp), TypePrinter.print(tpe))
    case Expr.Cast(exp, declaredType, _, _, _, _) => declaredType match {
      case None => print(exp) // TODO needs eff
      case Some(t) => DocAst.Expr.Cast(print(exp), TypePrinter.print(t))
    }
    case Expr.TryCatch(exp, rules, _, _, _) =>
      val expD = print(exp)
      val rulesD = rules.map {
        case LoweredAst.CatchRule(sym, clazz, body) => (sym, clazz, print(body))
      }
      DocAst.Expr.TryCatch(expD, rulesD)
    case Expr.RunWith(exp, effUse, rules, _, _, _) =>
      val expD = print(exp)
      val effD = effUse.sym
      val rulesD = rules.map {
        case LoweredAst.HandlerRule(op, fparams, body) => (op.sym, fparams.map(printFormalParam), print(body))
      }
      DocAst.Expr.RunWithHandler(expD, effD, rulesD)
    case Expr.Do(op, exps, _, _, _) => DocAst.Expr.Do(op.sym, exps.map(print))
    case Expr.NewObject(name, clazz, tpe, _, methods, _) =>
      val methodsD = methods.map {
        case LoweredAst.JvmMethod(ident, fparams, exp, retTpe, _, _) => DocAst.JvmMethod(ident, fparams.map(printFormalParam), print(exp), TypePrinter.print(retTpe))
      }
      DocAst.Expr.NewObject(name, clazz, TypePrinter.print(tpe), methodsD)
  }

  /**
    * Converts the given pattern into a [[DocAst.Expr]].
    */
  private def printPattern(pat: LoweredAst.Pattern): DocAst.Expr = pat match {
    case Pattern.Wild(_, _) => DocAst.Expr.Wild
    case Pattern.Var(sym, _, _) => DocAst.Expr.Var(sym)
    case Pattern.Cst(cst, _, _) => DocAst.Expr.Cst(cst)
    case Pattern.Tag(sym, pats, _, _) => DocAst.Expr.Tag(sym.sym, pats.map(printPattern))
    case Pattern.Tuple(elms, _, _) => DocAst.Expr.Tuple(elms.map(printPattern).toList)
    case Pattern.Record(pats, rest, _, _) => printRecordPattern(pats, rest)
  }

  /**
    * Converts the record pattern into a [[DocAst.Expr]] by adding a series of [[DocAst.Expr.RecordExtend]] expressions.
    */
  private def printRecordPattern(pats: List[LoweredAst.Pattern.Record.RecordLabelPattern], pat: LoweredAst.Pattern): DocAst.Expr = {
    pats.foldRight(printPattern(pat)) {
      case (LoweredAst.Pattern.Record.RecordLabelPattern(label, p, _, _), acc) =>
        DocAst.Expr.RecordExtend(label, printPattern(p), acc)
    }
  }

  /**
    * Returns the [[DocAst.Expr.AscriptionTpe]] representation of `fp`.
    */
  private def printFormalParam(fp: LoweredAst.FormalParam): DocAst.Expr.AscriptionTpe = {
    val LoweredAst.FormalParam(sym, _, tpe, _, _) = fp
    DocAst.Expr.AscriptionTpe(DocAst.Expr.Var(sym), TypePrinter.print(tpe))
  }

  /**
    * Returns the [[DocAst.TypeParam]] representation of `tp`.
    */
  private def printTypeParam(tp: LoweredAst.TypeParam): DocAst.TypeParam = tp match {
    case LoweredAst.TypeParam(_, sym, _) =>
      DocAst.TypeParam(sym)
  }
}

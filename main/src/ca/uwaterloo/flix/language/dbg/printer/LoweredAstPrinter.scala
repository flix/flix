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
      case LoweredAst.Enum(_, ann, mod, sym, tparams, _, cases0, _, _) =>
        val cases = cases0.values.map {
          case LoweredAst.Case(sym, tpe, _, _) =>
            DocAst.Case(sym, TypePrinter.print(tpe))
        }.toList
        DocAst.Enum(ann, mod, sym, tparams.map(printTypeParam), cases)
    }.toList
    val defs = root.defs.values.map {
      case LoweredAst.Def(sym, LoweredAst.Spec(_, ann, mod, _, fparams, _, retTpe, eff, _, _), exp) =>
        DocAst.Def(
          ann,
          mod,
          sym,
          fparams.map(printFormalParam),
          TypePrinter.print(retTpe),
          TypePrinter.printAsEffect(eff),
          print(exp)
        )
    }.toList
    DocAst.Program(enums, defs)
  }

  /**
    * Returns the [[DocAst.Expr]] representation of `e`.
    */
  def print(e: LoweredAst.Expr): DocAst.Expr = e match {
    case Expr.Cst(cst, tpe, loc) => ConstantPrinter.print(cst)
    case Expr.Var(sym, tpe, loc) => DocAst.Expr.Var(sym)
    case Expr.Def(sym, tpe, loc) => DocAst.Expr.Def(sym)
    case Expr.Sig(sym, tpe, loc) => DocAst.Expr.Sig(sym)
    case Expr.Lambda(fparam, exp, tpe, loc) => DocAst.Expr.Lambda(List(printFormalParam(fparam)), print(exp))
    case Expr.Apply(exp, exps, tpe, eff, loc) => DocAst.Expr.ApplyClo(print(exp), exps.map(print), None)
    case Expr.ApplyAtomic(op, exps, tpe, _, loc) => OpPrinter.print(op, exps.map(print), TypePrinter.print(tpe))
    case Expr.Let(sym, mod, exp1, exp2, tpe, eff, loc) => DocAst.Expr.Let(DocAst.Expr.Var(sym), None, print(exp1), print(exp2))
    case Expr.LetRec(sym, mod, exp1, exp2, tpe, eff, loc) => DocAst.Expr.LetRec(DocAst.Expr.Var(sym), None, print(exp1), print(exp2))
    case Expr.Scope(sym, regionVar, exp, tpe, eff, loc) => DocAst.Expr.Scope(DocAst.Expr.Var(sym), print(exp))
    case Expr.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) => DocAst.Expr.IfThenElse(print(exp1), print(exp2), print(exp3))
    case Expr.Stm(exp1, exp2, tpe, eff, loc) => DocAst.Expr.Stm(print(exp1), print(exp2))
    case Expr.Discard(exp, eff, loc) => DocAst.Expr.Discard(print(exp))
    case Expr.Match(exp, rules, tpe, eff, loc) =>
      val expD = print(exp)
      val rulesD = rules.map {
        case LoweredAst.MatchRule(pat, guard, body) =>
          val patD = printPattern(pat)
          val guardD = guard.map(print)
          val bodyD = print(body)
          (patD, guardD, bodyD)
      }
      DocAst.Expr.Match(expD, rulesD)
    case Expr.TypeMatch(exp, rules, tpe, eff, loc) =>
      val expD = print(exp)
      val rulesD = rules.map {
        case LoweredAst.TypeMatchRule(sym, tpe, body) =>
          val patD = DocAst.Expr.Var(sym)
          val tpeD = TypePrinter.print(tpe)
          val bodyD = print(body)
          (patD, tpeD, bodyD)
      }
      DocAst.Expr.TypeMatch(expD, rulesD)
    case Expr.VectorLit(exps, tpe, eff, loc) => DocAst.Expr.VectorLit(exps.map(print))
    case Expr.VectorLoad(exp1, exp2, tpe, eff, loc) => DocAst.Expr.VectorLoad(print(exp1), print(exp2))
    case Expr.VectorLength(exp, loc) => DocAst.Expr.ArrayLength(print(exp))
    case Expr.Ascribe(exp, tpe, eff, loc) => DocAst.Expr.Ascription(print(exp), TypePrinter.print(tpe))
    case Expr.Cast(exp, declaredType, declaredEff, tpe, eff, loc) => declaredType match {
      case None => print(exp) // TODO needs eff
      case Some(t) => DocAst.Expr.Cast(print(exp), TypePrinter.print(t))
    }
    case Expr.TryCatch(exp, rules, tpe, eff, loc) =>
      val expD = print(exp)
      val rulesD = rules.map {
        case LoweredAst.CatchRule(sym, clazz, body) => (sym, clazz, print(body))
      }
      DocAst.Expr.TryCatch(expD, rulesD)
    case Expr.TryWith(exp, effUse, rules, tpe, eff, loc) =>
      val expD = print(exp)
      val effD = effUse.sym
      val rulesD = rules.map {
        case LoweredAst.HandlerRule(op, fparams, exp) => (op.sym, fparams.map(printFormalParam), print(exp))
      }
      DocAst.Expr.TryWith(expD, effD, rulesD)
    case Expr.Do(op, exps, tpe, eff, loc) => DocAst.Expr.Do(op.sym, exps.map(print))
    case Expr.NewObject(name, clazz, tpe, eff, methods, loc) =>
      val methodsD = methods.map {
        case LoweredAst.JvmMethod(ident, fparams, exp, retTpe, eff, loc) => DocAst.JvmMethod(ident, fparams.map(printFormalParam), print(exp), TypePrinter.print(retTpe))
      }
      DocAst.Expr.NewObject(name, clazz, TypePrinter.print(tpe), methodsD)
  }

  /**
    * Converts the given pattern into a [[DocAst.Expr]].
    */
  private def printPattern(pat: LoweredAst.Pattern): DocAst.Expr = pat match {
    case Pattern.Wild(tpe, loc) => DocAst.Expr.Wild
    case Pattern.Var(sym, tpe, loc) => DocAst.Expr.Var(sym)
    case Pattern.Cst(cst, tpe, loc) => DocAst.Expr.Cst(cst)
    case Pattern.Tag(sym, pat, tpe, loc) => DocAst.Expr.Tag(sym.sym, List(printPattern(pat)))
    case Pattern.Tuple(elms, tpe, loc) => DocAst.Expr.Tuple(elms.map(printPattern))
    case Pattern.Record(pats, pat, tpe, loc) => printRecordPattern(pats, pat)
    case Pattern.RecordEmpty(_, _) => DocAst.Expr.RecordEmpty
  }

  /**
    * Converts the record pattern into a [[DocAst.Expr]] by adding a series of [[DocAst.Expr.RecordExtend]] expressions.
    */
  private def printRecordPattern(pats: List[LoweredAst.Pattern.Record.RecordLabelPattern], pat: LoweredAst.Pattern): DocAst.Expr = {
    pats.foldRight(printPattern(pat)) {
      case (LoweredAst.Pattern.Record.RecordLabelPattern(label, _, p, _), acc) =>
        DocAst.Expr.RecordExtend(label, printPattern(p), acc)
    }
  }

  /**
    * Returns the [[DocAst.Expr.Ascription]] representation of `fp`.
    */
  private def printFormalParam(fp: LoweredAst.FormalParam): DocAst.Expr.Ascription = {
    val LoweredAst.FormalParam(sym, _, tpe, _, _) = fp
    DocAst.Expr.Ascription(DocAst.Expr.Var(sym), TypePrinter.print(tpe))
  }

  /**
    * Returns the [[DocAst.TypeParam]] representation of `tp`.
    */
  private def printTypeParam(tp: LoweredAst.TypeParam): DocAst.TypeParam = tp match {
    case LoweredAst.TypeParam(_, sym, _) =>
      DocAst.TypeParam(sym)
  }
}

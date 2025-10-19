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

import ca.uwaterloo.flix.language.ast.{LoweredAst, Symbol}
import ca.uwaterloo.flix.language.ast.LoweredAst.{Exp, ExtPattern, ExtTagPattern, Pattern}
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
    * Returns the [[DocAst.Exp]] representation of `e`.
    */
  def print(e: LoweredAst.Exp): DocAst.Exp = e match {
    case Exp.Cst(cst, _, _) => ConstantPrinter.print(cst)
    case Exp.Var(sym, _, _) => DocAst.Exp.Var(sym)
    case Exp.Lambda(fparam, exp, _, _) => DocAst.Exp.Lambda(List(printFormalParam(fparam)), print(exp))
    case Exp.ApplyClo(exp1, exp2, _, _, _) => DocAst.Exp.ApplyClo(print(exp1), List(print(exp2)))
    case Exp.ApplyDef(sym, exps, _, _, _, _, _) => DocAst.Exp.ApplyDef(sym, exps.map(print))
    case Exp.ApplyLocalDef(sym, exps, _, _, _) => DocAst.Exp.ApplyClo(DocAst.Exp.Var(sym), exps.map(print))
    case Exp.ApplyOp(op, exps, _, _, _) => DocAst.Exp.ApplyOp(op, exps.map(print))
    case Exp.ApplySig(sym, exps, _, _, _, _, _, _) => DocAst.Exp.ApplyClo(DocAst.Exp.Sig(sym), exps.map(print))
    case Exp.ApplyAtomic(op, exps, tpe, eff, _) => OpPrinter.print(op, exps.map(print), TypePrinter.print(tpe), TypePrinter.print(eff))
    case Exp.Let(sym, exp1, exp2, _, _, _) => DocAst.Exp.Let(DocAst.Exp.Var(sym), None, print(exp1), print(exp2))
    case Exp.LocalDef(sym, fparams, exp1, exp2, tpe, eff, _) => DocAst.Exp.LocalDef(DocAst.Exp.Var(sym), fparams.map(printFormalParam), Some(TypePrinter.print(tpe)), Some(TypePrinter.print(eff)), print(exp1), print(exp2))
    case Exp.Region(sym, _, exp, _, _, _) => DocAst.Exp.Region(DocAst.Exp.Var(sym), print(exp))
    case Exp.IfThenElse(exp1, exp2, exp3, _, _, _) => DocAst.Exp.IfThenElse(print(exp1), print(exp2), print(exp3))
    case Exp.Stm(exp1, exp2, _, _, _) => DocAst.Exp.Stm(print(exp1), print(exp2))
    case Exp.Discard(exp, _, _) => DocAst.Exp.Discard(print(exp))
    case Exp.Match(exp, rules, _, _, _) =>
      val expD = print(exp)
      val rulesD = rules.map {
        case LoweredAst.MatchRule(pat, guard, body) =>
          val patD = printPattern(pat)
          val guardD = guard.map(print)
          val bodyD = print(body)
          (patD, guardD, bodyD)
      }
      DocAst.Exp.Match(expD, rulesD)
    case Exp.ExtMatch(exp, rules, _, _, _) => DocAst.Exp.ExtMatch(print(exp), rules.map(printExtMatchRule))
    case Exp.TypeMatch(exp, rules, _, _, _) =>
      val expD = print(exp)
      val rulesD = rules.map {
        case LoweredAst.TypeMatchRule(sym, tpe, body) =>
          val patD = DocAst.Exp.Var(sym)
          val tpeD = TypePrinter.print(tpe)
          val bodyD = print(body)
          (patD, tpeD, bodyD)
      }
      DocAst.Exp.TypeMatch(expD, rulesD)
    case Exp.VectorLit(exps, _, _, _) => DocAst.Exp.VectorLit(exps.map(print))
    case Exp.VectorLoad(exp1, exp2, _, _, _) => DocAst.Exp.VectorLoad(print(exp1), print(exp2))
    case Exp.VectorLength(exp, _) => DocAst.Exp.ArrayLength(print(exp))
    case Exp.Ascribe(exp, tpe, _, _) => DocAst.Exp.AscriptionTpe(print(exp), TypePrinter.print(tpe))
    case Exp.Cast(exp, declaredType, _, _, _, _) => declaredType match {
      case None => print(exp) // TODO needs eff
      case Some(t) => DocAst.Exp.Cast(print(exp), TypePrinter.print(t))
    }
    case Exp.TryCatch(exp, rules, _, _, _) =>
      val expD = print(exp)
      val rulesD = rules.map {
        case LoweredAst.CatchRule(sym, clazz, body) => (sym, clazz, print(body))
      }
      DocAst.Exp.TryCatch(expD, rulesD)
    case Exp.RunWith(exp, effSymUse, rules, _, _, _) =>
      val expD = print(exp)
      val effD = effSymUse.sym
      val rulesD = rules.map {
        case LoweredAst.HandlerRule(opSymUse, fparams, body) => (opSymUse.sym, fparams.map(printFormalParam), print(body))
      }
      DocAst.Exp.RunWithHandler(expD, effD, rulesD)
    case Exp.NewObject(name, clazz, tpe, _, methods, _) =>
      val methodsD = methods.map {
        case LoweredAst.JvmMethod(ident, fparams, exp, retTpe, _, _) => DocAst.JvmMethod(ident, fparams.map(printFormalParam), print(exp), TypePrinter.print(retTpe))
      }
      DocAst.Exp.NewObject(name, clazz, TypePrinter.print(tpe), methodsD)
  }

  /**
    * Returns the [[DocAst]] representation of `rule`.
    */
  private def printExtMatchRule(rule: LoweredAst.ExtMatchRule): (DocAst.Exp, DocAst.Exp) = rule match {
    case LoweredAst.ExtMatchRule(pat, exp, _) =>
      (printExtPattern(pat), print(exp))
  }

  /**
    * Converts the given pattern into a [[DocAst.Exp]].
    */
  private def printPattern(pat: LoweredAst.Pattern): DocAst.Exp = pat match {
    case Pattern.Wild(_, _) => DocAst.Exp.Wild
    case Pattern.Var(sym, _, _) => DocAst.Exp.Var(sym)
    case Pattern.Cst(cst, _, _) => DocAst.Exp.Cst(cst)
    case Pattern.Tag(symUse, pats, _, _) => DocAst.Exp.Tag(symUse.sym, pats.map(printPattern))
    case Pattern.Tuple(elms, _, _) => DocAst.Exp.Tuple(elms.map(printPattern).toList)
    case Pattern.Record(pats, rest, _, _) => printRecordPattern(pats, rest)
  }

  /**
    * Converts the record pattern into a [[DocAst.Exp]] by adding a series of [[DocAst.Exp.RecordExtend]] expressions.
    */
  private def printRecordPattern(pats: List[LoweredAst.Pattern.Record.RecordLabelPattern], pat: LoweredAst.Pattern): DocAst.Exp = {
    pats.foldRight(printPattern(pat)) {
      case (LoweredAst.Pattern.Record.RecordLabelPattern(label, p, _, _), acc) =>
        DocAst.Exp.RecordExtend(label, printPattern(p), acc)
    }
  }

  /**
    * Returns the [[DocAst.Exp]] representation of `pattern`.
    */
  private def printExtPattern(pattern: LoweredAst.ExtPattern): DocAst.Exp = pattern match {
    case ExtPattern.Default(_) => DocAst.Pattern.Default
    case ExtPattern.Tag(label, pats, _) => DocAst.Pattern.ExtTag(label, pats.map(printExtTagPattern))
  }

  /**
    * Returns the [[DocAst.Exp]] representation of `pattern`.
    */
  private def printExtTagPattern(pattern: LoweredAst.ExtTagPattern): DocAst.Exp = pattern match {
    case ExtTagPattern.Wild(_, _) => DocAst.Exp.Wild
    case ExtTagPattern.Var(sym, _, _) => DocAst.Exp.Var(sym)
    case ExtTagPattern.Unit(_, _) => DocAst.Exp.Unit
  }

  /**
    * Returns the [[DocAst.Exp.AscriptionTpe]] representation of `fp`.
    */
  private def printFormalParam(fp: LoweredAst.FormalParam): DocAst.Exp.AscriptionTpe = {
    val LoweredAst.FormalParam(sym, tpe, _) = fp
    DocAst.Exp.AscriptionTpe(DocAst.Exp.Var(sym), TypePrinter.print(tpe))
  }

  /**
    * Returns the [[DocAst.TypeParam]] representation of `tp`.
    */
  private def printTypeParam(tp: LoweredAst.TypeParam): DocAst.TypeParam = tp match {
    case LoweredAst.TypeParam(_, sym, _) =>
      DocAst.TypeParam(sym)
  }
}

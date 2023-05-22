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
import ca.uwaterloo.flix.language.ast.LoweredAst.{Expression, Pattern, RelationalChoicePattern}
import ca.uwaterloo.flix.language.dbg.DocAst

class LoweredAstPrinter {

  /**
    * Returns the [[DocAst.Program]] representation of `root`.
    */
  def print(root: LoweredAst.Root): DocAst.Program = {
    val enums = root.enums.values.map {
      case LoweredAst.Enum(_, ann, mod, sym, _, _, cases0, _, _) =>
        val cases = cases0.values.map {
          case LoweredAst.Case(sym, _, _, _) => DocAst.Case(sym)
        }.toList
        DocAst.Enum(ann, mod, sym, cases)
    }.toList
    val defs = root.defs.values.map {
      case LoweredAst.Def(sym, LoweredAst.Spec(doc, ann, mod, tparams, fparams, declaredScheme, retTpe, pur, tconstrs, loc), LoweredAst.Impl(exp, inferredScheme)) =>
        DocAst.Def(
          ann,
          mod,
          sym,
          fparams.map(printFormalParam),
          TypePrinter.print(retTpe),
          print(exp)
        )
    }.toList
    DocAst.Program(enums, defs)
  }

  /**
    * Returns the [[DocAst.Expression]] representation of `e`.
    */
  def print(e: LoweredAst.Expression): DocAst.Expression = e match {
    case Expression.Cst(cst, tpe, loc) => ConstantPrinter.print(cst)
    case Expression.Wild(tpe, loc) => ??? // TODO will be removed
    case Expression.Var(sym, tpe, loc) => DocAst.Expression.Var(sym)
    case Expression.Def(sym, tpe, loc) => DocAst.Expression.Def(sym)
    case Expression.Sig(sym, tpe, loc) => DocAst.Expression.Sig(sym)
    case Expression.Hole(sym, tpe, loc) => DocAst.Expression.Hole(sym)
    case Expression.Lambda(fparam, exp, tpe, loc) => DocAst.Expression.Lambda(List(printFormalParam(fparam)), print(exp))
    case Expression.Apply(exp, exps, tpe, pur, loc) => DocAst.Expression.ApplyClo(print(exp), exps.map(print))
    case Expression.Unary(sop, exp, tpe, pur, loc) => DocAst.Expression.Unary(OperatorPrinter.print(sop), print(exp))
    case Expression.Binary(sop, exp1, exp2, tpe, pur, loc) => DocAst.Expression.Binary(print(exp1), OperatorPrinter.print(sop), print(exp2))
    case Expression.Let(sym, mod, exp1, exp2, tpe, pur, loc) => DocAst.Expression.Let(DocAst.Expression.Var(sym), None, print(exp1), print(exp2))
    case Expression.LetRec(sym, mod, exp1, exp2, tpe, pur, loc) => DocAst.Expression.LetRec(DocAst.Expression.Var(sym), None, print(exp1), print(exp2))
    case Expression.Region(tpe, loc) => DocAst.Expression.Region
    case Expression.Scope(sym, regionVar, exp, tpe, pur, loc) => DocAst.Expression.Scope(DocAst.Expression.Var(sym), print(exp))
    case Expression.ScopeExit(exp1, exp2, tpe, pur, loc) => DocAst.Expression.ScopeExit(print(exp1), print(exp2))
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, pur, loc) => DocAst.Expression.IfThenElse(print(exp1), print(exp2), print(exp3))
    case Expression.Stm(exp1, exp2, tpe, pur, loc) => DocAst.Expression.Stm(print(exp1), print(exp2))
    case Expression.Discard(exp, pur, loc) => DocAst.Expression.Discard(print(exp))
    case Expression.Match(exp, rules, tpe, pur, loc) =>
      val expD = print(exp)
      val rulesD = rules.map {
        case LoweredAst.MatchRule(pat, guard, body) =>
          val patD = printPattern(pat)
          val guardD = guard.map(print)
          val bodyD = print(body)
          (patD, guardD, bodyD)
      }
      DocAst.Expression.Match(expD, rulesD)
    case Expression.TypeMatch(exp, rules, tpe, pur, loc) =>
      val expD = print(exp)
      val rulesD = rules.map {
        case LoweredAst.MatchTypeRule(sym, tpe, body) =>
          val patD = DocAst.Expression.Var(sym)
          val tpeD = TypePrinter.print(tpe)
          val bodyD = print(body)
          (patD, tpeD, bodyD)
      }
      DocAst.Expression.TypeMatch(expD, rulesD)
    case Expression.RelationalChoose(exps, rules, tpe, pur, loc) =>
      val expD = DocAst.Expression.Tuple(exps.map(print))
      val rulesD = rules.map {
        case LoweredAst.RelationalChoiceRule(pat, body) =>
          val patD = DocAst.Expression.Tuple(pat.map(printRelationalChoicePattern))
          val guardD = None
          val bodyD = print(body)
          (patD, guardD, bodyD)
      }
      DocAst.Expression.Match(expD, rulesD)
    case Expression.Tag(sym, exp, tpe, pur, loc) => DocAst.Expression.Tag(sym.sym, List(print(exp)))
    case Expression.Tuple(elms, tpe, pur, loc) => DocAst.Expression.Tuple(elms.map(print))
    case Expression.RecordEmpty(tpe, loc) => DocAst.Expression.RecordEmpty
    case Expression.RecordSelect(exp, field, tpe, pur, loc) => DocAst.Expression.RecordSelect(field, print(exp))
    case Expression.RecordExtend(field, value, rest, tpe, pur, loc) => DocAst.Expression.RecordExtend(field, print(value), print(rest))
    case Expression.RecordRestrict(field, rest, tpe, pur, loc) => DocAst.Expression.RecordRestrict(field, print(rest))
    case Expression.ArrayLit(exps, exp, tpe, pur, loc) => DocAst.Expression.ArrayLit(exps.map(print)) // TODO needs region
    case Expression.ArrayNew(exp1, exp2, exp3, tpe, pur, loc) => DocAst.Expression.ArrayNew(print(exp1), print(exp2)) // TODO needs region
    case Expression.ArrayLoad(base, index, tpe, pur, loc) => DocAst.Expression.ArrayLoad(print(base), print(index))
    case Expression.ArrayLength(base, pur, loc) => DocAst.Expression.ArrayLength(print(base))
    case Expression.ArrayStore(base, index, elm, pur, loc) => DocAst.Expression.ArrayStore(print(base), print(index), print(elm))
    case Expression.VectorLit(exps, tpe, pur, loc) => ???
    case Expression.VectorLoad(exp1, exp2, tpe, pur, loc) => ???
    case Expression.VectorLength(exp, loc) => ???
    case Expression.Ref(exp1, exp2, tpe, pur, loc) => DocAst.Expression.Ref(print(exp1)) // TODO needs region
    case Expression.Deref(exp, tpe, pur, loc) => DocAst.Expression.Deref(print(exp))
    case Expression.Assign(exp1, exp2, tpe, pur, loc) => DocAst.Expression.Assign(print(exp1), print(exp2))
    case Expression.Ascribe(exp, tpe, pur, loc) => DocAst.Expression.Ascription(print(exp), TypePrinter.print(tpe))
    case Expression.InstanceOf(exp, clazz, loc) => DocAst.Expression.InstanceOf(print(exp), clazz)
    case Expression.Cast(exp, declaredType, declaredPur, tpe, pur, loc) => DocAst.Expression.Cast(print(exp), TypePrinter.print(declaredType.get)) // TODO needs eff
    case Expression.Without(exp, effUse, tpe, pur, loc) => DocAst.Expression.Without(print(exp), effUse.sym)
    case Expression.TryCatch(exp, rules, tpe, pur, loc) =>
      val expD = print(exp)
      val rulesD = rules.map {
        case LoweredAst.CatchRule(sym, clazz, body) => (sym, clazz, print(body))
      }
      DocAst.Expression.TryCatch(expD, rulesD)
    case Expression.TryWith(exp, effUse, rules, tpe, pur, loc) => ???
    case Expression.Do(op, exps, pur, loc) => DocAst.Expression.Do(op.sym, exps.map(print))
    case Expression.Resume(exp, tpe, loc) => DocAst.Expression.Resume(print(exp))
    case Expression.InvokeConstructor(constructor, args, tpe, pur, loc) => DocAst.Expression.JavaInvokeConstructor(constructor, args.map(print))
    case Expression.InvokeMethod(method, exp, args, tpe, pur, loc) => DocAst.Expression.JavaInvokeMethod(method, print(exp), args.map(print))
    case Expression.InvokeStaticMethod(method, args, tpe, pur, loc) => DocAst.Expression.JavaInvokeStaticMethod(method, args.map(print))
    case Expression.GetField(field, exp, tpe, pur, loc) => DocAst.Expression.JavaGetField(field, print(exp))
    case Expression.PutField(field, exp1, exp2, tpe, pur, loc) => DocAst.Expression.JavaPutField(field, print(exp1), print(exp2))
    case Expression.GetStaticField(field, tpe, pur, loc) => DocAst.Expression.JavaGetStaticField(field)
    case Expression.PutStaticField(field, exp, tpe, pur, loc) => DocAst.Expression.JavaPutStaticField(field, print(exp))
    case Expression.NewObject(name, clazz, tpe, pur, methods, loc) =>
      val methodsD = methods.map {
        case LoweredAst.JvmMethod(ident, fparams, exp, retTpe, pur, loc) => DocAst.JvmMethod(ident, fparams.map(printFormalParam), print(exp), TypePrinter.print(retTpe))
      }
      DocAst.Expression.NewObject(name, clazz, )
    case Expression.Spawn(exp1, exp2, tpe, pur, loc) => DocAst.Expression.Spawn(print(exp1), print(exp2))
    case Expression.Lazy(exp, tpe, loc) => DocAst.Expression.Lazy(print(exp))
    case Expression.Force(exp, tpe, pur, loc) => DocAst.Expression.Force(print(exp))
  }

  // MATT docs
  private def printPattern(pat: LoweredAst.Pattern): DocAst.Expression = pat match {
    case Pattern.Wild(tpe, loc) => DocAst.Expression.Wild
    case Pattern.Var(sym, tpe, loc) => DocAst.Expression.Var(sym)
    case Pattern.Cst(cst, tpe, loc) => DocAst.Expression.Cst(cst)
    case Pattern.Tag(sym, pat, tpe, loc) => DocAst.Expression.Tag(sym.sym, List(printPattern(pat)))
    case Pattern.Tuple(elms, tpe, loc) => DocAst.Expression.Tuple(elms.map(printPattern))
  }

  // MATT docs
  private def printRelationalChoicePattern(pat: LoweredAst.RelationalChoicePattern): DocAst.Expression = pat match {
    case RelationalChoicePattern.Wild(loc) => DocAst.Expression.Wild
    case RelationalChoicePattern.Absent(loc) => DocAst.Expression.Absent
    case RelationalChoicePattern.Present(sym, tpe, loc) => DocAst.Expression.Present(DocAst.Expression.Var(sym))
  }

  /**
    * Returns the [[DocAst.Expression.Ascription]] representation of `fp`.
    */
  private def printFormalParam(fp: LoweredAst.FormalParam): DocAst.Expression.Ascription = {
    val LoweredAst.FormalParam(sym, _, tpe, _, _) = fp
    DocAst.Expression.Ascription(DocAst.Expression.Var(sym), TypePrinter.print(tpe))
  }
}

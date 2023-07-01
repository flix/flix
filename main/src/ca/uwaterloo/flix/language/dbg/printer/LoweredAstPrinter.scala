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
import ca.uwaterloo.flix.language.ast.LoweredAst.{Expression, Pattern, RelationalChoosePattern}
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
      case LoweredAst.Def(sym, LoweredAst.Spec(doc, ann, mod, tparams, fparams, declaredScheme, retTpe, eff, tconstrs, loc), LoweredAst.Impl(exp, inferredScheme)) =>
        DocAst.Def(
          ann,
          mod,
          sym,
          fparams.map(printFormalParam),
          DocAst.Type.Arrow(List(DocAst.Type.AsIs("ignoreme")), TypePrinter.print(retTpe)), // TODO EVIL, EVIL hack due to formatter impl
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
    case Expression.Var(sym, tpe, loc) => DocAst.Expression.Var(sym)
    case Expression.Def(sym, tpe, loc) => DocAst.Expression.Def(sym)
    case Expression.Sig(sym, tpe, loc) => DocAst.Expression.Sig(sym)
    case Expression.Hole(sym, tpe, loc) => DocAst.Expression.Hole(sym)
    case Expression.Lambda(fparam, exp, tpe, loc) => DocAst.Expression.Lambda(List(printFormalParam(fparam)), print(exp))
    case Expression.Apply(exp, exps, tpe, eff, loc) => DocAst.Expression.ApplyClo(print(exp), exps.map(print))
    case Expression.ApplyAtomic(op, exps, tpe, _, loc) => DocAst.Expression.fromAtomic(op, exps.map(print), TypePrinter.print(tpe), loc)
    case Expression.Let(sym, mod, exp1, exp2, tpe, eff, loc) => DocAst.Expression.Let(DocAst.Expression.Var(sym), None, print(exp1), print(exp2))
    case Expression.LetRec(sym, mod, exp1, exp2, tpe, eff, loc) => DocAst.Expression.LetRec(DocAst.Expression.Var(sym), None, print(exp1), print(exp2))
    case Expression.Scope(sym, regionVar, exp, tpe, eff, loc) => DocAst.Expression.Scope(DocAst.Expression.Var(sym), print(exp))
    case Expression.IfThenElse(exp1, exp2, exp3, tpe, eff, loc) => DocAst.Expression.IfThenElse(print(exp1), print(exp2), print(exp3))
    case Expression.Stm(exp1, exp2, tpe, eff, loc) => DocAst.Expression.Stm(print(exp1), print(exp2))
    case Expression.Discard(exp, eff, loc) => DocAst.Expression.Discard(print(exp))
    case Expression.Match(exp, rules, tpe, eff, loc) =>
      val expD = print(exp)
      val rulesD = rules.map {
        case LoweredAst.MatchRule(pat, guard, body) =>
          val patD = printPattern(pat)
          val guardD = guard.map(print)
          val bodyD = print(body)
          (patD, guardD, bodyD)
      }
      DocAst.Expression.Match(expD, rulesD)
    case Expression.TypeMatch(exp, rules, tpe, eff, loc) =>
      val expD = print(exp)
      val rulesD = rules.map {
        case LoweredAst.TypeMatchRule(sym, tpe, body) =>
          val patD = DocAst.Expression.Var(sym)
          val tpeD = TypePrinter.print(tpe)
          val bodyD = print(body)
          (patD, tpeD, bodyD)
      }
      DocAst.Expression.TypeMatch(expD, rulesD)
    case Expression.RelationalChoose(exps, rules, tpe, eff, loc) =>
      val expD = DocAst.Expression.Tuple(exps.map(print))
      val rulesD = rules.map {
        case LoweredAst.RelationalChooseRule(pat, body) =>
          val patD = DocAst.Expression.Tuple(pat.map(printRelationalChoosePattern))
          val guardD = None
          val bodyD = print(body)
          (patD, guardD, bodyD)
      }
      DocAst.Expression.Match(expD, rulesD)
    case Expression.RecordEmpty(tpe, loc) => DocAst.Expression.RecordEmpty
    case Expression.RecordSelect(exp, field, tpe, eff, loc) => DocAst.Expression.RecordSelect(field, print(exp))
    case Expression.RecordExtend(field, value, rest, tpe, eff, loc) => DocAst.Expression.RecordExtend(field, print(value), print(rest))
    case Expression.RecordRestrict(field, rest, tpe, eff, loc) => DocAst.Expression.RecordRestrict(field, print(rest))
    case Expression.ArrayLit(exps, exp, tpe, eff, loc) => DocAst.Expression.InRegion(DocAst.Expression.ArrayLit(exps.map(print)), print(exp))
    case Expression.ArrayNew(exp1, exp2, exp3, tpe, eff, loc) => DocAst.Expression.InRegion(DocAst.Expression.ArrayNew(print(exp1), print(exp2)), print(exp3))
    case Expression.ArrayLoad(base, index, tpe, eff, loc) => DocAst.Expression.ArrayLoad(print(base), print(index))
    case Expression.ArrayLength(base, eff, loc) => DocAst.Expression.ArrayLength(print(base))
    case Expression.ArrayStore(base, index, elm, eff, loc) => DocAst.Expression.ArrayStore(print(base), print(index), print(elm))
    case Expression.VectorLit(exps, tpe, eff, loc) => DocAst.Expression.VectorLit(exps.map(print))
    case Expression.VectorLoad(exp1, exp2, tpe, eff, loc) => DocAst.Expression.VectorLoad(print(exp1), print(exp2))
    case Expression.VectorLength(exp, loc) => DocAst.Expression.ArrayLength(print(exp))
    case Expression.Ref(exp1, exp2, tpe, eff, loc) => DocAst.Expression.InRegion(DocAst.Expression.Ref(print(exp1)), print(exp2))
    case Expression.Deref(exp, tpe, eff, loc) => DocAst.Expression.Deref(print(exp))
    case Expression.Assign(exp1, exp2, tpe, eff, loc) => DocAst.Expression.Assign(print(exp1), print(exp2))
    case Expression.Ascribe(exp, tpe, eff, loc) => DocAst.Expression.Ascription(print(exp), TypePrinter.print(tpe))
    case Expression.InstanceOf(exp, clazz, loc) => DocAst.Expression.InstanceOf(print(exp), clazz)
    case Expression.Cast(exp, declaredType, declaredEff, tpe, eff, loc) => declaredType match {
      case None => print(exp) // TODO needs eff
      case Some(t) => DocAst.Expression.Cast(print(exp), TypePrinter.print(t))
    }
    case Expression.TryCatch(exp, rules, tpe, eff, loc) =>
      val expD = print(exp)
      val rulesD = rules.map {
        case LoweredAst.CatchRule(sym, clazz, body) => (sym, clazz, print(body))
      }
      DocAst.Expression.TryCatch(expD, rulesD)
    case Expression.TryWith(exp, effUse, rules, tpe, eff, loc) =>
      val expD = print(exp)
      val effD = effUse.sym
      val rulesD = rules.map {
        case LoweredAst.HandlerRule(op, fparams, exp) => (op.sym, fparams.map(printFormalParam), print(exp))
      }
      DocAst.Expression.TryWith(expD, effD, rulesD)
    case Expression.Do(op, exps, tpe, eff, loc) => DocAst.Expression.Do(op.sym, exps.map(print))
    case Expression.Resume(exp, tpe, loc) => DocAst.Expression.Resume(print(exp))
    case Expression.InvokeConstructor(constructor, args, tpe, eff, loc) => DocAst.Expression.JavaInvokeConstructor(constructor, args.map(print))
    case Expression.InvokeMethod(method, exp, args, tpe, eff, loc) => DocAst.Expression.JavaInvokeMethod(method, print(exp), args.map(print))
    case Expression.InvokeStaticMethod(method, args, tpe, eff, loc) => DocAst.Expression.JavaInvokeStaticMethod(method, args.map(print))
    case Expression.GetField(field, exp, tpe, eff, loc) => DocAst.Expression.JavaGetField(field, print(exp))
    case Expression.PutField(field, exp1, exp2, tpe, eff, loc) => DocAst.Expression.JavaPutField(field, print(exp1), print(exp2))
    case Expression.GetStaticField(field, tpe, eff, loc) => DocAst.Expression.JavaGetStaticField(field)
    case Expression.PutStaticField(field, exp, tpe, eff, loc) => DocAst.Expression.JavaPutStaticField(field, print(exp))
    case Expression.NewObject(name, clazz, tpe, eff, methods, loc) =>
      val methodsD = methods.map {
        case LoweredAst.JvmMethod(ident, fparams, exp, retTpe, eff, loc) => DocAst.JvmMethod(ident, fparams.map(printFormalParam), print(exp), TypePrinter.print(retTpe))
      }
      DocAst.Expression.NewObject(name, clazz, TypePrinter.print(tpe), methodsD)
    case Expression.Spawn(exp1, exp2, tpe, eff, loc) => DocAst.Expression.Spawn(print(exp1), print(exp2))
    case Expression.Lazy(exp, tpe, loc) => DocAst.Expression.Lazy(print(exp))
    case Expression.Force(exp, tpe, eff, loc) => DocAst.Expression.Force(print(exp))
  }

  /**
    * Converts the given pattern into a [[DocAst.Expression]].
    */
  private def printPattern(pat: LoweredAst.Pattern): DocAst.Expression = pat match {
    case Pattern.Wild(tpe, loc) => DocAst.Expression.Wild
    case Pattern.Var(sym, tpe, loc) => DocAst.Expression.Var(sym)
    case Pattern.Cst(cst, tpe, loc) => DocAst.Expression.Cst(cst)
    case Pattern.Tag(sym, pat, tpe, loc) => DocAst.Expression.Tag(sym.sym, List(printPattern(pat)))
    case Pattern.Tuple(elms, tpe, loc) => DocAst.Expression.Tuple(elms.map(printPattern))
  }

  /**
    * Converts the given pattern into a [[DocAst.Expression]].
    */
  private def printRelationalChoosePattern(pat: LoweredAst.RelationalChoosePattern): DocAst.Expression = pat match {
    case RelationalChoosePattern.Wild(loc) => DocAst.Expression.Wild
    case RelationalChoosePattern.Absent(loc) => DocAst.Expression.Absent
    case RelationalChoosePattern.Present(sym, tpe, loc) => DocAst.Expression.Present(DocAst.Expression.Var(sym))
  }

  /**
    * Returns the [[DocAst.Expression.Ascription]] representation of `fp`.
    */
  private def printFormalParam(fp: LoweredAst.FormalParam): DocAst.Expression.Ascription = {
    val LoweredAst.FormalParam(sym, _, tpe, _, _) = fp
    DocAst.Expression.Ascription(DocAst.Expression.Var(sym), TypePrinter.print(tpe))
  }

  /**
    * Returns the [[DocAst.TypeParam]] representation of `tp`.
    */
  private def printTypeParam(tp: LoweredAst.TypeParam): DocAst.TypeParam = tp match {
    case LoweredAst.TypeParam(_, sym, _) =>
      DocAst.TypeParam(sym)
  }
}

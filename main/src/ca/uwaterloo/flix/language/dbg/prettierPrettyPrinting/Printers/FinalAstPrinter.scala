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

package ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.Printers

import ca.uwaterloo.flix.language.ast.FinalAst
import ca.uwaterloo.flix.language.ast.FinalAst.Expression
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.DocAst

object FinalAstPrinter {

  /**
    * Returns the [[DocAst.Program]] representation of `root`.
    */
  def print(root: FinalAst.Root): DocAst.Program = {
    val enums = root.enums.values.map {
      case FinalAst.Enum(ann, mod, sym, cases0, _, _) =>
        val cases = cases0.values.map {
          case FinalAst.Case(sym, _, _) => DocAst.Case(sym)
        }.toList
        DocAst.Enum(ann, mod, sym, cases)
    }.toList
    val defs = root.defs.values.map {
      case FinalAst.Def(ann, mod, sym, formals, exp, tpe, _) =>
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
    * Returns the [[DocAst]] representation of `e`.
    */
  def print(e: FinalAst.Expression): DocAst = e match {
    case Expression.Cst(cst, _, _) => DocAst.Cst(cst)
    case Expression.Var(sym, _, _) => DocAst.Var(sym)
    case Expression.Closure(sym, closureArgs, _, _) => DocAst.ClosureLifted(sym, closureArgs.map(print))
    case Expression.ApplyClo(exp, args, _, _) => DocAst.AppClo(print(exp), args.map(print))
    case Expression.ApplyDef(sym, args, _, _) => DocAst.App(sym, args.map(print))
    case Expression.ApplyCloTail(exp, args, _, _) => DocAst.AppCloTail(print(exp), args.map(print))
    case Expression.ApplyDefTail(sym, args, _, _) => DocAst.AppDefTail(sym, args.map(print))
    case Expression.ApplySelfTail(sym, _, actuals, _, _) => DocAst.AppSelfTail(sym, actuals.map(print))
    case Expression.Unary(sop, _, exp, _, _) => DocAst.Unary(OperatorPrinter.print(sop), print(exp))
    case Expression.Binary(sop, _, exp1, exp2, _, _) => DocAst.Binary(print(exp1), OperatorPrinter.print(sop), print(exp2))
    case Expression.IfThenElse(exp1, exp2, exp3, _, _) => DocAst.IfThenElse(print(exp1), print(exp2), print(exp3))
    case Expression.Branch(exp, branches, _, _) =>
      val bs = branches.map {
        case (bsym, bexp) => (bsym, print(bexp))
      }
      DocAst.Branch(print(exp), bs)
    case Expression.JumpTo(sym, _, _) => DocAst.JumpTo(sym)
    case Expression.Let(sym, exp1, exp2, _, _) => DocAst.Let(DocAst.Var(sym), None, print(exp1), print(exp2))
    case Expression.LetRec(varSym, _, _, exp1, exp2, _, _) => DocAst.LetRec(DocAst.Var(varSym), None, print(exp1), print(exp2))
    case Expression.Region(_, _) => DocAst.Region
    case Expression.Scope(sym, exp, _, _) => DocAst.Scope(DocAst.Var(sym), print(exp))
    case Expression.ScopeExit(exp1, exp2, _, _) => DocAst.ScopeExit(print(exp1), print(exp2))
    case Expression.Is(sym, exp, _) => DocAst.IsTag(sym, print(exp))
    case Expression.Tag(sym, exp, _, _) => DocAst.Tag(sym, List(print(exp)))
    case Expression.Untag(sym, exp, _, _) => DocAst.Untag(sym, print(exp))
    case Expression.Index(base, offset, _, _) => DocAst.Index(offset, print(base))
    case Expression.Tuple(elms, _, _) => DocAst.Tuple(elms.map(print))
    case Expression.RecordEmpty(_, _) => DocAst.RecordEmpty
    case Expression.RecordSelect(exp, field, _, _) => DocAst.RecordSelect(field, print(exp))
    case Expression.RecordExtend(field, value, rest, _, _) => DocAst.RecordExtend(field, print(value), print(rest))
    case Expression.RecordRestrict(field, rest, _, _) => DocAst.RecordRestrict(field, print(rest))
    case Expression.ArrayLit(elms, _, _) => DocAst.ArrayLit(elms.map(print))
    case Expression.ArrayNew(elm, len, _, _) => DocAst.ArrayNew(print(elm), print(len))
    case Expression.ArrayLoad(base, index, _, _) => DocAst.ArrayLoad(print(base), print(index))
    case Expression.ArrayStore(base, index, elm, _, _) => DocAst.ArrayStore(print(base), print(index), print(elm))
    case Expression.ArrayLength(base, _, _) => DocAst.ArrayLength(print(base))
    case Expression.Ref(exp, _, _) => DocAst.Ref(print(exp))
    case Expression.Deref(exp, _, _) => DocAst.Deref(print(exp))
    case Expression.Assign(exp1, exp2, _, _) => DocAst.Assign(print(exp1), print(exp2))
    case Expression.Cast(exp, tpe, _) => DocAst.Cast(print(exp), MonoTypePrinter.print(tpe))
    case Expression.TryCatch(exp, rules, _, _) =>
      val rs = rules.map {
        case FinalAst.CatchRule(sym, clazz, exp) => (sym, clazz, print(exp))
      }
      DocAst.TryCatch(print(exp), rs)
    case Expression.InvokeConstructor(constructor, args, _, _) => DocAst.JavaInvokeConstructor(constructor, args.map(print))
    case Expression.InvokeMethod(method, exp, args, _, _) => DocAst.JavaInvokeMethod(method, print(exp), args.map(print))
    case Expression.InvokeStaticMethod(method, args, _, _) => DocAst.JavaInvokeStaticMethod(method, args.map(print))
    case Expression.GetField(field, exp, _, _) => DocAst.JavaGetField(field, print(exp))
    case Expression.PutField(field, exp1, exp2, _, _) => DocAst.JavaPutField(field, print(exp1), print(exp2))
    case Expression.GetStaticField(field, _, _) => DocAst.JavaGetStaticField(field)
    case Expression.PutStaticField(field, exp, _, _) => DocAst.JavaPutStaticField(field, print(exp))
    case Expression.NewObject(name, clazz, tpe, methods, _) =>
      val ms = methods.map{
        case FinalAst.JvmMethod(ident, fparams, clo, retTpe, _) =>
          DocAst.JvmMethod(ident, fparams.map(printFormalParam), print(clo), MonoTypePrinter.print(retTpe))
      }
      DocAst.NewObject(name, clazz, MonoTypePrinter.print(tpe), ms)
    case Expression.Spawn(exp1, exp2, _, _) => DocAst.Spawn(print(exp1), print(exp2))
    case Expression.Lazy(exp, _, _) => DocAst.Lazy(print(exp))
    case Expression.Force(exp, _, _) => DocAst.Force(print(exp))
    case Expression.HoleError(sym, _, _) => DocAst.HoleError(sym)
    case Expression.MatchError(_, _) => DocAst.MatchError
  }

  /**
    * Returns the [[DocAst.Ascription]] representation of `fp`.
    */
  private def printFormalParam(fp: FinalAst.FormalParam): DocAst.Ascription = {
    val FinalAst.FormalParam(sym, tpe) = fp
    DocAst.Ascription(DocAst.VarWithOffset(sym), MonoTypePrinter.print(tpe))
  }

}

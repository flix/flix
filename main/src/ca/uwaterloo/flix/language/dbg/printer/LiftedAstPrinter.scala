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

import ca.uwaterloo.flix.language.ast.{LiftedAst, Symbol}
import ca.uwaterloo.flix.language.ast.LiftedAst.Expression
import ca.uwaterloo.flix.language.ast.LiftedAst.Expression._
import ca.uwaterloo.flix.language.dbg.DocAst

object LiftedAstPrinter {

  /**
    * Returns the [[DocAst.Program]] representation of `root`.
    */
  def print(root: LiftedAst.Root): DocAst.Program = {
    val enums = root.enums.values.map {
      case LiftedAst.Enum(ann, mod, sym, cases0, _, _) =>
        val cases = cases0.values.map {
          case LiftedAst.Case(sym, _, _) => DocAst.Case(sym)
        }.toList
        DocAst.Enum(ann, mod, sym, cases)
    }.toList
    val defs = root.defs.values.map {
      case LiftedAst.Def(ann, mod, sym, formals, exp, tpe, _) =>
        DocAst.Def(
          ann,
          mod,
          sym,
          formals.map(printFormalParam),
          TypePrinter.print(tpe),
          print(exp)
        )
    }.toList
    DocAst.Program(enums, defs)
  }

  /**
    * Returns the [[DocAst.Expression]] representation of `e`.
    */
  def print(e: LiftedAst.Expression): DocAst.Expression = e match {
    case Cst(cst, _, _) => ConstantPrinter.print(cst)
    case Var(sym, _, _) => printVarSym(sym)
    case Closure(sym, closureArgs, _, _) => DocAst.Expression.ClosureLifted(sym, closureArgs.map(print))
    case ApplyClo(exp, args, _, _, _) => DocAst.Expression.ApplyClo(print(exp), args.map(print))
    case ApplyDef(sym, args, _, _, _) => DocAst.Expression.ApplyDef(sym, args.map(print))
    case ApplyCloTail(exp, args, _, _, _) => DocAst.Expression.ApplyCloTail(print(exp), args.map(print))
    case ApplyDefTail(sym, args, _, _, _) => DocAst.Expression.ApplyDefTail(sym, args.map(print))
    case ApplySelfTail(sym, _, actuals, _, _, _) => DocAst.Expression.ApplySelfTail(sym, actuals.map(print))
    case Unary(sop, exp, _, _, _) => DocAst.Expression.Unary(OperatorPrinter.print(sop), print(exp))
    case Binary(sop, exp1, exp2, _, _, _) => DocAst.Expression.Binary(print(exp1), OperatorPrinter.print(sop), print(exp2))
    case IfThenElse(exp1, exp2, exp3, _, _, _) => DocAst.Expression.IfThenElse(print(exp1), print(exp2), print(exp3))
    case Branch(exp, branches, _, _, _) => DocAst.Expression.Branch(print(exp), branches.view.mapValues(print).toMap)
    case JumpTo(sym, _, _, _) => DocAst.Expression.JumpTo(sym)
    case Let(sym, exp1, exp2, _, _, _) => DocAst.Expression.Let(printVarSym(sym), None, print(exp1), print(exp2))
    case LetRec(varSym, _, _, exp1, exp2, _, _, _) => DocAst.Expression.LetRec(printVarSym(varSym), None, print(exp1), print(exp2))
    case Region(_, _) => DocAst.Expression.Region
    case Scope(sym, exp, _, _, _) => DocAst.Expression.Scope(printVarSym(sym), print(exp))
    case ScopeExit(exp1, exp2, _, _, _) => DocAst.Expression.ScopeExit(print(exp1), print(exp2))
    case Is(sym, exp, _, _) => DocAst.Expression.Is(sym, print(exp))
    case Tag(sym, exp, _, _, _) => DocAst.Expression.Tag(sym, List(print(exp)))
    case Untag(sym, exp, _, _, _) => DocAst.Expression.Untag(sym, print(exp))
    case Index(base, offset, _, _, _) => DocAst.Expression.Index(offset, print(base))
    case Tuple(elms, _, _, _) => DocAst.Expression.Tuple(elms.map(print))
    case RecordEmpty(_, _) => DocAst.Expression.RecordEmpty
    case RecordSelect(exp, field, _, _, _) => DocAst.Expression.RecordSelect(field, print(exp))
    case RecordExtend(field, value, rest, _, _, _) => DocAst.Expression.RecordExtend(field, print(value), print(rest))
    case RecordRestrict(field, rest, _, _, _) => DocAst.Expression.RecordRestrict(field, print(rest))
    case ArrayLit(elms, _, _) => DocAst.Expression.ArrayLit(elms.map(print))
    case ArrayNew(elm, len, _, _) => DocAst.Expression.ArrayNew(print(elm), print(len))
    case ArrayLoad(base, index, _, _) => DocAst.Expression.ArrayLoad(print(base), print(index))
    case ArrayStore(base, index, elm, _, _) => DocAst.Expression.ArrayStore(print(base), print(index), print(elm))
    case ArrayLength(base, _, _, _) => DocAst.Expression.ArrayLength(print(base))
    case Ref(exp, _, _) => DocAst.Expression.Ref(print(exp))
    case Deref(exp, _, _) => DocAst.Expression.Deref(print(exp))
    case Assign(exp1, exp2, _, _) => DocAst.Expression.Assign(print(exp1), print(exp2))
    case InstanceOf(exp, clazz, _) => DocAst.Expression.InstanceOf(print(exp), clazz)
    case Cast(exp, tpe, _, _) => DocAst.Expression.Cast(print(exp), TypePrinter.print(tpe))
    case TryCatch(exp, rules, _, _, _) => DocAst.Expression.TryCatch(print(exp), rules.map {
      case LiftedAst.CatchRule(sym, clazz, rexp) => (sym, clazz, print(rexp))
    })
    case InvokeConstructor(constructor, args, _, _, _) => DocAst.Expression.JavaInvokeConstructor(constructor, args.map(print))
    case InvokeMethod(method, exp, args, _, _, _) => DocAst.Expression.JavaInvokeMethod(method, print(exp), args.map(print))
    case InvokeStaticMethod(method, args, _, _, _) => DocAst.Expression.JavaInvokeStaticMethod(method, args.map(print))
    case GetField(field, exp, _, _, _) => DocAst.Expression.JavaGetField(field, print(exp))
    case PutField(field, exp1, exp2, _, _, _) => DocAst.Expression.JavaPutField(field, print(exp1), print(exp2))
    case GetStaticField(field, _, _, _) => DocAst.Expression.JavaGetStaticField(field)
    case PutStaticField(field, exp, _, _, _) => DocAst.Expression.JavaPutStaticField(field, print(exp))
    case NewObject(name, clazz, tpe, _, methods, _) => DocAst.Expression.NewObject(name, clazz, TypePrinter.print(tpe), methods.map {
      case LiftedAst.JvmMethod(ident, fparams, clo, retTpe, _, _) =>
        DocAst.JvmMethod(ident, fparams.map(printFormalParam), print(clo), TypePrinter.print(retTpe))
    })
    case Spawn(exp1, exp2, _, _) => DocAst.Expression.Spawn(print(exp1), print(exp2))
    case Lazy(exp, _, _) => DocAst.Expression.Lazy(print(exp))
    case Force(exp, _, _) => DocAst.Expression.Force(print(exp))
    case HoleError(sym, _, _) => DocAst.Expression.HoleError(sym)
    case MatchError(_, _) => DocAst.Expression.MatchError
  }

  /**
    * Returns the [[DocAst.Expression.Ascription]] representation of `fp`.
    */
  private def printFormalParam(fp: LiftedAst.FormalParam): DocAst.Expression.Ascription = {
    val LiftedAst.FormalParam(sym, _, tpe, _) = fp
    DocAst.Expression.Ascription(printVarSym(sym), TypePrinter.print(tpe))
  }

  /**
    * Returns the [[DocAst.Expression]] representation of `sym`.
    */
  private def printVarSym(sym: Symbol.VarSym): DocAst.Expression =
    DocAst.Expression.Var(sym)


}

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
import ca.uwaterloo.flix.language.ast.{AtomicOp, MonoTypedAst, Symbol}
import ca.uwaterloo.flix.language.ast.MonoTypedAst.Expr._
import ca.uwaterloo.flix.language.ast.MonoTypedAst.Stmt
import ca.uwaterloo.flix.language.dbg.DocAst
import ca.uwaterloo.flix.util.InternalCompilerException

object MonoTypedAstPrinter {

  /**
    * Returns the [[DocAst.Program]] representation of `root`.
    */
  def print(root: MonoTypedAst.Root): DocAst.Program = {
    val enums = root.enums.values.map {
      case MonoTypedAst.Enum(ann, mod, sym, cases0, _, _) =>
        val cases = cases0.values.map {
          case MonoTypedAst.Case(sym, _, _) => DocAst.Case(sym)
        }.toList
        DocAst.Enum(ann, mod, sym, cases)
    }.toList
    val defs = root.defs.values.map {
      case MonoTypedAst.Def(ann, mod, sym, formals, exp, tpe, _) =>
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
  def print(e: MonoTypedAst.Expr): DocAst.Expression = e match {
    case Cst(cst, _, _) => DocAst.Expression.Cst(cst)
    case Var(sym, _, _) => printVarSym(sym)
    case ApplyAtomic(op, exps, tpe, loc) => (op, exps) match {
      case (AtomicOp.Closure(sym), exps) => DocAst.Expression.ClosureLifted(sym, exps.map(print))
      case (AtomicOp.Unary(sop), List(e)) => DocAst.Expression.Unary(OperatorPrinter.print(sop), print(e))
      case (AtomicOp.Binary(sop), List(e1, e2)) => DocAst.Expression.Binary(print(e1), OperatorPrinter.print(sop), print(e2))
      case (AtomicOp.Region, Nil) => DocAst.Expression.Region
      case (AtomicOp.ScopeExit, List(exp1, exp2)) => DocAst.Expression.ScopeExit(print(exp1), print(exp2))
      case (AtomicOp.Is(sym), List(exp)) => DocAst.Expression.Is(sym, print(exp))
      case (AtomicOp.Tag(sym), List(exp)) => DocAst.Expression.Tag(sym, List(print(exp)))
      case (AtomicOp.Untag(sym), List(exp)) => DocAst.Expression.Untag(sym, print(exp))
      case (AtomicOp.Index(idx), List(exp)) => DocAst.Expression.Index(idx, print(exp))
      case (AtomicOp.Tuple, exps) => DocAst.Expression.Tuple(exps.map(print))
      case (AtomicOp.RecordEmpty, _) => DocAst.Expression.RecordEmpty
      case (AtomicOp.RecordSelect(field), List(exp)) => DocAst.Expression.RecordSelect(field, print(exp))
      case (AtomicOp.RecordExtend(field), List(exp1, exp2)) => DocAst.Expression.RecordExtend(field, print(exp1), print(exp2))
      case (AtomicOp.RecordRestrict(field), List(exp)) => DocAst.Expression.RecordRestrict(field, print(exp))
      case (AtomicOp.ArrayLit, exps) => DocAst.Expression.ArrayLit(exps.map(print))
      case (AtomicOp.ArrayNew, List(exp1, exp2)) => DocAst.Expression.ArrayNew(print(exp1), print(exp2))
      case (AtomicOp.ArrayLoad, List(exp1, exp2)) => DocAst.Expression.ArrayLoad(print(exp1), print(exp2))
      case (AtomicOp.ArrayStore, List(exp1, exp2, exp3)) => DocAst.Expression.ArrayStore(print(exp1), print(exp2), print(exp3))
      case (AtomicOp.ArrayLength, List(exp)) => DocAst.Expression.ArrayLength(print(exp))
      case (AtomicOp.Ref, List(exp)) => DocAst.Expression.Ref(print(exp))
      case (AtomicOp.Deref, List(exp)) => DocAst.Expression.Deref(print(exp))
      case (AtomicOp.Assign, List(exp1, exp2)) => DocAst.Expression.Assign(print(exp1), print(exp2))
      case (AtomicOp.InstanceOf(_), List(exp)) => DocAst.Expression.Unknown
      case (AtomicOp.Cast, List(exp)) => DocAst.Expression.Cast(print(exp), MonoTypePrinter.print(tpe))
      case (AtomicOp.InvokeConstructor(constructor), exps) => DocAst.Expression.JavaInvokeConstructor(constructor, exps.map(print))
      case (AtomicOp.InvokeMethod(method), exp :: exps) => DocAst.Expression.JavaInvokeMethod(method, print(exp), exps.map(print))
      case (AtomicOp.InvokeStaticMethod(method), exps) => DocAst.Expression.JavaInvokeStaticMethod(method, exps.map(print))
      case (AtomicOp.GetField(field), List(exp)) => DocAst.Expression.JavaGetField(field, print(exp))
      case (AtomicOp.PutField(field), List(exp1, exp2)) => DocAst.Expression.JavaPutField(field, print(exp1), print(exp2))
      case (AtomicOp.GetStaticField(field), Nil) => DocAst.Expression.JavaGetStaticField(field)
      case (AtomicOp.PutStaticField(field), List(exp)) => DocAst.Expression.JavaPutStaticField(field, print(exp))
      case (AtomicOp.Lazy, List(exp)) => DocAst.Expression.Lazy(print(exp))
      case (AtomicOp.Force, List(exp)) => DocAst.Expression.Force(print(exp))
      case (AtomicOp.HoleError(sym), Nil) => DocAst.Expression.HoleError(sym)
      case (AtomicOp.MatchError, Nil) => DocAst.Expression.MatchError
      case _ => throw InternalCompilerException("Mismatched Arity", e.loc)
    }
    case ApplyClo(exp, exps, ct, _, _) => ct match {
      case CallType.TailCall => DocAst.Expression.ApplyCloTail(print(exp), exps.map(print))
      case CallType.NonTailCall => DocAst.Expression.ApplyClo(print(exp), exps.map(print))
    }
    case ApplyDef(sym, exps, ct, _, _) => ct match {
      case CallType.TailCall => DocAst.Expression.ApplyDefTail(sym, exps.map(print))
      case CallType.NonTailCall => DocAst.Expression.ApplyDef(sym, exps.map(print))
    }
    case ApplySelfTail(sym, _, actuals, _, _) => DocAst.Expression.ApplySelfTail(sym, actuals.map(print))
    case IfThenElse(exp1, exp2, exp3, _, _) => DocAst.Expression.IfThenElse(print(exp1), print(exp2), print(exp3))
    case Branch(exp, branches, _, _) => DocAst.Expression.Branch(print(exp), branches.view.mapValues(print).toMap)
    case JumpTo(sym, _, _) => DocAst.Expression.JumpTo(sym)
    case Let(sym, exp1, exp2, _, _) => DocAst.Expression.Let(printVarSym(sym), None, print(exp1), print(exp2))
    case LetRec(varSym, _, _, exp1, exp2, _, _) => DocAst.Expression.LetRec(printVarSym(varSym), None, print(exp1), print(exp2))
    case Scope(sym, exp, _, _) => DocAst.Expression.Scope(printVarSym(sym), print(exp))
    case TryCatch(exp, rules, _, _) => DocAst.Expression.TryCatch(print(exp), rules.map {
      case MonoTypedAst.CatchRule(sym, clazz, rexp) => (sym, clazz, print(rexp))
    })
    case NewObject(name, clazz, tpe, methods, _) =>
      val ms = methods.map {
        case MonoTypedAst.JvmMethod(ident, fparams, clo, retTpe, _) =>
          DocAst.JvmMethod(ident, fparams.map(printFormalParam), print(clo), MonoTypePrinter.print(retTpe))
      }
      DocAst.Expression.NewObject(name, clazz, MonoTypePrinter.print(tpe), ms)
    case Spawn(exp1, exp2, _, _) => DocAst.Expression.Spawn(print(exp1), print(exp2))

  }

  /**
    * Returns the [[DocAst.Expression]] representation of `s`.
    */
  def print(s: MonoTypedAst.Stmt): DocAst.Expression = s match {
    case Stmt.Ret(e, tpe, loc) => print(e)
  }

  /**
    * Returns the [[DocAst.Expression.Ascription]] representation of `fp`.
    */
  private def printFormalParam(fp: MonoTypedAst.FormalParam): DocAst.Expression.Ascription = {
    val MonoTypedAst.FormalParam(sym, tpe) = fp
    DocAst.Expression.Ascription(printVarSym(sym), MonoTypePrinter.print(tpe))
  }

  /**
    * Returns the [[DocAst.Expression]] representation of `sym`.
    */
  private def printVarSym(sym: Symbol.VarSym): DocAst.Expression =
    DocAst.Expression.VarWithOffset(sym)

}

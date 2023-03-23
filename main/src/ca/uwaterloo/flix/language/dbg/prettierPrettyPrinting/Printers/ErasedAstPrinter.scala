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

import ca.uwaterloo.flix.language.ast.ErasedAst
import ca.uwaterloo.flix.language.ast.ErasedAst.Expression._
import ca.uwaterloo.flix.language.ast.ErasedAst._
import ca.uwaterloo.flix.language.dbg.prettierPrettyPrinting.DocAst

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
    case Intrinsic0(op, _, _) => op match {
      case IntrinsicOperator0.Cst(cst) => DocAst.Expression.Cst(cst)
      case IntrinsicOperator0.Region => DocAst.Expression.Region
      case IntrinsicOperator0.RecordEmpty => DocAst.Expression.RecordEmpty
      case IntrinsicOperator0.GetStaticField(field) => DocAst.Expression.JavaGetStaticField(field)
      case IntrinsicOperator0.HoleError(sym) => DocAst.Expression.HoleError(sym)
      case IntrinsicOperator0.MatchError => DocAst.Expression.MatchError
    }
    case Intrinsic1(op, exp, tpe, _) =>
      val d = print(exp)
      op match {
        case IntrinsicOperator1.Is(sym) => DocAst.Expression.IsTag(sym, d)
        case IntrinsicOperator1.Tag(sym) => DocAst.Expression.Tag(sym, List(d))
        case IntrinsicOperator1.Untag(sym) => DocAst.Expression.Untag(sym, d)
        case IntrinsicOperator1.Cast => DocAst.Expression.Cast(d, MonoTypePrinter.print(tpe))
        case IntrinsicOperator1.Index(idx) => DocAst.Expression.Index(idx, d)
        case IntrinsicOperator1.RecordSelect(field) => DocAst.Expression.RecordSelect(field, d)
        case IntrinsicOperator1.RecordRestrict(field) => DocAst.Expression.RecordRestrict(field, d)
        case IntrinsicOperator1.Ref => DocAst.Expression.Ref(d)
        case IntrinsicOperator1.Deref => DocAst.Expression.Deref(d)
        case IntrinsicOperator1.ArrayLength => DocAst.Expression.ArrayLength(d)
        case IntrinsicOperator1.Lazy => DocAst.Expression.Lazy(d)
        case IntrinsicOperator1.Force => DocAst.Expression.Force(d)
        case IntrinsicOperator1.GetField(field) => DocAst.Expression.JavaGetField(field, d)
        case IntrinsicOperator1.PutStaticField(field) => DocAst.Expression.JavaPutStaticField(field, d)
        case IntrinsicOperator1.BoxBool => DocAst.Expression.Box(d)
        case IntrinsicOperator1.BoxInt8 => DocAst.Expression.Box(d)
        case IntrinsicOperator1.BoxInt16 => DocAst.Expression.Box(d)
        case IntrinsicOperator1.BoxInt32 => DocAst.Expression.Box(d)
        case IntrinsicOperator1.BoxInt64 => DocAst.Expression.Box(d)
        case IntrinsicOperator1.BoxChar => DocAst.Expression.Box(d)
        case IntrinsicOperator1.BoxFloat32 => DocAst.Expression.Box(d)
        case IntrinsicOperator1.BoxFloat64 => DocAst.Expression.Box(d)
        case IntrinsicOperator1.UnboxBool => DocAst.Expression.Unbox(d)
        case IntrinsicOperator1.UnboxInt8 => DocAst.Expression.Unbox(d)
        case IntrinsicOperator1.UnboxInt16 => DocAst.Expression.Unbox(d)
        case IntrinsicOperator1.UnboxInt32 => DocAst.Expression.Unbox(d)
        case IntrinsicOperator1.UnboxInt64 => DocAst.Expression.Unbox(d)
        case IntrinsicOperator1.UnboxChar => DocAst.Expression.Unbox(d)
        case IntrinsicOperator1.UnboxFloat32 => DocAst.Expression.Unbox(d)
        case IntrinsicOperator1.UnboxFloat64 => DocAst.Expression.Unbox(d)
      }
    case Intrinsic2(op, exp1, exp2, _, _) =>
      val d1 = print(exp1)
      val d2 = print(exp2)
      op match {
        case IntrinsicOperator2.RecordExtend(field) => DocAst.Expression.RecordExtend(field, d1, d2)
        case IntrinsicOperator2.Assign => DocAst.Expression.Assign(d1, d2)
        case IntrinsicOperator2.ArrayNew => DocAst.Expression.ArrayNew(d1, d2)
        case IntrinsicOperator2.ArrayLoad => DocAst.Expression.ArrayLoad(d1, d2)
        case IntrinsicOperator2.Spawn => DocAst.Expression.Spawn(d1, d2)
        case IntrinsicOperator2.PutField(field) => DocAst.Expression.JavaPutField(field, d1, d2)
      }
    case Intrinsic3(op, exp1, exp2, exp3, _, _) =>
      val d1 = print(exp1)
      val d2 = print(exp2)
      val d3 = print(exp3)
      op match {
        case IntrinsicOperator3.ArrayStore => DocAst.Expression.ArrayStore(d1, d2, d3)
      }
    case IntrinsicN(op, exps, _, _) =>
      val ds = exps.map(print)
      op match {
        case IntrinsicOperatorN.Closure(sym) => DocAst.Expression.ClosureLifted(sym, ds)
        case IntrinsicOperatorN.ApplyDef(sym) => DocAst.Expression.App(sym, ds)
        case IntrinsicOperatorN.ApplyDefTail(sym) => DocAst.Expression.AppDefTail(sym, ds)
        case IntrinsicOperatorN.ApplySelfTail(sym, _) => DocAst.Expression.AppSelfTail(sym, ds)
        case IntrinsicOperatorN.Tuple => DocAst.Expression.Tuple(ds)
        case IntrinsicOperatorN.ArrayLit => DocAst.Expression.ArrayLit(ds)
        case IntrinsicOperatorN.InvokeConstructor(constructor) => DocAst.Expression.JavaInvokeConstructor(constructor, ds)
        case IntrinsicOperatorN.InvokeStaticMethod(method) => DocAst.Expression.JavaInvokeStaticMethod(method, ds)
      }
    case Intrinsic1N(op, exp, exps, _, _) =>
      val d = print(exp)
      val ds = exps.map(print)
      op match {
        case IntrinsicOperator1N.ApplyClo => DocAst.Expression.AppClo(d, ds)
        case IntrinsicOperator1N.ApplyCloTail => DocAst.Expression.AppCloTail(d, ds)
        case IntrinsicOperator1N.InvokeMethod(method) => DocAst.Expression.JavaInvokeMethod(method, d, ds)
      }
  }

  /**
    * Returns the [[DocAst.Expression.Ascription]] representation of `fp`.
    */
  private def printFormalParam(fp: ErasedAst.FormalParam): DocAst.Expression.Ascription = {
    val ErasedAst.FormalParam(sym, tpe) = fp
    DocAst.Expression.Ascription(DocAst.Expression.VarWithOffset(sym), MonoTypePrinter.print(tpe))
  }

}

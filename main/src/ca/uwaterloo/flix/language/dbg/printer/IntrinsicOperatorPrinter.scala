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

import ca.uwaterloo.flix.language.ast.ErasedAst._
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.dbg.DocAst
import ca.uwaterloo.flix.language.dbg.DocAst.Expression
import ca.uwaterloo.flix.language.dbg.DocAst.Expression._
import ca.uwaterloo.flix.util.InternalCompilerException

object IntrinsicOperatorPrinter {

  /**
    * Returns the [[DocAst.Expression]] representation of `op`.
    */
  def print(op: IntrinsicOperator, exps: List[Expression], tpe: DocAst.Type, loc: SourceLocation): Expression = (op, exps) match {
    case (IntrinsicOperator.Cst(cst), Nil) => ConstantPrinter.print(cst)
    case (IntrinsicOperator.Region, Nil) => Region
    case (IntrinsicOperator.RecordEmpty, Nil) => RecordEmpty
    case (IntrinsicOperator.GetStaticField(field), Nil) => JavaGetStaticField(field)
    case (IntrinsicOperator.HoleError(sym), Nil) => HoleError(sym)
    case (IntrinsicOperator.MatchError, Nil) => MatchError
    case (IntrinsicOperator.Unary(sop), exp :: Nil) => Unary(OperatorPrinter.print(sop), exp)
    case (IntrinsicOperator.Is(sym), exp :: Nil) => Is(sym, exp)
    case (IntrinsicOperator.Tag(sym), exp :: Nil) => Tag(sym, List(exp))
    case (IntrinsicOperator.Untag(sym), exp :: Nil) => Untag(sym, exp)
    case (IntrinsicOperator.InstanceOf(_), exp :: Nil) => Unknown
    case (IntrinsicOperator.Cast, exp :: Nil) => Cast(exp, tpe)
    case (IntrinsicOperator.Index(idx), exp :: Nil) => Index(idx, exp)
    case (IntrinsicOperator.RecordSelect(field), exp :: Nil) => RecordSelect(field, exp)
    case (IntrinsicOperator.RecordRestrict(field), exp :: Nil) => RecordRestrict(field, exp)
    case (IntrinsicOperator.Ref, exp :: Nil) => Ref(exp)
    case (IntrinsicOperator.Deref, exp :: Nil) => Deref(exp)
    case (IntrinsicOperator.ArrayLength, exp :: Nil) => ArrayLength(exp)
    case (IntrinsicOperator.Lazy, exp :: Nil) => Lazy(exp)
    case (IntrinsicOperator.Force, exp :: Nil) => Force(exp)
    case (IntrinsicOperator.Spawn, exp1 :: exp2 :: Nil) => Spawn(exp1, exp2)
    case _ => throw InternalCompilerException("Unexpected pattern at IntrinsicOperatorPrinter", loc)
  }

  /**
    * Returns the [[DocAst.Expression]] representation of `op`.
    */
  def print(op: IntrinsicOperator1, d: Expression, tpe: DocAst.Type): Expression = op match {
    case IntrinsicOperator1.GetField(field) => JavaGetField(field, d)
    case IntrinsicOperator1.PutStaticField(field) => JavaPutStaticField(field, d)
    case IntrinsicOperator1.BoxBool => Box(d)
    case IntrinsicOperator1.BoxInt8 => Box(d)
    case IntrinsicOperator1.BoxInt16 => Box(d)
    case IntrinsicOperator1.BoxInt32 => Box(d)
    case IntrinsicOperator1.BoxInt64 => Box(d)
    case IntrinsicOperator1.BoxChar => Box(d)
    case IntrinsicOperator1.BoxFloat32 => Box(d)
    case IntrinsicOperator1.BoxFloat64 => Box(d)
    case IntrinsicOperator1.UnboxBool => Unbox(d)
    case IntrinsicOperator1.UnboxInt8 => Unbox(d)
    case IntrinsicOperator1.UnboxInt16 => Unbox(d)
    case IntrinsicOperator1.UnboxInt32 => Unbox(d)
    case IntrinsicOperator1.UnboxInt64 => Unbox(d)
    case IntrinsicOperator1.UnboxChar => Unbox(d)
    case IntrinsicOperator1.UnboxFloat32 => Unbox(d)
    case IntrinsicOperator1.UnboxFloat64 => Unbox(d)
  }

  /**
    * Returns the [[DocAst.Expression]] representation of `op`.
    */
  def print(op: IntrinsicOperator2, d1: Expression, d2: Expression): Expression = op match {
    case IntrinsicOperator2.RecordExtend(field) => RecordExtend(field, d1, d2)
    case IntrinsicOperator2.Assign => Assign(d1, d2)
    case IntrinsicOperator2.ArrayNew => ArrayNew(d1, d2)
    case IntrinsicOperator2.ArrayLoad => ArrayLoad(d1, d2)
    case IntrinsicOperator2.ScopeExit => ScopeExit(d1, d2)
    case IntrinsicOperator2.PutField(field) => JavaPutField(field, d1, d2)
  }

  /**
    * Returns the [[DocAst.Expression]] representation of `op`.
    */
  def print(op: IntrinsicOperator3, d1: Expression, d2: Expression, d3: Expression): Expression = op match {
    case IntrinsicOperator3.ArrayStore => ArrayStore(d1, d2, d3)
  }

  /**
    * Returns the [[DocAst.Expression]] representation of `op`.
    */
  def print(op: IntrinsicOperatorN, ds: List[Expression]): Expression = op match {
    case IntrinsicOperatorN.Closure(sym) => ClosureLifted(sym, ds)
    case IntrinsicOperatorN.ApplyDef(sym) => App(sym, ds)
    case IntrinsicOperatorN.ApplyDefTail(sym) => AppDefTail(sym, ds)
    case IntrinsicOperatorN.ApplySelfTail(sym, _) => AppSelfTail(sym, ds)
    case IntrinsicOperatorN.Tuple => Tuple(ds)
    case IntrinsicOperatorN.ArrayLit => ArrayLit(ds)
    case IntrinsicOperatorN.InvokeConstructor(constructor) => JavaInvokeConstructor(constructor, ds)
    case IntrinsicOperatorN.InvokeStaticMethod(method) => JavaInvokeStaticMethod(method, ds)
  }

  /**
    * Returns the [[DocAst.Expression]] representation of `op`.
    */
  def print(op: IntrinsicOperator1N, d: Expression, ds: List[Expression]): Expression = op match {
    case IntrinsicOperator1N.ApplyClo => AppClo(d, ds)
    case IntrinsicOperator1N.ApplyCloTail => AppCloTail(d, ds)
    case IntrinsicOperator1N.InvokeMethod(method) => JavaInvokeMethod(method, d, ds)
  }

}

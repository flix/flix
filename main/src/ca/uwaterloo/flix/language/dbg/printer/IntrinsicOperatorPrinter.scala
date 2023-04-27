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
  def print(op: IntrinsicOperator1, d: Expression, tpe: DocAst.Type): Expression = op match {
    case IntrinsicOperator1.Unary(sop) => Unary(OperatorPrinter.print(sop), d)
    case IntrinsicOperator1.Is(sym) => Is(sym, d)
    case IntrinsicOperator1.Tag(sym) => Tag(sym, List(d))
    case IntrinsicOperator1.Untag(sym) => Untag(sym, d)
    case IntrinsicOperator1.InstanceOf(_) => Unknown
    case IntrinsicOperator1.Cast => Cast(d, tpe)
    case IntrinsicOperator1.Index(idx) => Index(idx, d)
    case IntrinsicOperator1.RecordSelect(field) => RecordSelect(field, d)
    case IntrinsicOperator1.RecordRestrict(field) => RecordRestrict(field, d)
    case IntrinsicOperator1.Ref => Ref(d)
    case IntrinsicOperator1.Deref => Deref(d)
    case IntrinsicOperator1.ArrayLength => ArrayLength(d)
    case IntrinsicOperator1.Lazy => Lazy(d)
    case IntrinsicOperator1.Force => Force(d)
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
  def print(op: IntrinsicOp, ds: List[Expression]): Expression = (op, ds) match {
    case (IntrinsicOp.Region, Nil) => Region
    case (IntrinsicOp.RecordEmpty, Nil) => RecordEmpty
    case (IntrinsicOp.GetStaticField(field), Nil) => JavaGetStaticField(field)
    case (IntrinsicOp.HoleError(sym), Nil) => HoleError(sym)
    case (IntrinsicOp.MatchError, Nil) => MatchError

    case (IntrinsicOp.Closure(sym), _) => ClosureLifted(sym, ds)
    case (IntrinsicOp.ApplyDef(sym), _) => App(sym, ds)
    case (IntrinsicOp.ApplyDefTail(sym), _) => AppDefTail(sym, ds)
    case (IntrinsicOp.ApplySelfTail(sym, _), _) => AppSelfTail(sym, ds)
    case (IntrinsicOp.Tuple, _) => Tuple(ds)
    case (IntrinsicOp.ArrayLit, _) => ArrayLit(ds)
    case (IntrinsicOp.InvokeConstructor(constructor), _) => JavaInvokeConstructor(constructor, ds)
    case (IntrinsicOp.InvokeStaticMethod(method), _) => JavaInvokeStaticMethod(method, ds)

    case (IntrinsicOp.RecordExtend(field), d1 :: d2 :: Nil) => RecordExtend(field, d1, d2)
    case (IntrinsicOp.Assign, d1 :: d2 :: Nil) => Assign(d1, d2)
    case (IntrinsicOp.ArrayNew, d1 :: d2 :: Nil) => ArrayNew(d1, d2)
    case (IntrinsicOp.ArrayLoad, d1 :: d2 :: Nil) => ArrayLoad(d1, d2)
    case (IntrinsicOp.Spawn, d1 :: d2 :: Nil) => Spawn(d1, d2)
    case (IntrinsicOp.ScopeExit, d1 :: d2 :: Nil) => ScopeExit(d1, d2)
    case (IntrinsicOp.PutField(field), d1 :: d2 :: Nil) => JavaPutField(field, d1, d2)

    case (IntrinsicOp.ArrayStore, d1 :: d2 :: d3 :: Nil) => ArrayStore(d1, d2, d3)

    case _ => throw InternalCompilerException("Mismatched Arity", SourceLocation.Unknown)
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

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
  def print(op: IntrinsicOp, ds: List[Expression], tpe: DocAst.Type): Expression = (op, ds) match {
    case (IntrinsicOp.Region, Nil) => Region
    case (IntrinsicOp.RecordEmpty, Nil) => RecordEmpty
    case (IntrinsicOp.GetStaticField(field), Nil) => JavaGetStaticField(field)
    case (IntrinsicOp.HoleError(sym), Nil) => HoleError(sym)
    case (IntrinsicOp.MatchError, Nil) => MatchError

    case (IntrinsicOp.Unary(sop), List(d)) => Unary(OperatorPrinter.print(sop), d)
    case (IntrinsicOp.Is(sym), List(d)) => Is(sym, d)
    case (IntrinsicOp.Tag(sym), List(d)) => Tag(sym, List(d))
    case (IntrinsicOp.Untag(sym), List(d)) => Untag(sym, d)
    case (IntrinsicOp.InstanceOf(_), List(d)) => Unknown
    case (IntrinsicOp.Cast, List(d)) => Cast(d, tpe)
    case (IntrinsicOp.Index(idx), List(d)) => Index(idx, d)
    case (IntrinsicOp.RecordSelect(field), List(d)) => RecordSelect(field, d)
    case (IntrinsicOp.RecordRestrict(field), List(d)) => RecordRestrict(field, d)
    case (IntrinsicOp.Ref, List(d)) => Ref(d)
    case (IntrinsicOp.Deref, List(d)) => Deref(d)
    case (IntrinsicOp.ArrayLength, List(d)) => ArrayLength(d)
    case (IntrinsicOp.Lazy, List(d)) => Lazy(d)
    case (IntrinsicOp.Force, List(d)) => Force(d)
    case (IntrinsicOp.GetField(field), List(d)) => JavaGetField(field, d)
    case (IntrinsicOp.PutStaticField(field), List(d)) => JavaPutStaticField(field, d)
    case (IntrinsicOp.BoxBool, List(d)) => Box(d)
    case (IntrinsicOp.BoxInt8, List(d)) => Box(d)
    case (IntrinsicOp.BoxInt16, List(d)) => Box(d)
    case (IntrinsicOp.BoxInt32, List(d)) => Box(d)
    case (IntrinsicOp.BoxInt64, List(d)) => Box(d)
    case (IntrinsicOp.BoxChar, List(d)) => Box(d)
    case (IntrinsicOp.BoxFloat32, List(d)) => Box(d)
    case (IntrinsicOp.BoxFloat64, List(d)) => Box(d)
    case (IntrinsicOp.UnboxBool, List(d)) => Unbox(d)
    case (IntrinsicOp.UnboxInt8, List(d)) => Unbox(d)
    case (IntrinsicOp.UnboxInt16, List(d)) => Unbox(d)
    case (IntrinsicOp.UnboxInt32, List(d)) => Unbox(d)
    case (IntrinsicOp.UnboxInt64, List(d)) => Unbox(d)
    case (IntrinsicOp.UnboxChar, List(d)) => Unbox(d)
    case (IntrinsicOp.UnboxFloat32, List(d)) => Unbox(d)
    case (IntrinsicOp.UnboxFloat64, List(d)) => Unbox(d)

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

    case (IntrinsicOp.ApplyClo, d :: rs) => AppClo(d, rs)
    case (IntrinsicOp.ApplyCloTail, d :: rs) => AppCloTail(d, rs)
    case (IntrinsicOp.InvokeMethod(method), d :: rs) => JavaInvokeMethod(method, d, rs)

    case _ => throw InternalCompilerException("Mismatched Arity", SourceLocation.Unknown)
  }


}

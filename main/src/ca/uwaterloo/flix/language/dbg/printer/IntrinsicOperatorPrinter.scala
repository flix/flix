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

import ca.uwaterloo.flix.language.ast.{AtomicOp, SourceLocation}
import ca.uwaterloo.flix.language.dbg.DocAst
import ca.uwaterloo.flix.language.dbg.DocAst.Expression
import ca.uwaterloo.flix.language.dbg.DocAst.Expression._
import ca.uwaterloo.flix.util.InternalCompilerException

object IntrinsicOperatorPrinter {

  /**
    * Returns the [[DocAst.Expression]] representation of `op`.
    */
  def print(op: AtomicOp, ds: List[Expression], tpe: DocAst.Type): Expression = (op, ds) match {
    case (AtomicOp.Region, Nil) => Region
    case (AtomicOp.RecordEmpty, Nil) => RecordEmpty
    case (AtomicOp.GetStaticField(field), Nil) => JavaGetStaticField(field)
    case (AtomicOp.HoleError(sym), Nil) => HoleError(sym)
    case (AtomicOp.MatchError, Nil) => MatchError

    case (AtomicOp.Unary(sop), List(d)) => Unary(OperatorPrinter.print(sop), d)
    case (AtomicOp.Is(sym), List(d)) => Is(sym, d)
    case (AtomicOp.Tag(sym), List(d)) => Tag(sym, List(d))
    case (AtomicOp.Untag(sym), List(d)) => Untag(sym, d)
    case (AtomicOp.InstanceOf(_), List(d)) => Unknown
    case (AtomicOp.Cast, List(d)) => Cast(d, tpe)
    case (AtomicOp.Index(idx), List(d)) => Index(idx, d)
    case (AtomicOp.RecordSelect(field), List(d)) => RecordSelect(field, d)
    case (AtomicOp.RecordRestrict(field), List(d)) => RecordRestrict(field, d)
    case (AtomicOp.Ref, List(d)) => Ref(d)
    case (AtomicOp.Deref, List(d)) => Deref(d)
    case (AtomicOp.ArrayLength, List(d)) => ArrayLength(d)
    case (AtomicOp.Lazy, List(d)) => Lazy(d)
    case (AtomicOp.Force, List(d)) => Force(d)
    case (AtomicOp.GetField(field), List(d)) => JavaGetField(field, d)
    case (AtomicOp.PutStaticField(field), List(d)) => JavaPutStaticField(field, d)
    case (AtomicOp.BoxBool, List(d)) => Box(d)
    case (AtomicOp.BoxInt8, List(d)) => Box(d)
    case (AtomicOp.BoxInt16, List(d)) => Box(d)
    case (AtomicOp.BoxInt32, List(d)) => Box(d)
    case (AtomicOp.BoxInt64, List(d)) => Box(d)
    case (AtomicOp.BoxChar, List(d)) => Box(d)
    case (AtomicOp.BoxFloat32, List(d)) => Box(d)
    case (AtomicOp.BoxFloat64, List(d)) => Box(d)
    case (AtomicOp.UnboxBool, List(d)) => Unbox(d)
    case (AtomicOp.UnboxInt8, List(d)) => Unbox(d)
    case (AtomicOp.UnboxInt16, List(d)) => Unbox(d)
    case (AtomicOp.UnboxInt32, List(d)) => Unbox(d)
    case (AtomicOp.UnboxInt64, List(d)) => Unbox(d)
    case (AtomicOp.UnboxChar, List(d)) => Unbox(d)
    case (AtomicOp.UnboxFloat32, List(d)) => Unbox(d)
    case (AtomicOp.UnboxFloat64, List(d)) => Unbox(d)

    case (AtomicOp.Closure(sym), _) => ClosureLifted(sym, ds)
    case (AtomicOp.Tuple, _) => Tuple(ds)
    case (AtomicOp.ArrayLit, _) => ArrayLit(ds)
    case (AtomicOp.InvokeConstructor(constructor), _) => JavaInvokeConstructor(constructor, ds)
    case (AtomicOp.InvokeStaticMethod(method), _) => JavaInvokeStaticMethod(method, ds)

    case (AtomicOp.RecordExtend(field), d1 :: d2 :: Nil) => RecordExtend(field, d1, d2)
    case (AtomicOp.Assign, d1 :: d2 :: Nil) => Assign(d1, d2)
    case (AtomicOp.ArrayNew, d1 :: d2 :: Nil) => ArrayNew(d1, d2)
    case (AtomicOp.ArrayLoad, d1 :: d2 :: Nil) => ArrayLoad(d1, d2)
    case (AtomicOp.Spawn, d1 :: d2 :: Nil) => Spawn(d1, d2)
    case (AtomicOp.ScopeExit, d1 :: d2 :: Nil) => ScopeExit(d1, d2)
    case (AtomicOp.PutField(field), d1 :: d2 :: Nil) => JavaPutField(field, d1, d2)

    case (AtomicOp.ArrayStore, d1 :: d2 :: d3 :: Nil) => ArrayStore(d1, d2, d3)

    case (AtomicOp.InvokeMethod(method), d :: rs) => JavaInvokeMethod(method, d, rs)

    case (op, exps) => App(AsIs(op.toString), exps)
  }


}

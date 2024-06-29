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

import ca.uwaterloo.flix.language.ast.SemanticOp._
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.dbg.DocAst
import ca.uwaterloo.flix.language.dbg.DocAst.Expr
import ca.uwaterloo.flix.language.dbg.DocAst.Expr._

object OpPrinter {

  private val and = "and"
  private val div = "/"
  private val eq = "=="
  private val exp = "**"
  private val ge = ">="
  private val gt = ">"
  private val le = "<="
  private val lt = "<"
  private val minus = "-"
  private val mul = "*"
  private val neg = "-"
  private val neq = "!="
  private val not = "!"
  private val or = "or"
  private val plus = "+"
  private val rem = "rem"
  private val shl = "shl"
  private val shr = "shr"
  private val xor = "xor"

  /**
    * Returns the string representation of `so`.
    */
  def print(so: SemanticOp): String = so match {
    case BoolOp.Not |
         Int8Op.Not |
         Int16Op.Not |
         Int32Op.Not |
         Int64Op.Not => not
    case BoolOp.And |
         Int8Op.And |
         Int16Op.And |
         Int32Op.And => and
    case BoolOp.Or |
         Int8Op.Or |
         Int16Op.Or |
         Int32Op.Or |
         Int64Op.Or => or
    case BoolOp.Eq |
         Float32Op.Eq |
         CharOp.Eq |
         Float64Op.Eq |
         Int8Op.Eq |
         Int16Op.Eq |
         Int32Op.Eq |
         Int64Op.Eq => eq
    case BoolOp.Neq |
         CharOp.Neq |
         Float32Op.Neq |
         Float64Op.Neq |
         Int8Op.Neq |
         Int16Op.Neq |
         Int32Op.Neq |
         Int64Op.Neq => neq
    case CharOp.Lt |
         Float32Op.Lt |
         Float64Op.Lt |
         Int8Op.Lt |
         Int16Op.Lt |
         Int32Op.Lt |
         Int64Op.Lt => lt
    case CharOp.Le |
         Float32Op.Le |
         Float64Op.Le |
         Int8Op.Le |
         Int16Op.Le |
         Int32Op.Le |
         Int64Op.Le => le
    case CharOp.Gt |
         Float32Op.Gt |
         Float64Op.Gt |
         Int8Op.Gt |
         Int16Op.Gt |
         Int32Op.Gt |
         Int64Op.Gt => gt
    case CharOp.Ge |
         Float32Op.Ge |
         Float64Op.Ge |
         Int8Op.Ge |
         Int16Op.Ge |
         Int32Op.Ge |
         Int64Op.Ge => ge
    case Float32Op.Add |
         Float64Op.Add |
         Int8Op.Add |
         Int16Op.Add |
         Int32Op.Add |
         Int64Op.Add |
         Int64Op.And |
         StringOp.Concat => plus
    case Float32Op.Sub |
         Float64Op.Sub |
         Int8Op.Sub |
         Int16Op.Sub |
         Int32Op.Sub |
         Int64Op.Sub => minus
    case Float32Op.Mul |
         Float64Op.Mul |
         Int8Op.Mul |
         Int16Op.Mul |
         Int32Op.Mul |
         Int64Op.Mul => mul
    case Float32Op.Div |
         Float64Op.Div |
         Int8Op.Div |
         Int16Op.Div |
         Int32Op.Div |
         Int64Op.Div => div
    case Float32Op.Exp |
         Float64Op.Exp |
         Int8Op.Exp |
         Int16Op.Exp |
         Int32Op.Exp |
         Int64Op.Exp => exp
    case Float32Op.Neg |
         Float64Op.Neg |
         Int8Op.Neg |
         Int16Op.Neg |
         Int32Op.Neg |
         Int64Op.Neg => neg
    case Int8Op.Rem |
         Int16Op.Rem |
         Int32Op.Rem |
         Int64Op.Rem => rem
    case Int8Op.Xor |
         Int16Op.Xor |
         Int32Op.Xor |
         Int64Op.Xor => xor
    case Int8Op.Shl |
         Int16Op.Shl |
         Int32Op.Shl |
         Int64Op.Shl => shl
    case Int8Op.Shr |
         Int16Op.Shr |
         Int32Op.Shr |
         Int64Op.Shr => shr
  }

  /**
    * Returns the [[DocAst.Expr]] representation of `op`.
    */
  def print(op: AtomicOp, ds: List[Expr], tpe: DocAst.Type): Expr = (op, ds) match {
    case (AtomicOp.Region, Nil) => Region
    case (AtomicOp.RecordEmpty, Nil) => RecordEmpty
    case (AtomicOp.GetStaticField(field), Nil) => JavaGetStaticField(field)
    case (AtomicOp.HoleError(sym), Nil) => HoleError(sym)
    case (AtomicOp.MatchError, Nil) => MatchError
    case (AtomicOp.Unary(sop), List(d)) => Unary(OpPrinter.print(sop), d)
    case (AtomicOp.Binary(sop), List(d1, d2)) => Binary(d1, OpPrinter.print(sop), d2)
    case (AtomicOp.Is(sym), List(d)) => Is(sym, d)
    case (AtomicOp.Tag(sym), List(d)) => Tag(sym, List(d))
    case (AtomicOp.Untag(sym), List(d)) => Untag(sym, d)
    case (AtomicOp.InstanceOf(clazz), List(d)) => InstanceOf(d, clazz)
    case (AtomicOp.Cast, List(d)) => Cast(d, tpe)
    case (AtomicOp.Unbox, List(d)) => Unbox(d, tpe)
    case (AtomicOp.Box, List(d)) => Box(d)
    case (AtomicOp.Index(idx), List(d)) => Index(idx, d)
    case (AtomicOp.RecordSelect(label), List(d)) => RecordSelect(label, d)
    case (AtomicOp.RecordRestrict(label), List(d)) => RecordRestrict(label, d)
    case (AtomicOp.Ref, List(d)) => Ref(d)
    case (AtomicOp.Deref, List(d)) => Deref(d)
    case (AtomicOp.ArrayLength, List(d)) => ArrayLength(d)
    case (AtomicOp.Lazy, List(d)) => Lazy(d)
    case (AtomicOp.Force, List(d)) => Force(d)
    case (AtomicOp.GetField(field), List(d)) => JavaGetField(field, d)
    case (AtomicOp.PutStaticField(field), List(d)) => JavaPutStaticField(field, d)
    case (AtomicOp.Closure(sym), _) => ClosureLifted(sym, ds)
    case (AtomicOp.Tuple, _) => Tuple(ds)
    case (AtomicOp.ArrayLit, _) => ArrayLit(ds)
    case (AtomicOp.InvokeConstructor(constructor), _) => JavaInvokeConstructor(constructor, ds)
    case (AtomicOp.InvokeStaticMethod(method), _) => JavaInvokeStaticMethod(method, ds)
    case (AtomicOp.RecordExtend(label), List(d1, d2)) => RecordExtend(label, d1, d2)
    case (AtomicOp.Assign, List(d1, d2)) => Assign(d1, d2)
    case (AtomicOp.ArrayNew, List(d1, d2)) => ArrayNew(d1, d2)
    case (AtomicOp.ArrayLoad, List(d1, d2)) => ArrayLoad(d1, d2)
    case (AtomicOp.Spawn, List(d1, d2)) => Spawn(d1, d2)
    case (AtomicOp.PutField(field), List(d1, d2)) => JavaPutField(field, d1, d2)
    case (AtomicOp.ArrayStore, List(d1, d2, d3)) => ArrayStore(d1, d2, d3)
    case (AtomicOp.InvokeMethod(method), d :: rs) => JavaInvokeMethod(method, d, rs)
    // fall back if non other applies
    case (op, ds) => App(Meta(op.toString), ds)
  }

}

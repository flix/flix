/*
 * Copyright 2016 Magnus Madsen
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

package ca.uwaterloo.flix.runtime.evaluator

import ca.uwaterloo.flix.language.ast.ExecutableAst.Expression
import ca.uwaterloo.flix.language.ast.{Symbol, Type}
import com.microsoft.z3.Model

/**
  * Symbolic Values.
  */
sealed trait SymVal

object SymVal {

  /**
    * An atomic symbolic variable.
    *
    * @param sym the identifier.
    * @param tpe the type of the variable.
    */
  case class AtomicVar(sym: Symbol.VarSym, tpe: Type) extends SymVal

  /**
    * The `Unit` value.
    */
  case object Unit extends SymVal

  /**
    * A common super-type for booleans.
    */
  sealed trait Bool extends SymVal

  /**
    * The `True` value.
    */
  case object True extends Bool

  /**
    * The `False` value.
    */
  case object False extends Bool

  /**
    * A Char value.
    */
  case class Char(lit: Int) extends SymVal

  /**
    * A Float32 value.
    */
  case class Float32(lit: Float) extends SymVal

  /**
    * A Float64 value.
    */
  case class Float64(lit: Double) extends SymVal

  /**
    * An Int8 value.
    *
    * @param lit the int literal.
    */
  case class Int8(lit: Byte) extends SymVal

  /**
    * An Int16 value.
    *
    * @param lit the int literal.
    */
  case class Int16(lit: Short) extends SymVal

  /**
    * An Int32 value.
    *
    * @param lit the int literal.
    */
  case class Int32(lit: Int) extends SymVal

  /**
    * An Int64 value.
    *
    * @param lit the int literal.
    */
  case class Int64(lit: Long) extends SymVal

  /**
    * A BigInt value.
    *
    * @param lit the int literal.
    */
  case class BigInt(lit: java.math.BigInteger) extends SymVal

  /**
    * A String value.
    *
    * @param lit the int literal.
    */
  case class Str(lit: String) extends SymVal

  /**
    * A tag value.
    *
    * @param tag the tag name.
    * @param elm the tagged value.
    */
  case class Tag(tag: String, elm: SymVal) extends SymVal

  /**
    * A tuple value.
    *
    * @param elms the elements of the tuple.
    */
  case class Tuple(elms: List[SymVal]) extends SymVal

  /**
    * The `FNil` value.
    */
  case object FNil extends SymVal

  /**
    * The `FList` value.
    */
  case class FList(hd: SymVal, tl: SymVal) extends SymVal

  /**
    * A closure value.
    *
    * @param exp the expression of the closure.
    * @param env the closure environment.
    */
  case class Closure(exp: Expression.Ref, env: List[SymVal]) extends SymVal

  /**
    * Returns a stringified model of the given quantifier map `qua` with free variables replaced by the optional Z3 model.
    */
  def mkModel(qua: SymbolicEvaluator.Quantifiers, model: Option[Model]): Map[Symbol.VarSym, String] = {
    def visit(v0: SymVal): String = v0 match {
      case SymVal.AtomicVar(id, tpe) => model match {
        case None => "?"
        case Some(m) => getConstant(id, m)
      }
      case SymVal.Unit => "#U"
      case SymVal.True => "true"
      case SymVal.False => "false"
      case SymVal.Char(c) => c.toString
      case SymVal.Float32(f) => f.toString
      case SymVal.Float64(f) => f.toString
      case SymVal.Int8(i) => i.toString
      case SymVal.Int16(i) => i.toString
      case SymVal.Int32(i) => i.toString
      case SymVal.Int64(i) => i.toString
      case SymVal.BigInt(i) => i.toString()
      case SymVal.Str(s) => s
      case SymVal.Tag(tag, SymVal.Unit) => tag
      case SymVal.Tag(tag, elm) => tag + "(" + visit(elm) + ")"
      case SymVal.Tuple(elms) => "(" + elms.map(visit).mkString(", ") + ")"
      case SymVal.FNil => "Nil"
      case SymVal.FList(hd, tl) => visit(hd) + " :: " + visit(tl)
      case SymVal.Closure(_, _) => "<<closure>>"
    }

    qua.m.foldLeft(Map.empty[Symbol.VarSym, String]) {
      case (macc, (sym, value)) => macc + (sym -> visit(value))
    }
  }

  /**
    * Returns a string representation of the given constant `sym` in the Z3 model `m`.
    */
  private def getConstant(sym: Symbol.VarSym, m: Model): String = {
    for (decl <- m.getConstDecls) {
      if (sym.toString == decl.getName.toString) {
        return m.getConstInterp(decl).toString
      }
    }
    "<<unknown>>"
  }
}
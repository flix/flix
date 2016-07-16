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
import ca.uwaterloo.flix.language.ast.Name
import com.microsoft.z3.Model

import scala.collection.immutable.SortedMap

/**
  * Symbolic Values.
  */
sealed trait SymVal

object SymVal {

  /**
    * An atomic symbolic variable.
    *
    * @param ident the identifier.
    */
  case class AtomicVar(ident: Name.Ident) extends SymVal

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
    * A closure value.
    *
    * @param exp the expression of the closure.
    * @param env the closure environment.
    */
  case class Closure(exp: Expression.Ref, env: List[SymVal]) extends SymVal

  /**
    * Returns a stringified model of `env` where all free variables have been
    * replaced by their corresponding values from the Z3 model `model`.
    */
  def mkModel(env: Map[String, SymVal], model: Option[Model]): Map[String, String] = {
    def visit(e0: SymVal): String = e0 match {
      case SymVal.AtomicVar(id) => model match {
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
      case SymVal.Closure(_, _) => "<<closure>>"
    }

    env.foldLeft(SortedMap.empty[String, String]) {
      case (macc, (key, value)) => macc + (key -> visit(value))
    }
  }

  /**
    * Returns a string representation of the given constant `id` in the Z3 model `m`.
    */
  private def getConstant(id: Name.Ident, m: Model): String = {
    for (decl <- m.getConstDecls) {
      if (id.name == decl.getName.toString) {
        return m.getConstInterp(decl).toString
      }
    }
    "<<unknown>>"
  }
}
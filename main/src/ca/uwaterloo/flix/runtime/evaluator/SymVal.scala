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

}
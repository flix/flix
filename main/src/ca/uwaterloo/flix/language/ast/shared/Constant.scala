/*
 * Copyright 2024 Holger Dal Mogensen
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
package ca.uwaterloo.flix.language.ast.shared

/**
  * A common supertype for constant values.
  */
sealed trait Constant

object Constant {
  case object Unit extends Constant

  case object Null extends Constant

  case class Bool(lit: scala.Boolean) extends Constant

  case class Char(lit: scala.Char) extends Constant

  case class Float32(lit: scala.Float) extends Constant

  case class Float64(lit: scala.Double) extends Constant

  case class BigDecimal(lit: java.math.BigDecimal) extends Constant

  case class Int8(lit: scala.Byte) extends Constant

  case class Int16(lit: scala.Short) extends Constant

  case class Int32(lit: scala.Int) extends Constant

  case class Int64(lit: scala.Long) extends Constant

  case class BigInt(lit: java.math.BigInteger) extends Constant

  case class Str(lit: java.lang.String) extends Constant

  case class Regex(lit: java.util.regex.Pattern) extends Constant
}

/*
 * Copyright 2019 Miguel Fialho
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
package ca.uwaterloo.flix.language.phase.njvm

import ca.uwaterloo.flix.language.phase.jvm.JvmName

/**
  * A common super-type for JVM types.
  *
  * A JVM type is either one of the primitive types or a reference type.
  */
sealed trait NJvmType {
  /**
    * Returns the type descriptor of `this` Java name.
    */
  def toDescriptor: String = this match {
    case NJvmType.Void() => "V"
    case NJvmType.PrimBool() => "Z"
    case NJvmType.PrimChar() => "C"
    case NJvmType.PrimByte() => "B"
    case NJvmType.PrimShort() => "S"
    case NJvmType.PrimInt() => "I"
    case NJvmType.PrimLong() => "J"
    case NJvmType.PrimFloat() => "F"
    case NJvmType.PrimDouble() => "D"
    case NJvmType.Reference(name) => name.toDescriptor
  }
}

object NJvmType {

  /**
    * The Flix Context class.
    */
  val Context: Reference = Reference(JvmName.Context)

  /**
    * The `ca.uwaterloo.flix.api.Unit` type
    */
  val Unit: Reference = Reference(JvmName.Runtime.Value.Unit)

  /**
    * The `java.lang.BigInteger` type.
    */
  val BigInteger: Reference = Reference(JvmName.BigInteger)

  /**
    * The `java.lang.Object` type.
    */
  val Object: Reference = Reference(JvmName.Object)

  /**
    * The `java.lang.String` type.
    */
  val JString: Reference = Reference(JvmName.String)
  /**
    * The `ca.uwaterloo.flix.runtime.interpreter.Spawnable` type.
    */
  val Spawnable: Reference = Reference(JvmName.Spawnable)

  /**
    * The `scala.math.package$` type
    */
  val ScalaMathPkg: Reference = Reference(JvmName.ScalaMathPkg)

  val ProxyObject: Reference = Reference(JvmName.ProxyObject)

  val Function: Reference = Reference(JvmName.Function)

  /**
    * Represents the void type.
    */
  case class Void() extends NJvmType

  /**
    * Represents the primitive boolean type.
    */
  case class PrimBool() extends NJvmType

  /**
    * Represents the primitive character type.
    */
  case class PrimChar() extends NJvmType

  /**
    * Represents the primitive byte type.
    */
  case class PrimByte() extends NJvmType

  /**
    * Represents the primitive short type.
    */
  case class PrimShort() extends NJvmType

  /**
    * Represents the primitive int type.
    */
  case class PrimInt() extends NJvmType

  /**
    * Represents the primitive long type.
    */
  case class PrimLong() extends NJvmType

  /**
    * Represents the primitive float type.
    */
  case class PrimFloat() extends NJvmType

  /**
    * Represents the primitive double type.
    */
  case class PrimDouble() extends NJvmType

  /**
    * Represents a reference type of the given `name`.
    */
  case class Reference(name: JvmName) extends NJvmType

}

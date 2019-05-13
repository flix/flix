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
    case NJvmType.Void => "V"
    case NJvmType.PrimBool => "Z"
    case NJvmType.PrimChar => "C"
    case NJvmType.PrimByte => "B"
    case NJvmType.PrimShort => "S"
    case NJvmType.PrimInt => "I"
    case NJvmType.PrimLong => "J"
    case NJvmType.PrimFloat => "F"
    case NJvmType.PrimDouble => "D"
    case NJvmType.Reference(name) => name.toDescriptor
    case NJvmType.JArray(jt) => "[" + jt.toDescriptor

  }
}

object NJvmType {

  // TODO: Miguel: Ultimately all of these will be removed and replaced by a generic-typed Reference.

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
  val String: Reference = Reference(JvmName.String)
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

  case object Void extends NJvmType


  /**
    * Represents the primitive boolean type.
    */

  case object PrimBool extends NJvmType


  /**
    * Represents the primitive character type.
    */

  case object PrimChar extends NJvmType


  /**
    * Represents the primitive byte type.
    */

  case object PrimByte extends NJvmType


  /**
    * Represents the primitive short type.
    */

  case object PrimShort extends NJvmType


  /**
    * Represents the primitive int type.
    */

  case object PrimInt extends NJvmType


  /**
    * Represents the primitive long type.
    */
  case object PrimLong extends NJvmType

  /**
    * Represents the primitive float type.
    */
  case object PrimFloat extends NJvmType

  /**
    * Represents the primitive double type.
    */
  case object PrimDouble extends NJvmType

  /**
    * Represents a reference type of the given `name`.
    */
  case class Reference(name: JvmName) extends NJvmType

  case class JArray(jt: NJvmType) extends NJvmType
}

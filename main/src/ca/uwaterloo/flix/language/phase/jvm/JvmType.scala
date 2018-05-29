/*
 * Copyright 2017 Magnus Madsen
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

package ca.uwaterloo.flix.language.phase.jvm

/**
  * A common super-type for JVM types.
  *
  * A JVM type is either one of the primitive types or a reference type.
  */
sealed trait JvmType {
  /**
    * Returns the type descriptor of `this` Java name.
    */
  def toDescriptor: String = this match {
    case JvmType.Void => "V"
    case JvmType.PrimBool => "Z"
    case JvmType.PrimChar => "C"
    case JvmType.PrimByte => "B"
    case JvmType.PrimShort => "S"
    case JvmType.PrimInt => "I"
    case JvmType.PrimLong => "J"
    case JvmType.PrimFloat => "F"
    case JvmType.PrimDouble => "D"
    case JvmType.Reference(name) => name.toDescriptor
  }

  /**
   * Returns the boxed java name for primitive types.
   */
  def getBoxedTypeString: String = this match {
    case JvmType.Void => "java/lang/Void"
    case JvmType.PrimBool => JvmType.Boolean.name.toInternalName
    case JvmType.PrimChar => JvmType.Character.name.toInternalName
    case JvmType.PrimByte => JvmType.Byte.name.toInternalName
    case JvmType.PrimShort => JvmType.Short.name.toInternalName
    case JvmType.PrimInt => JvmType.Integer.name.toInternalName
    case JvmType.PrimLong => JvmType.Long.name.toInternalName
    case JvmType.PrimFloat => JvmType.Float.name.toInternalName
    case JvmType.PrimDouble => JvmType.Double.name.toInternalName
    case JvmType.Object => JvmType.Object.name.toInternalName
    case JvmType.Reference(name) => name.toDescriptor
  }

  /**
   * Returns the boxed type of a primitive type.
   */
  def getBoxedType: JvmType = this match {
    case JvmType.Void => JvmType.Void
    case JvmType.PrimBool => JvmType.Boolean
    case JvmType.PrimChar => JvmType.Character
    case JvmType.PrimByte => JvmType.Byte
    case JvmType.PrimShort => JvmType.Short
    case JvmType.PrimInt => JvmType.Integer
    case JvmType.PrimLong => JvmType.Long
    case JvmType.PrimFloat => JvmType.Float
    case JvmType.PrimDouble => JvmType.Double
    //case JvmType.Object => JvmType.Object
    case JvmType.Reference(name) => JvmType.Reference(name)
  }

  /**
   * Returns the method for unboxing a boxed value.
   */
  def getUnboxingMethod: String = this match {
    case JvmType.Void => null
    case JvmType.PrimBool => "booleanValue"
    case JvmType.PrimChar => "charValue"
    case JvmType.PrimByte => "byteValue"
    case JvmType.PrimShort => "shortValue"
    case JvmType.PrimInt=> "intValue"
    case JvmType.PrimLong => "longValue"
    case JvmType.PrimFloat => "floatValue"
    case JvmType.PrimDouble => "doubleValue"
    case JvmType.Reference(name) => name.toDescriptor
  }

}

object JvmType {

  /**
    * The Flix Context class.
    */
  val Context: JvmType.Reference = Reference(JvmName.Context)

  /**
    *  The `java.util.List` type.
    */
  val JavaList: JvmType.Reference = Reference(JvmName.JavaList)

  /**
    *  The `java.util.ArrayList` type.
    */
  val ArrayList: JvmType.Reference = Reference(JvmName.ArrayList)

  /**
    *  The `java.util.LinkedList` type.
    */
  val LinkedList: JvmType.Reference = Reference(JvmName.LinkedList)

  /**
    *  The `java.util.concurrent.locks.Lock` type.
    */
  val Lock: JvmType.Reference = Reference(JvmName.Lock)

  /**
    *  The `java.util.concurrent.locks.ReentrantLock` type.
    */
  val ReentrantLock: JvmType.Reference = Reference(JvmName.ReentrantLock)

  /**
    *  The `java.util.concurrent.locks.Condition` type.
    */
  val Condition: JvmType.Reference = Reference(JvmName.Condition)

  /**
    * The `ca.uwaterloo.flix.api.Unit` type
    */
  val Unit: JvmType.Reference = Reference(JvmName.Unit)

  /**
    * The `ca.uwaterloo.flix.api.Spawn` type
    */
  val Spawn: JvmType.Reference = Reference(JvmName.Spawn)

  /**
    * The `java.lang.Runnable` type
    */
  val Runnable: JvmType.Reference = Reference(JvmName.Runnable)

  /**
    * The `java.lang.BigInteger` type.
    */
  val BigInteger: JvmType.Reference = Reference(JvmName.BigInteger)

  /**
    * The `java.lang.Boolean` type.
    */
  val Boolean: JvmType.Reference = Reference(JvmName.Boolean)

  /**
    * The `java.lang.Byte` type.
    */
  val Byte: JvmType.Reference = Reference(JvmName.Byte)

  /**
    * The `java.lang.Character` type.
    */
  val Character: JvmType.Reference = Reference(JvmName.Character)

  /**
    * The `java.lang.Short` type.
    */
  val Short: JvmType.Reference = Reference(JvmName.Short)

  /**
    * The `java.lang.Integer` type.
    */
  val Integer: JvmType.Reference = Reference(JvmName.Integer)

  /**
    * The `java.lang.Object` type.
    */
  val Object: JvmType.Reference = Reference(JvmName.Object)

  /**
    * The `java.lang.Long` type.
    */
  val Long: JvmType.Reference = Reference(JvmName.Long)

  /**
    * The `java.lang.Float` type.
    */
  val Float: JvmType.Reference = Reference(JvmName.Float)

  /**
    * The `java.lang.Double` type.
    */
  val Double: JvmType.Reference = Reference(JvmName.Double)

  /**
    * The `java.lang.String` type.
    */
  val String: JvmType.Reference = Reference(JvmName.String)

  /**
    * The `ca.uwaterloo.flix.api.Channel` type.
    */
  val Channel: JvmType.Reference = Reference(JvmName.Object)

  /**
    * The `ca.uwaterloo.flix.api.Tuple` type
    */
  val Tuple: JvmType.Reference = Reference(JvmName.Tuple)

  /**
    * The `scala.math.package$` type
    */
  val ScalaMathPkg: JvmType.Reference = Reference(JvmName.ScalaMathPkg)

  /**
    * Represents the void type.
    */
  case object Void extends JvmType

  /**
    * Represents the primitive boolean type.
    */
  case object PrimBool extends JvmType

  /**
    * Represents the primitive character type.
    */
  case object PrimChar extends JvmType

  /**
    * Represents the primitive byte type.
    */
  case object PrimByte extends JvmType

  /**
    * Represents the primitive short type.
    */
  case object PrimShort extends JvmType

  /**
    * Represents the primitive int type.
    */
  case object PrimInt extends JvmType

  /**
    * Represents the primitive long type.
    */
  case object PrimLong extends JvmType

  /**
    * Represents the primitive float type.
    */
  case object PrimFloat extends JvmType

  /**
    * Represents the primitive double type.
    */
  case object PrimDouble extends JvmType

  /**
    * Represents a reference type of the given `name`.
    */
  case class Reference(name: JvmName) extends JvmType

}

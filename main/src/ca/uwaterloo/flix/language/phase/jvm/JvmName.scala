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

import java.nio.file.{Path, Paths}

/**
  * Companion object for the [[JvmName]] class.
  */
object JvmName {

  /**
    * The Flix Context class.
    */
  val Context: JvmName = JvmName(Nil, "Context")

  /**
    *  The `java.util.List` name.
    */
  val JavaList: JvmName = JvmName(List("java", "util"), "List")

  /**
    *  The `java.util.ArrayList` name.
    */
  val ArrayList: JvmName = JvmName(List("java", "util"), "ArrayList")

  /**
    *  The `java.util.LinkedList` name.
    */
  val LinkedList: JvmName = JvmName(List("java", "util"), "LinkedList")

  /**
    *  The `java.util.concurrent.locks.Lock` name.
    */
  val Lock: JvmName = JvmName(List("java", "util", "concurrent", "locks"), "Lock")

  /**
    *  The `java.util.concurrent.locks.ReentrantLock` name.
    */
  val ReentrantLock: JvmName = JvmName(List("java", "util", "concurrent", "locks"), "ReentrantLock")

  /**
    *  The `java.util.concurrent.locks.Lock` name.
    */
  val Condition: JvmName = JvmName(List("java", "util", "concurrent", "locks"), "Condition")

  /**
    * The `ca.uwaterloo.flix.api.Unit` name
    */
  val Unit: JvmName = JvmName(List("ca", "uwaterloo", "flix"), "Unit")

  /**
    * The `java.math.BigInteger` name.
    */
  val BigInteger: JvmName = JvmName(List("java", "math"), "BigInteger")

  /**
    * The `java.lang.Boolean` name.
    */
  val Boolean: JvmName = JvmName(List("java", "lang"), "Boolean")

  /**
    * The `java.lang.Byte` name.
    */
  val Byte: JvmName = JvmName(List("java", "lang"), "Byte")

  /**
    * The `java.lang.Character` name.
    */
  val Character: JvmName = JvmName(List("java", "lang"), "Character")

  /**
    * The `java.lang.Short` name.
    */
  val Short: JvmName = JvmName(List("java", "lang"), "Short")

  /**
    * The `java.lang.Integer` name.
    */
  val Integer: JvmName = JvmName(List("java", "lang"), "Integer")

  /**
    * The `java.lang.Long` name.
    */
  val Long: JvmName = JvmName(List("java", "lang"), "Long")

  /**
    * The `java.lang.Float` name.
    */
  val Float: JvmName = JvmName(List("java", "lang"), "Float")

  /**
    * The `java.lang.Double` name.
    */
  val Double: JvmName = JvmName(List("java", "lang"), "Double")

  /**
    * The `java.lang.Object` name.
    */
  val Object: JvmName = JvmName(List("java", "lang"), "Object")

  /**
    * The `java.lang.String` name.
    */
  val String: JvmName = JvmName(List("java", "lang"), "String")

  /**
    * The `java.lang.Runnable` name.
    */
  val Runnable: JvmName = JvmName(List("java", "lang"), "Runnable")

  /**
    * The `java.lang.Thread` name.
    */
  val Thread: JvmName = JvmName(List("java", "lang"), "Thread")

  /**
    * The `ca.uwaterloo.flix.Spawn` name.
    */
  val Spawn: JvmName = JvmName(List("ca", "uwaterloo", "flix"), "Spawn")

  /**
    * The `ca.uwaterloo.flix.api.Tuple` name
    */
  // TODO: Magnus: Get rid of tuple interface.
  val Tuple: JvmName = JvmName(List("ca", "uwaterloo", "flix", "api"), "Tuple")

  /**
    * The `ca.uwaterloo.flix.api.Tag` name
    */
  // TODO: Magnus: Get rid of tag interface.
  val Tag: JvmName = JvmName(List("ca", "uwaterloo", "flix", "api"), "Tag")

  /**
    * The `scala.math.package$` name
    */
  val ScalaMathPkg: JvmName = JvmName(List("scala", "math"), "package$")

  /**
    * The `java.lang.Exception` name
    */
  val Exception: JvmName = JvmName(List("java", "lang"), "Exception")

  /**
    * The `java.lang.Exception` name
    */
  val InterruptedException: JvmName = JvmName(List("java", "lang"), "InterruptedException")

  /**
    * The `ca.uwaterloo.flix.api.UserException$` name
    */
  val UserException: JvmName = JvmName(List("ca", "uwaterloo", "flix", "api"), "UserException")

  /**
    * The `ca.uwaterloo.flix.api.MatchException$` name
    */
  val MatchException: JvmName = JvmName(List("ca", "uwaterloo", "flix", "api"), "MatchException")

  /**
    * The `ca.uwaterloo.flix.api.SwitchException$` name
    */
  val SwitchException: JvmName = JvmName(List("ca", "uwaterloo", "flix", "api"), "SwitchException")

  /**
    * The `ca.uwaterloo.flix.FlixException` name
    */
  val FlixException: JvmName = JvmName(List("ca", "uwaterloo", "flix"), "FlixException")

  /**
    * The `ca.uwaterloo.flix.RuntimeException` name
    */
  val RuntimeException: JvmName = JvmName(List("ca", "uwaterloo", "flix"), "RuntimeException")

  /**
    * The `java.lang.Exception` name
    */
  val UnsupportedOperationException: JvmName = JvmName(List("java", "lang"), "UnsupportedOperationException")

  /**
    * Get the class type for the cell with subtype `subType`
    */
  def getCellClassType(subType: JvmType): JvmType.Reference = {
    val name = "Cell" + "$" + JvmOps.stringify(subType)

    // The type resides in the ca.uwaterloo.flix package.
    JvmType.Reference(JvmName(List("ca", "uwaterloo", "flix"), name))
  }

  /**
    * Get the class type for the channel with subtype `subType`
    */
  def getChannelClassType(subType: JvmType): JvmType.Reference = {
    val name = "Channel" + "$" + JvmOps.stringify(subType)

    // The type resides in the ca.uwaterloo.flix package.
    JvmType.Reference(JvmName(List("ca", "uwaterloo", "flix"), name))
  }

}

/**
  * Represents the name of a Java class or interface.
  *
  * @param pkg  the package name.
  * @param name the class or interface name.
  */
case class JvmName(pkg: List[String], name: String) {
  /**
    * Returns the type descriptor of `this` Java name.
    */
  def toDescriptor: String =
    if (pkg.isEmpty) "L" + name + ";" else "L" + pkg.mkString("/") + "/" + name + ";"

  /**
    * Returns the binary name of `this` Java name.
    *
    * The binary name is of the form `java.lang.String`.
    *
    * The binary name is rarely used. Mostly likely you need the [[toInternalName]].
    */
  def toBinaryName: String =
    if (pkg.isEmpty) name else pkg.mkString(".") + "." + name

  /**
    * Returns the internal name of `this` Java name.
    *
    * The internal name is of the form `java/lang/String`.
    */
  def toInternalName: String =
    if (pkg.isEmpty) name else pkg.mkString("/") + "/" + name

  /**
    * Returns the relative path of `this` Java name.
    */
  def toPath: Path = Paths.get(pkg.mkString("/"), name + ".class")
}

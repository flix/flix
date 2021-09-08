/*
 * Copyright 2020-2021 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.phase.sjvm

import ca.uwaterloo.flix.language.ast.Describable
import ca.uwaterloo.flix.util.InternalCompilerException

import java.nio.file.{Path, Paths}

/**
 * Companion object for the [[JvmName]] class.
 */
object JvmName {

  // TODO(JLS): Find better alternative that is not allowed in surface syntax
  val reservedDelimiter: String = "_"

  val constructorMethod = "<init>"

  val voidDescriptor = "V"
  val javaMainDescriptor = "([Ljava/lang/String;)V"

  val nothingToVoid: String = getMethodDescriptor(Nil, None)
  val booleanToVoid: String = getMethodDescriptor(Java.Boolean, None)
  val byteToVoid: String = getMethodDescriptor(Java.Byte, None)
  val shortToVoid: String = getMethodDescriptor(Java.Short, None)
  val integerToVoid: String = getMethodDescriptor(Java.Integer, None)
  val longToVoid: String = getMethodDescriptor(Java.Long, None)
  val characterToVoid: String = getMethodDescriptor(Java.Character, None)
  val floatToVoid: String = getMethodDescriptor(Java.Float, None)
  val doubleToVoid: String = getMethodDescriptor(Java.Double, None)
  val objectToVoid: String = getMethodDescriptor(Java.Object, None)

  /**
   * Returns the JvmName of the given string `s`.
   */
  def fromClass(clazz: Class[_]): JvmName = {
    // TODO(JLS): why not asm.Type.getInternalName ?
    val l = clazz.getName.split("\\.")
    if (l.isEmpty) throw InternalCompilerException(s"class name non existent $clazz")
    else if (l.size == 1) JvmName(Nil, l.last)
    else JvmName(l.init.toList, l.last)
  }

  /**
   * Returns the descriptor of a method take takes the given `argumentTypes` and returns the given `resultType`.
   */
  def getMethodDescriptor(arguments: List[Describable], result: Option[Describable]): String = {
    // Descriptor of result
    val resultDescriptor = result.fold(voidDescriptor)(_.toDescriptor)

    // Descriptor of arguments
    val argumentDescriptor = arguments.map(_.toDescriptor).mkString

    // Descriptor of the method
    s"($argumentDescriptor)$resultDescriptor"
  }

  def getMethodDescriptor(arguments: List[Describable], result: Describable): String =
    getMethodDescriptor(arguments, Some(result))

  def getMethodDescriptor(argument: Describable, result: Option[Describable]): String =
    getMethodDescriptor(List(argument), result)

  def getMethodDescriptor(argument: Describable, result: Describable): String =
    getMethodDescriptor(List(argument), Some(result))

  object Java {

    val Boolean: JvmName = JvmName(List("java", "lang"), "Boolean")
    val Byte: JvmName = JvmName(List("java", "lang"), "Byte")
    val Short: JvmName = JvmName(List("java", "lang"), "Short")
    val Integer: JvmName = JvmName(List("java", "lang"), "Integer")
    val Long: JvmName = JvmName(List("java", "lang"), "Long")
    val Character: JvmName = JvmName(List("java", "lang"), "Character")
    val Float: JvmName = JvmName(List("java", "lang"), "Float")
    val Double: JvmName = JvmName(List("java", "lang"), "Double")
    val Object: JvmName = JvmName(List("java", "lang"), "Object")
    val Objects: JvmName = JvmName(List("java", "lang"), "Objects")
    val Runnable: JvmName = JvmName(List("java", "lang"), "Runnable")
    val String: JvmName = JvmName(List("java", "lang"), "String")
    val System: JvmName = JvmName(List("java", "lang"), "System")
    val StringBuilder: JvmName = JvmName(List("java", "lang"), "StringBuilder")
    val runtimeException: JvmName = JvmName(List("java", "lang"), "RuntimeException")

    val BigInteger: JvmName = JvmName(List("java", "math"), "BigInteger")

    val Arrays: JvmName = JvmName(List("java", "util"), "Arrays")

  }

  val main: JvmName = JvmName(Nil, "Main")

  object Flix {

    val NotImplementedError: JvmName = JvmName(List("dev", "flix", "runtime"), "NotImplementedError")
    val MatchError: JvmName = JvmName(List("dev", "flix", "runtime"), "MatchError")
    val HoleError: JvmName = JvmName(List("dev", "flix", "runtime"), "HoleError")
    val ReifiedSourceLocation: JvmName = JvmName(List("dev", "flix", "runtime"), "ReifiedSourceLocation")

    // TODO(JLS): these should be moved
    val Channel: JvmName = JvmName("ca" :: "uwaterloo" :: "flix" :: "runtime" :: "interpreter" :: Nil, "Channel")
    val SelectChoice: JvmName = JvmName("ca" :: "uwaterloo" :: "flix" :: "runtime" :: "interpreter" :: Nil, "SelectChoice")

    val Unit: JvmName = JvmName(List("dev", "flix", "runtime"), "Unit")

  }

  object Scala {

    val Package: JvmName = JvmName(List("scala", "math"), "package$")

  }

}

/**
 * Represents the name of a Java class or interface.
 *
 * @param pkg  the package name.
 * @param name the class or interface name.
 */
case class JvmName(pkg: List[String], name: String) extends Describable {

  /**
   * Returns the type descriptor of `this` Java name.
   */
  lazy val toDescriptor: String =
    if (pkg.isEmpty) "L" + name + ";" else "L" + pkg.mkString("/") + "/" + name + ";"

  /**
   * Returns the binary name of `this` Java name.
   *
   * The binary name is of the form `java.lang.String`.
   *
   * The binary name is rarely used. Mostly likely you need the [[toInternalName]].
   */
  lazy val toBinaryName: String =
    if (pkg.isEmpty) name else pkg.mkString(".") + "." + name


  /**
   * Returns the internal name of `this` Java name.
   *
   * The internal name is of the form `java/lang/String`.
   */
  lazy val toInternalName: String =
    if (pkg.isEmpty) name else pkg.mkString("/") + "/" + name

  /**
   * Returns the relative path of `this` Java name.
   */
  lazy val toPath: Path = Paths.get(pkg.mkString("/"), name + ".class")
}

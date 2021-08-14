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

  // TODO(JLS): Find better alternative
  val reservedDelimiter: String = "_"

  val constructorMethod = "<init>"

  val voidDescriptor = "V"
  val javaMainDescriptor = "([Ljava/lang/String;)V"

  val nothingToVoid: String = getMethodDescriptor(Nil, None)
  val booleanToVoid: String = getMethodDescriptor(Java.Lang.Boolean, None)
  val byteToVoid: String = getMethodDescriptor(Java.Lang.Byte, None)
  val shortToVoid: String = getMethodDescriptor(Java.Lang.Short, None)
  val integerToVoid: String = getMethodDescriptor(Java.Lang.Integer, None)
  val longToVoid: String = getMethodDescriptor(Java.Lang.Long, None)
  val characterToVoid: String = getMethodDescriptor(Java.Lang.Character, None)
  val floatToVoid: String = getMethodDescriptor(Java.Lang.Float, None)
  val doubleToVoid: String = getMethodDescriptor(Java.Lang.Double, None)
  val objectToVoid: String = getMethodDescriptor(Java.Lang.Object, None)

  /**
    * Returns the JvmName of the given string `s`.
    */
  def fromClass(clazz: Class[_]): JvmName = {
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

  //TODO(JLS): redo this tree stuff
  object Java {
    val pckage: List[String] = List("java")

    object Lang {
      val pckage: List[String] = Java.pckage :+ "lang"
      val Boolean: JvmName = JvmName(pckage, "Boolean")
      val Byte: JvmName = JvmName(pckage, "Byte")
      val Short: JvmName = JvmName(pckage, "Short")
      val Integer: JvmName = JvmName(pckage, "Integer")
      val Long: JvmName = JvmName(pckage, "Long")
      val Character: JvmName = JvmName(pckage, "Character")
      val Float: JvmName = JvmName(pckage, "Float")
      val Double: JvmName = JvmName(pckage, "Double")
      val Object: JvmName = JvmName(pckage, "Object")

      val String: JvmName = JvmName(pckage, "String")
      val System: JvmName = JvmName(pckage, "System")
    }

    object Math {
      val pckage: List[String] = Java.pckage :+ "math"
      val BigInteger: JvmName = JvmName(pckage, "BigInteger")
    }

    object Util {
      val pckage: List[String] = Java.pckage :+ "util"
      val Arrays: JvmName = JvmName(pckage, "Arrays")
    }

  }

  val main: JvmName = JvmName(Nil, "Main")

  object Flix {
    val pckage: List[String] = List("flix")

    object Runtime {
      val pckage: List[String] = Flix.pckage :+ "runtime"

      object Value {
        val pckage: List[String] = Runtime.pckage :+ "value"
        val Unit: JvmName = JvmName(pckage, "Unit")
      }

    }

  }

  object Scala {
    val pckage: List[String] = List("scala")

    object Math {
      val pckage: List[String] = Scala.pckage :+ "math"

      val Package: JvmName = JvmName(pckage, "package$")
    }

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

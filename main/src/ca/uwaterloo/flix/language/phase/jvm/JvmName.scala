/*
 * Copyright 2017 Magnus Madsen
 * Copyright 2021 Jonathan Lindegaard Starup
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

  // TODO: Would be nice to allow BackendObjType here to avoid conversions
  case class MethodDescriptor(arguments: List[BackendType], result: VoidableType) {
    /**
      * Returns the type descriptor of this method.
      */
    override lazy val toString: String = {
      // Descriptor of result
      val resultDescriptor = result.toDescriptor

      // Descriptor of arguments
      val argumentDescriptor = arguments.map(_.toDescriptor).mkString

      // Descriptor of the method
      s"($argumentDescriptor)$resultDescriptor"
    }
  }

  object MethodDescriptor {
    val NothingToVoid: MethodDescriptor = MethodDescriptor(Nil, VoidableType.Void)

    def mkDescriptor(argument: BackendType*)(result: VoidableType): MethodDescriptor = MethodDescriptor(argument.toList, result)
  }

  /**
    * The name of the static constructor method `<clinit>`.
    */
  val StaticConstructorMethod: String = "<clinit>"

  /**
    * The name of the constructor method `<init>`.
    */
  val ConstructorMethod: String = "<init>"

  /**
    * The Flix reserved delimiter for generated jvm classes.
    */
  val Delimiter: String = "$"

  /**
    * Returns the JvmName of the given string `s`.
    */
  def mk(s: String): JvmName = {
    val l = s.split("/")
    JvmName(l.init.toList, l.last)
  }

  //
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Java Names ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //

  private val javaLang = List("java", "lang")

  val AtomicLong: JvmName = JvmName(List("java", "util", "concurrent", "atomic"), "AtomicLong")
  val Boolean: JvmName = JvmName(javaLang, "Boolean")
  val Byte: JvmName = JvmName(javaLang, "Byte")
  val Character: JvmName = JvmName(javaLang, "Character")
  val Class: JvmName = JvmName(javaLang, "Class")
  val Double: JvmName = JvmName(javaLang, "Double")
  val Exception: JvmName = JvmName(javaLang, "Exception")
  val Float: JvmName = JvmName(javaLang, "Float")
  val Function: JvmName = JvmName(List("java", "util", "function"), "Function")
  val Integer: JvmName = JvmName(javaLang, "Integer")
  val Long: JvmName = JvmName(javaLang, "Long")
  val Object: JvmName = JvmName(javaLang, "Object")
  val Objects: JvmName = JvmName(javaLang, "Objects")
  val Runnable: JvmName = JvmName(javaLang, "Runnable")
  val RuntimeException: JvmName = JvmName(javaLang, "RuntimeException")
  val Short: JvmName = JvmName(javaLang, "Short")
  val StringBuilder: JvmName = JvmName(javaLang, "StringBuilder")
  val UnsupportedOperationException: JvmName = JvmName(javaLang, "UnsupportedOperationException")

  //
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Flix Names ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //

  private val devFlixRuntime = List("dev", "flix", "runtime")

  val Context: JvmName = JvmName(Nil, "Context")
  val FlixError: JvmName = JvmName(devFlixRuntime, "FlixError")
  val GlobalCounter: JvmName = JvmName(devFlixRuntime, "GlobalCounter")
  val HoleError: JvmName = JvmName(devFlixRuntime, "HoleError")
  val MatchError: JvmName = JvmName(devFlixRuntime, "MatchError")
  val ProxyObject: JvmName = JvmName(devFlixRuntime, "ProxyObject")
  val ReifiedSourceLocation: JvmName = JvmName(devFlixRuntime, "ReifiedSourceLocation")
  val SelectChoice: JvmName = JvmName(List("ca", "uwaterloo", "flix", "runtime", "interpreter"), "SelectChoice")

  //
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Scala Names ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //

  val ScalaMathPkg: JvmName = JvmName(List("scala", "math"), "package$")
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

  /**
    * Wraps this name in `backendObjType.Native`.
    */
  def toObjTpe: BackendObjType.Native = BackendObjType.Native(this)
}

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
    val toDescriptor: String = {
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
    * Returns the JvmName of the given string `s`.
    */
  def mk(s: String): JvmName = {
    val l = s.split("/")
    JvmName(l.init.toList, l.last)
  }

  val RootPackage: List[String] = Nil

  //
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Java Names ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //

  val JavaLang: List[String] = List("java", "lang")

  val AtomicLong: JvmName = JvmName(List("java", "util", "concurrent", "atomic"), "AtomicLong")
  val Boolean: JvmName = JvmName(JavaLang, "Boolean")
  val Byte: JvmName = JvmName(JavaLang, "Byte")
  val Character: JvmName = JvmName(JavaLang, "Character")
  val Class: JvmName = JvmName(JavaLang, "Class")
  val Double: JvmName = JvmName(JavaLang, "Double")
  val Error: JvmName = JvmName(JavaLang, "Error")
  val Exception: JvmName = JvmName(JavaLang, "Exception")
  val Float: JvmName = JvmName(JavaLang, "Float")
  val Function: JvmName = JvmName(List("java", "util", "function"), "Function")
  val Integer: JvmName = JvmName(JavaLang, "Integer")
  val Long: JvmName = JvmName(JavaLang, "Long")
  val Object: JvmName = JvmName(JavaLang, "Object")
  val Objects: JvmName = JvmName(JavaLang, "Objects")
  val Runnable: JvmName = JvmName(JavaLang, "Runnable")
  val Short: JvmName = JvmName(JavaLang, "Short")
  val StringBuilder: JvmName = JvmName(JavaLang, "StringBuilder")
  val System: JvmName = JvmName(JavaLang, "System")
  val UnsupportedOperationException: JvmName = JvmName(JavaLang, "UnsupportedOperationException")

  //
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Flix Names ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //

  val DevFlixRuntime: List[String] = List("dev", "flix", "runtime")

  // TODO: These could be BackendObjType objects to allow method/field objects
  val FlixError: JvmName = JvmName(DevFlixRuntime, "FlixError")
  val Global: JvmName = JvmName(DevFlixRuntime, "Global")
  val HoleError: JvmName = JvmName(DevFlixRuntime, "HoleError")
  val MatchError: JvmName = JvmName(DevFlixRuntime, "MatchError")
  val ProxyObject: JvmName = JvmName(DevFlixRuntime, "ProxyObject")
  val ReifiedSourceLocation: JvmName = JvmName(DevFlixRuntime, "ReifiedSourceLocation")
  val SelectChoice: JvmName = JvmName(List("ca", "uwaterloo", "flix", "runtime", "interpreter"), "SelectChoice")

  // Deprecated: Should not be used in new code.
  val Channel: JvmName = JvmName(List("ca", "uwaterloo", "flix", "runtime", "interpreter"), "Channel")

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

  /**
    * Wraps this name in `BackendType.Reference(BackendObjType.Native(...))`.
    */
  def toTpe: BackendType.Reference = BackendType.Reference(this.toObjTpe)
}

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

  case class MethodDescriptor(arguments: List[JvmType], result: JvmType) {
    /**
      * Returns the type descriptor of this method.
      */
    override lazy val toString: String = AsmOps.getMethodDescriptor(arguments, result)
  }

  object MethodDescriptor {
    val NothingToVoid: MethodDescriptor = MethodDescriptor(Nil, JvmType.Void)
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

  /**
    * The Flix Context class.
    */
  val Context: JvmName = JvmName(Nil, "Context")

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
    * The `java.lang.Objects` name.
    */
  val Objects: JvmName = JvmName(List("java", "lang"), "Objects")

  /**
    * The `java.lang.Runnable` name.
    */
  val Runnable: JvmName = JvmName(List("java", "lang"), "Runnable")

  /**
    * The `java.lang.StringBuilder` name.
    */
  val StringBuilder: JvmName = JvmName(List("java", "lang"), "StringBuilder")

  /**
    * The `java.lang.String` name.
    */
  val String: JvmName = JvmName(List("java", "lang"), "String")

  /**
    * The `ca.uwaterloo.flix.runtime.interpreter.Channel` name.
    */
  val Channel: JvmName = JvmName(List("ca", "uwaterloo", "flix", "runtime", "interpreter"), "Channel")

  /**
    * The `ca.uwaterloo.flix.runtime.interpreter.SelectChoice` name.
    */
  val SelectChoice: JvmName = JvmName(List("ca", "uwaterloo", "flix", "runtime", "interpreter"), "SelectChoice")

  /**
    * The `scala.math.package$` name.
    */
  val ScalaMathPkg: JvmName = JvmName(List("scala", "math"), "package$")


  //TODO SJ: place this class a better place
  /**
    * The `java.lang.Exception` name.
    */
  val Exception: JvmName = JvmName(List("java", "lang"), "Exception")

  /**
    * The `java.lang.RuntimeException` name.
    */
  val RuntimeException: JvmName = JvmName(List("java", "lang"), "RuntimeException")

  /**
    * The Flix Unit class.
    */
  val Unit: JvmName = JvmName(List("dev", "flix", "runtime"), "Unit")

  /**
    * The `dev.flix.runtime.FlixError` name.
    */
  val FlixError: JvmName = JvmName(List("dev", "flix", "runtime"), "FlixError")

  /**
    * The `dev.flix.runtime.HoleError` name.
    */
  val HoleError: JvmName = JvmName(List("dev", "flix", "runtime"), "HoleError")

  /**
    * The `dev.flix.runtime.MatchError` name.
    */
  val MatchError: JvmName = JvmName(List("dev", "flix", "runtime"), "MatchError")

  /**
    * The `dev.flix.runtime.ReifiedSourceLocation` name.
    */
  val ReifiedSourceLocation: JvmName = JvmName(List("dev", "flix", "runtime"), "ReifiedSourceLocation")

  /**
    * The `dev.flix.runtime.GlobalCounter` name.
    */
  val GlobalCounter: JvmName = JvmName(List("dev", "flix", "runtime"), "GlobalCounter")

  /**
    * The `java.util.concurrent.atomic.AtomicLong` name.
    */
  val AtomicLong: JvmName = JvmName(List("java", "util", "concurrent", "atomic"), "AtomicLong")

  /**
    * The `java.lang.Exception` name.
    */
  val UnsupportedOperationException: JvmName = JvmName(List("java", "lang"), "UnsupportedOperationException")

  val Function: JvmName = mk("java/util/function/Function")
  
  val ProxyObject: JvmName = JvmName(List("dev", "flix", "runtime"), "ProxyObject")

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
}

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

import ca.uwaterloo.flix.api.Flix
import org.objectweb.asm

import java.nio.file.{Path, Paths}

/** Companion object for the [[JvmName]] class. */
object JvmName {

  // TODO: Would be nice to allow BackendObjType here to avoid conversions
  case class MethodDescriptor(arguments: List[BackendType], result: VoidableType) {
    /** Returns the type descriptor of this method. */
    val toDescriptor: String = {
      // Descriptor of result
      val resultDescriptor = result.toDescriptor

      // Descriptor of arguments
      val argumentDescriptor = arguments.map(_.toDescriptor).mkString

      // Descriptor of the method
      s"($argumentDescriptor)$resultDescriptor"
    }

    def toAsmType: asm.Type = asm.Type.getType(toDescriptor)
  }

  object MethodDescriptor {
    val NothingToVoid: MethodDescriptor = MethodDescriptor(Nil, VoidableType.Void)

    def mkDescriptor(argument: BackendType*)(result: VoidableType): MethodDescriptor =
      MethodDescriptor(argument.toList, result)
  }

  /** The name of the static constructor method `<clinit>`. */
  val StaticConstructorMethod: String = "<clinit>"

  /** The name of the constructor method `<init>`. */
  val ConstructorMethod: String = "<init>"

  /** Returns the JvmName of the given string `s`. */
  def mk(s: String): JvmName = {
    val l = s.split("/")
    JvmName(l.init.toList, l.last)
  }

  def ofClass(clazz: Class[_]): JvmName = {
    // TODO: Ugly hack.
    // Maybe use clazz.getPackage and clazz.getSimpleName
    val fqn = clazz.getName.replace('.', '/')
    JvmName.mk(fqn)
  }

  val RootPackage: List[String] = Nil

  /**
    * Constructs a concatenated string using `JvmName.Delimiter`. The call
    * `mkClassName("Tuple2", List(Object, Int, String))` would
    * result in the string `"Tuple2$Obj$Int32$Obj"`.
    */
  def mkClassName(prefix: String, args: List[String]): String = {
    // TODO: Should delimiter always be included?
    val cPrefix = mangle(prefix)
    if (args.isEmpty) s"$cPrefix${Flix.Delimiter}"
    else s"$cPrefix${Flix.Delimiter}${args.map(mangle).mkString(Flix.Delimiter)}"
  }

  def mkClassName(prefix: String, arg: String): String =
    mkClassName(prefix, List(arg))

  def mkClassName(prefix: String): String =
    mkClassName(prefix, Nil)

  /** Performs name mangling on the given string `s` to avoid issues with special characters. */
  // TODO: Magnus: Use this in appropriate places.
  def mangle(s: String): String = s.
    replace("+", Flix.Delimiter + "plus").
    replace("-", Flix.Delimiter + "minus").
    replace("*", Flix.Delimiter + "asterisk").
    replace("/", Flix.Delimiter + "fslash").
    replace("\\", Flix.Delimiter + "bslash").
    replace("<", Flix.Delimiter + "less").
    replace(">", Flix.Delimiter + "greater").
    replace("=", Flix.Delimiter + "eq").
    replace("&", Flix.Delimiter + "ampersand").
    replace("|", Flix.Delimiter + "bar").
    replace("^", Flix.Delimiter + "caret").
    replace("~", Flix.Delimiter + "tilde").
    replace("!", Flix.Delimiter + "exclamation").
    replace("#", Flix.Delimiter + "hashtag").
    replace(":", Flix.Delimiter + "colon").
    replace("?", Flix.Delimiter + "question").
    replace("@", Flix.Delimiter + "at")

  //
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Java Names ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //

  val JavaLang: List[String] = List("java", "lang")
  val JavaLangInvoke: List[String] = List("java", "lang", "invoke")
  val JavaUtil: List[String] = List("java", "util")
  val JavaUtilFunction: List[String] = JavaUtil ::: List("function")
  val JavaUtilConcurrent: List[String] = JavaUtil ::: List("concurrent")
  val JavaUtilConcurrentLocks: List[String] = JavaUtilConcurrent ::: List("locks")
  val JavaUtilRegex: List[String] = JavaUtil ::: List("regex")

  val AtomicLong: JvmName = JvmName(JavaUtil ::: List("concurrent", "atomic"), "AtomicLong")
  val Boolean: JvmName = JvmName(JavaLang, "Boolean")
  val Byte: JvmName = JvmName(JavaLang, "Byte")
  val Character: JvmName = JvmName(JavaLang, "Character")
  val Class: JvmName = JvmName(JavaLang, "Class")
  val Double: JvmName = JvmName(JavaLang, "Double")
  val DoubleConsumer: JvmName = JvmName(JavaUtilFunction, "DoubleConsumer")
  val DoubleFunction: JvmName = JvmName(JavaUtilFunction, "DoubleFunction")
  val DoublePredicate: JvmName = JvmName(JavaUtilFunction, "DoublePredicate")
  val DoubleUnaryOperator: JvmName = JvmName(JavaUtilFunction, "DoubleUnaryOperator")
  val Error: JvmName = JvmName(JavaLang, "Error")
  val Exception: JvmName = JvmName(JavaLang, "Exception")
  val Float: JvmName = JvmName(JavaLang, "Float")
  val Integer: JvmName = JvmName(JavaLang, "Integer")
  val IntConsumer: JvmName = JvmName(JavaUtilFunction, "IntConsumer")
  val IntFunction: JvmName = JvmName(JavaUtilFunction, "IntFunction")
  val IntPredicate: JvmName = JvmName(JavaUtilFunction, "IntPredicate")
  val IntUnaryOperator: JvmName = JvmName(JavaUtilFunction, "IntUnaryOperator")
  val Long: JvmName = JvmName(JavaLang, "Long")
  val LongConsumer: JvmName = JvmName(JavaUtilFunction, "LongConsumer")
  val LongFunction: JvmName = JvmName(JavaUtilFunction, "LongFunction")
  val LongPredicate: JvmName = JvmName(JavaUtilFunction, "LongPredicate")
  val LongUnaryOperator: JvmName = JvmName(JavaUtilFunction, "LongUnaryOperator")
  val Math: JvmName = JvmName(JavaLang, "Math")
  val ObjFunction: JvmName = JvmName(JavaUtilFunction, "Function")
  val ObjConsumer: JvmName = JvmName(JavaUtilFunction, "Consumer")
  val ObjPredicate: JvmName = JvmName(JavaUtilFunction, "Predicate")
  val ReentrantLock: JvmName = JvmName(JavaUtilConcurrentLocks, "ReentrantLock")
  val Regex: JvmName = JvmName(JavaUtilRegex, "Pattern")
  val Runnable: JvmName = JvmName(JavaLang, "Runnable")
  val Short: JvmName = JvmName(JavaLang, "Short")
  val System: JvmName = JvmName(JavaLang, "System")
  val Thread: JvmName = JvmName(JavaLang, "Thread")
  val Throwable: JvmName = JvmName(JavaLang, "Throwable")
  val UnsupportedOperationException: JvmName = JvmName(JavaLang, "UnsupportedOperationException")

  //
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Flix Names ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //

  val DevFlixRuntime: List[String] = List("dev", "flix", "runtime")
  val Main: JvmName = JvmName(RootPackage, "Main")

}

/**
  * Represents the name of a Java class or interface.
  *
  * @param pkg  the package name.
  * @param name the class or interface name.
  */
case class JvmName(pkg: List[String], name: String) {
  /** Returns the type descriptor of `this` Java name. */
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

  /** Returns the relative path of `this` Java name. */
  lazy val toPath: Path = Paths.get(pkg.mkString("/"), name + ".class")

  /** Wraps this name in `backendObjType.Native`. */
  def toObjTpe: BackendObjType.Native = BackendObjType.Native(this)

  /** Wraps this name in `BackendType.Reference(BackendObjType.Native(...))`. */
  def toTpe: BackendType.Reference = this.toObjTpe.toTpe
}

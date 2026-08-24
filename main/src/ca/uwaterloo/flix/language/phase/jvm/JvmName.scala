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

import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.util.InternalCompilerException

import java.lang.constant.ClassDesc
import java.nio.file.{Path, Paths}

/**
  * Companion object for the [[JvmName]] class.
  */
object JvmName {

  /**
    * The name of the static constructor method `<clinit>`.
    */
  val StaticConstructorMethod: String = "<clinit>"

  /**
    * The name of the constructor method `<init>`.
    */
  val ConstructorMethod: String = "<init>"

  /**
    * The name of the static method for invoking the function
    * if it is control pure.
    */
  val StaticApply: String = "staticApply"

  /** Returns the [[JvmName]] of `clazz`. Crashes if `clazz` is primitive, an array, or unnamed. */
  /** Returns the JvmName of the given class or interface descriptor `desc`. */
  def ofClassDesc(desc: ClassDesc): JvmName = {
    if (desc.isPrimitive) throw InternalCompilerException(s"Cannot create a JvmName from the primitive type '${desc.displayName()}'", SourceLocation.Unknown)
    if (desc.isArray) throw InternalCompilerException(s"Cannot create a JvmName from the array type '${desc.displayName()}'", SourceLocation.Unknown)

    // Strip the leading `L` and trailing `;` of the descriptor to obtain the internal name.
    val descriptor = desc.descriptorString()
    val parts = descriptor.substring(1, descriptor.length - 1).split("/")
    JvmName(parts.init.toList, parts.last)
  }

  //
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Java Names ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //

  val JavaIO: List[String] = List("java", "io")
  val JavaLang: List[String] = List("java", "lang")
  val JavaLangInvoke: List[String] = List("java", "lang", "invoke")
  val JavaMath: List[String] = List("java", "math")
  val JavaUtil: List[String] = List("java", "util")
  val JavaUtilConcurrent: List[String] = JavaUtil ::: List("concurrent")
  val JavaUtilConcurrentLocks: List[String] = JavaUtilConcurrent ::: List("locks")
  val JavaUtilFunction: List[String] = JavaUtil ::: List("function")
  val JavaUtilRegex: List[String] = JavaUtil ::: List("regex")

  val Arrays: JvmName = JvmName(JavaUtil, "Arrays")
  val AtomicLong: JvmName = JvmName(JavaUtil ::: List("concurrent", "atomic"), "AtomicLong")
  val BigDecimal: JvmName = JvmName(JavaMath, "BigDecimal")
  val BigInteger: JvmName = JvmName(JavaMath, "BigInteger")
  val Boolean: JvmName = JvmName(JavaLang, "Boolean")
  val Byte: JvmName = JvmName(JavaLang, "Byte")
  val CallSite: JvmName = JvmName(JavaLangInvoke, "CallSite")
  val Character: JvmName = JvmName(JavaLang, "Character")
  val Class: JvmName = JvmName(JavaLang, "Class")
  val ConcurrentLinkedQueue: JvmName = JvmName(JavaUtilConcurrent, "ConcurrentLinkedQueue")
  val Double: JvmName = JvmName(JavaLang, "Double")
  val DoubleConsumer: JvmName = JvmName(JavaUtilFunction, "DoubleConsumer")
  val DoubleFunction: JvmName = JvmName(JavaUtilFunction, "DoubleFunction")
  val DoublePredicate: JvmName = JvmName(JavaUtilFunction, "DoublePredicate")
  val DoubleUnaryOperator: JvmName = JvmName(JavaUtilFunction, "DoubleUnaryOperator")
  val Error: JvmName = JvmName(JavaLang, "Error")
  val Exception: JvmName = JvmName(JavaLang, "Exception")
  val Float: JvmName = JvmName(JavaLang, "Float")
  val IntConsumer: JvmName = JvmName(JavaUtilFunction, "IntConsumer")
  val IntFunction: JvmName = JvmName(JavaUtilFunction, "IntFunction")
  val IntPredicate: JvmName = JvmName(JavaUtilFunction, "IntPredicate")
  val IntUnaryOperator: JvmName = JvmName(JavaUtilFunction, "IntUnaryOperator")
  val Integer: JvmName = JvmName(JavaLang, "Integer")
  val Iterator: JvmName = JvmName(JavaUtil, "Iterator")
  val LambdaMetafactory: JvmName = JvmName(JavaLangInvoke, "LambdaMetafactory")
  val LinkedList: JvmName = JvmName(JavaUtil, "LinkedList")
  val Long: JvmName = JvmName(JavaLang, "Long")
  val LongConsumer: JvmName = JvmName(JavaUtilFunction, "LongConsumer")
  val LongFunction: JvmName = JvmName(JavaUtilFunction, "LongFunction")
  val LongPredicate: JvmName = JvmName(JavaUtilFunction, "LongPredicate")
  val LongUnaryOperator: JvmName = JvmName(JavaUtilFunction, "LongUnaryOperator")
  val Math: JvmName = JvmName(JavaLang, "Math")
  val MethodHandle: JvmName = JvmName(JavaLangInvoke, "MethodHandle")
  val MethodHandles$Lookup: JvmName = JvmName(JavaLangInvoke, "MethodHandles$Lookup")
  val MethodType: JvmName = JvmName(JavaLangInvoke, "MethodType")
  val ObjConsumer: JvmName = JvmName(JavaUtilFunction, "Consumer")
  val ObjFunction: JvmName = JvmName(JavaUtilFunction, "Function")
  val ObjPredicate: JvmName = JvmName(JavaUtilFunction, "Predicate")
  val Object: JvmName = JvmName(JavaLang, "Object")
  val PrintStream: JvmName = JvmName(JavaIO, "PrintStream")
  val ReentrantLock: JvmName = JvmName(JavaUtilConcurrentLocks, "ReentrantLock")
  val Regex: JvmName = JvmName(JavaUtilRegex, "Pattern")
  val Runnable: JvmName = JvmName(JavaLang, "Runnable")
  val Short: JvmName = JvmName(JavaLang, "Short")
  val String: JvmName = JvmName(JavaLang, "String")
  val StringBuilder: JvmName = JvmName(JavaLang, "StringBuilder")
  val System: JvmName = JvmName(JavaLang, "System")
  val Thread$Builder$OfVirtual: JvmName = JvmName(JavaLang, "Thread$Builder$OfVirtual")
  val Thread$UncaughtExceptionHandler: JvmName = JvmName(JavaLang, "Thread$UncaughtExceptionHandler")
  val Thread: JvmName = JvmName(JavaLang, "Thread")
  val Throwable: JvmName = JvmName(JavaLang, "Throwable")
  val UnsupportedOperationException: JvmName = JvmName(JavaLang, "UnsupportedOperationException")

  //
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Flix Names ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //

  val FlixError: JvmName = JvmName(Mangle.DevFlixRuntime, Mangle.mkClassName("FlixError"))

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
    * Returns the class file name of `this` Java name.
    *
    * The class file name is of the form `java/lang/String.class`.
    */
  lazy val toClassFileName: String = toInternalName + ".class"

  /**
    * Returns `this` Java name as a [[ClassDesc]].
    *
    * Temporary bridge while the backend migrates from [[JvmName]] to [[ClassDesc]].
    */
  lazy val toClassDesc: ClassDesc = ClassDesc.ofDescriptor(toDescriptor)

  /**
    * Wraps this name in `BackendType.Reference(BackendObjType.Native(...))`.
    */
  def toTpe: BackendType.Reference = BackendObjType.Native(this).toTpe
}

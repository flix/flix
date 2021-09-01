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
   * The Flix Unit class.
   */
  val Unit: JvmName = JvmName(Nil, "Unit")

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

  //TODO SJ: place this class a better place
  /**
    * The `ca.uwaterloo.flix.runtime.interpreter.Channel` name.
    */
  val Channel: JvmName = JvmName(List("ca", "uwaterloo", "flix", "runtime", "interpreter"), "Channel")

  /**
    * The `ca.uwaterloo.flix.runtime.interpreter.SelectChoice` name.
    */
  val SelectChoice: JvmName = JvmName(List("ca", "uwaterloo", "flix", "runtime", "interpreter"), "SelectChoice")

  /**
    * The `ca.uwaterloo.flix.runtime.interpreter.Spawnable` name.
    */
  val Spawnable: JvmName = JvmName(List("ca", "uwaterloo", "flix", "runtime", "interpreter"), "Spawnable")

  /**
    * The `scala.math.package$` name
    */
  val ScalaMathPkg: JvmName = JvmName(List("scala", "math"), "package$")

  /**
    * The `java.lang.Exception` name
    */
  val Exception: JvmName = JvmName(List("java", "lang"), "Exception")

  /**
   * The `java.lang.RuntimeException` name
   */
  val RuntimeException: JvmName = JvmName(List("java", "lang"), "RuntimeException")

  /**
   * The FlixError name
   */
  val FlixError: JvmName = JvmName(List("dev", "flix", "runtime"), "FlixError")

  /**
   * The Flix ReifiedSourceLocation class.
   */
  val ReifiedSourceLocation: JvmName = JvmName(List("dev", "flix", "runtime"), "ReifiedSourceLocation")

  /**
    * The `java.lang.Exception` name
    */
  val UnsupportedOperationException: JvmName = JvmName(List("java", "lang"), "UnsupportedOperationException")

  val PredSym: JvmName = mk("ca/uwaterloo/flix/runtime/solver/api/symbol/PredSym")

  val ProxyObject: JvmName = mk("ca/uwaterloo/flix/runtime/solver/api/ProxyObject")

  val Function: JvmName = mk("java/util/function/Function")

  /**
    * Get the class type for the cell with subtype `subType`
    */
  def getCellClassType(subType: JvmType): JvmType.Reference = {
    val name = "Ref" + "$" + JvmOps.stringify(subType)

    // The type resides in the ca.uwaterloo.flix package.
    JvmType.Reference(JvmName(Nil, name))
  }

  object Runtime {

    val ProxyObject: JvmName = mk("flix/runtime/ProxyObject")

    object Fixpoint {

      val Constraint: JvmName = mk("flix/runtime/fixpoint/Constraint")
      val ConstraintSystem: JvmName = mk("flix/runtime/fixpoint/ConstraintSystem")
      val ConstantFunction: JvmName = mk("flix/runtime/fixpoint/ConstantFunction")
      val Options: JvmName = mk("flix/runtime/fixpoint/Options")
      val LatticeOps: JvmName = mk("flix/runtime/fixpoint/LatticeOps")
      val Solver: JvmName = mk("flix/runtime/fixpoint/Solver")
      val Stratification: JvmName = mk("flix/runtime/fixpoint/Stratification")

      object Predicate {
        val Predicate: JvmName = mk("flix/runtime/fixpoint/predicate/Predicate")

        val AtomPredicate: JvmName = mk("flix/runtime/fixpoint/predicate/AtomPredicate")
        val GuardPredicate: JvmName = mk("flix/runtime/fixpoint/predicate/GuardPredicate")
        val UnionPredicate: JvmName = mk("flix/runtime/fixpoint/predicate/UnionPredicate")
      }

      object Symbol {
        val PredSym: JvmName = mk("flix/runtime/fixpoint/symbol/PredSym")
        val LatSym: JvmName = mk("flix/runtime/fixpoint/symbol/LatSym")
        val RelSym: JvmName = mk("flix/runtime/fixpoint/symbol/RelSym")

        val VarSym: JvmName = mk("flix/runtime/fixpoint/symbol/VarSym")
      }

      object Term {
        val Term: JvmName = mk("flix/runtime/fixpoint/term/Term")

        val AppTerm: JvmName = mk("flix/runtime/fixpoint/term/AppTerm")
        val LitTerm: JvmName = mk("flix/runtime/fixpoint/term/LitTerm")
        val VarTerm: JvmName = mk("flix/runtime/fixpoint/term/VarTerm")
        val WildTerm: JvmName = mk("flix/runtime/fixpoint/term/WildTerm")
      }

    }

    val HoleError: JvmName = JvmName(List("flix", "runtime"), "HoleError")

    val MatchError: JvmName = JvmName(List("flix", "runtime"), "MatchError")

    val NotImplementedError: JvmName = JvmName(List("flix", "runtime"), "NotImplementedError")
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

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

package ca.uwaterloo.flix.language.phase

import java.nio.file.Path

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.ExecutableAst._
import ca.uwaterloo.flix.language.ast.{Symbol, Type}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

object JvmBackend extends Phase[Root, Root] {

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
    def toDescriptor: String = ???

    /**
      * Returns the relative path of `this` Java name.
      */
    def toPath: Path = ???
  }

  /**
    * A common super-type for JVM types.
    *
    * A JVM type is either one of the primitive types or a reference type.
    */
  sealed trait JvmType {
    /**
      * Returns the type descriptor of `this` Java name.
      */
    def toDescriptor: String = ???
  }

  object JvmType {

    /**
      * Represents the primitive boolean type.
      */
    case object PrimBool extends JvmType

    /**
      * Represents a reference type of the given `name`.
      */
    case class Reference(name: JvmName) extends JvmType

  }

  /**
    * Represents a Java class (or interface)
    *
    * @param name     the name of the class (or interface).
    * @param bytecode the bytecode of the class (or interface).
    */
  case class JvmClass(name: JvmName, bytecode: Array[Byte])

  /**
    * A derived class used to hold information about the tag of an enum.
    */
  case class TagInfo(enum: Symbol.EnumSym, tag: String, tpe: Type, tparams: List[Type])

  /**
    * Emits JVM bytecode for the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): Validation[Root, CompilationError] = {
    //
    // Compute the set of instantiated types in the program.
    //
    val types = typesOf(root)

    //
    // Emit the Global class. TODO: Need better name.
    //
    // ...

    //
    // Emit the namespace classes.
    //
    // ...


    //
    // Emit functional interfaces for each function type in the program.
    //
    val functionalInterfaces = genFunctionalInterfaces(types)

    //
    // Emit functional classes for each function in the program.
    //
    val functionalClasses = genFunctionalClasses(root.defs)

    //
    // Emit tagged tuple classes for each tag tuple type in the program.
    //
    val taggedTupleClasses = genTaggedTupleClasses(types)

    root.toSuccess
  }

  /**
    * Returns the set of all instantiated types in the given AST `root`.
    *
    * This include type components. For example, if the program contains
    * the type (Bool, (Char, Int)) this includes the type (Char, Int).
    */
  private def typesOf(root: Root): Set[Type] = ???

  /**
    * Returns all the type components of the given type `tpe`.
    *
    * For example, if the given type is `Option[(Bool, Char, Int)]`
    * this returns the set `Bool`, `Char`, `Int`, `(Bool, Char, Int)`, and `Option[(Bool, Char, Int)]`.
    */
  private def typesOf(tpe: Type): Set[Type] = ???

  /**
    * Returns the given Flix type `tpe` as JVM type.
    */
  private def getJvmType(tpe: Type, root: Root): JvmType = ???

  /**
    * Returns the type constructor of a given type `tpe`.
    */
  private def getTypeConstructor(tpe: Type): Type = ???

  /**
    * Returns the type arguments of a given type `tpe`.
    */
  private def getTypeArguments(tpe: Type): List[Type] = tpe match {
    case Type.Apply(Type.Enum(sym, kind), arguments) => arguments
    case _ => ??? // TODO: Rest
  }

  /**
    * Returns the JVM type of the given enum symbol `sym` with `tag` and inner type `tpe`.
    *
    * For example, if the symbol is `Option`, the tag `Some` and the inner type is `Int` then the result is None$Int.
    */
  private def getJvmTypeFromEnumAndTag(sym: Symbol.EnumSym, tag: String, tpe: Type): JvmType = ???

  /**
    * Returns the information about the tags of the given type `tpe`.
    */
  private def getTagsOf(tpe: Type): Set[TagInfo] = ???

  /**
    * Returns the JVM type of the given tag info `i`.
    */
  private def getJvmType(i: TagInfo, root: Root): JvmType = ???

  /**
    * Returns the set of functional interfaces for the given set of types `ts`.
    */
  private def genFunctionalInterfaces(ts: Set[Type]): Map[JvmName, JvmClass] = ???

  /**
    * Optionally returns the functional interface of the given type `tpe`.
    *
    * Returns `[[None]]` if the type is not a function type.
    */
  private def genFunctionalInterface(tpe: Type): Option[JvmClass] = {
    // Compute the type constructor and type arguments.
    val base = getTypeConstructor(tpe)
    val args = getTypeArguments(tpe)

    // Check if the type constructor is a function type.
    if (base.isArrow) {
      // The function arity is simply the number of type arguments.
      val arity = args.length

      // The name of the functional interface is of the form:
      // Fn1$Int$Bool,
      // Fn2$Int$Int$Bool,
      // Fn3$Obj$Obj$Obj$Bool, etc.
      // TODO: Probably use a helper function to compute this?
      val name = "Fn" + arity + "$"

      for (arg <- args) {
        // do something with the arguments ...

      }

    }

    // The tpe is a non-function type.
    None
  }

  /**
    * Returns the set of functional classes for the given set of definitions `defs`.
    */
  def genFunctionalClasses(defs: Map[Symbol.DefnSym, Def]): Map[JvmName, JvmClass] = ???

  /**
    * Returns the functional class for the given definition.
    */
  def genFunctionalClass(defn: Def): JvmClass = ???

  /**
    * Returns the set of tagged tuple classes for the given set of types `ts`.
    */
  def genTaggedTupleClasses(ts: Set[Type]): Map[JvmName, JvmClass] = ???


}

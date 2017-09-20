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

import ca.uwaterloo.flix.language.ast.ExecutableAst.Root
import ca.uwaterloo.flix.language.ast.{Symbol, Type}

object JvmOps {

  /**
    * Returns the set of all instantiated types in the given AST `root`.
    *
    * This include type components. For example, if the program contains
    * the type (Bool, (Char, Int)) this includes the type (Char, Int).
    */
  def typesOf(root: Root): Set[Type] = Set.empty // TODO

  /**
    * Returns all the type components of the given type `tpe`.
    *
    * For example, if the given type is `Option[(Bool, Char, Int)]`
    * this returns the set `Bool`, `Char`, `Int`, `(Bool, Char, Int)`, and `Option[(Bool, Char, Int)]`.
    */
  def typesOf(tpe: Type): Set[Type] = ???

  /**
    * Returns the given Flix type `tpe` as JVM type.
    *
    * For example, if the type is:
    *
    * Bool                  =>      Boolean
    * Char                  =>      Char
    * Option[Int]           =>      Option$Int
    * Result[Bool, Int]     =>      Result$Bool$Int
    * Int -> Bool           =>      Fn1$Int$Bool
    * (Int, Int) -> Bool    =>      Fn2$Int$Int$Bool
    */
  def getJvmType(tpe: Type, root: Root): JvmType = ???

  /**
    * Returns the type constructor of a given type `tpe`.
    *
    * For example, if the type is:
    *
    * Celsius                   =>      Celsius
    * Option[Int]               =>      Option
    * Result[Bool, Int]         =>      Result
    * Result[Bool][Int]         =>      Result
    * Arrow[Bool, Char, Int]    =>      Arrow
    * Arrow[Bool, Char][Int]    =>      Arrow
    * Option[Result[Bool, Int]] =>      Option
    */
  def getTypeConstructor(tpe: Type): Type = ???

  /**
    * Returns the type arguments of a given type `tpe`.
    */
  def getTypeArguments(tpe: Type): List[Type] = tpe match {
    case Type.Apply(Type.Enum(sym, kind), arguments) => arguments
    case _ => ??? // TODO: Rest
  }

  /**
    * Returns the JVM type of the given enum symbol `sym` with `tag` and inner type `tpe`.
    *
    * For example, if the symbol is `Option`, the tag `Some` and the inner type is `Int` then the result is None$Int.
    */
  def getJvmTypeFromEnumAndTag(sym: Symbol.EnumSym, tag: String, tpe: Type): JvmType = ???

  /**
    * Returns the information about the tags of the given type `tpe`.
    */
  def getTagsOf(tpe: Type): Set[TagInfo] = ???

  /**
    * Returns the JVM type of the given tag info `i`.
    */
  def getJvmType(i: TagInfo, root: Root): JvmType = ???

}

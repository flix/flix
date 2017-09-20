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

import java.nio.file.{Files, Path}

import ca.uwaterloo.flix.language.ast.ExecutableAst.Root
import ca.uwaterloo.flix.language.ast.{Symbol, Type}
import ca.uwaterloo.flix.util.InternalCompilerException

object JvmOps {

  /**
    * Returns the set of all instantiated types in the given AST `root`.
    *
    * This include type components. For example, if the program contains
    * the type (Bool, (Char, Int)) this includes the type (Char, Int).
    */
  def typesOf(root: Root): Set[Type] = {
    // TODO: Temporary implementation which just returns some types to get us started.

    root.defs.map(_._2.tpe).toSet
  }

  /**
    * Returns all the type components of the given type `tpe`.
    *
    * For example, if the given type is `Option[(Bool, Char, Int)]`
    * this returns the set `Bool`, `Char`, `Int`, `(Bool, Char, Int)`, and `Option[(Bool, Char, Int)]`.
    */
  def typesOf(tpe: Type): Set[Type] = ??? // TODO

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
  def getJvmType(tpe: Type, root: Root): JvmType = ??? // TODO

  /**
    * Returns the type constructor of a given type `tpe`.
    *
    * For example,
    *
    * Celsius                       =>      Celsius
    * Option[Int]                   =>      Option
    * Arrow[Bool, Char]             =>      Arrow
    * Tuple[Bool, Int]              =>      Tuple
    * Result[Bool, Int]             =>      Result
    * Result[Bool][Int]             =>      Result
    * Option[Result[Bool, Int]]     =>      Option
    */
  def getTypeConstructor(tpe: Type): Type = tpe match {
    case Type.Apply(base, _) => getTypeConstructor(base)
    case _ => tpe
  }

  /**
    * Returns the type arguments of a given type `tpe`.
    *
    * For example,
    *
    * Celsius                       =>      Nil
    * Option[Int]                   =>      Int :: Nil
    * Arrow[Bool, Char]             =>      Bool :: Char :: Nil
    * Tuple[Bool, Int]              =>      Bool :: Int :: Nil
    * Result[Bool, Int]             =>      Bool :: Int :: Nil
    * Result[Bool][Int]             =>      Bool :: Int :: Nil
    * Option[Result[Bool, Int]]     =>      Result[Bool, Int] :: Nil
    */
  def getTypeArguments(tpe: Type): List[Type] = tpe match {
    case Type.Apply(base, arguments) => arguments ::: getTypeArguments(base)
    case _ => Nil
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

  /**
    * Writes the given JVM class `clazz` to a sub path under the given `prefixPath`.
    *
    * For example, if the prefix path is `/tmp/` and the class name is Foo.Bar.Baz
    * then the bytecode is written to the path `/tmp/Foo/Bar/Baz.class` provided
    * that this path either does not exist or is already a JVM class file.
    */
  def emitClass(prefixPath: Path, clazz: JvmClass): Unit = {
    // Compute the absolute path of the class file to write.
    val path = prefixPath.resolve(clazz.name.toPath).toAbsolutePath

    // Create all parent directories (in case they don't exist).
    Files.createDirectories(path.getParent)

    // Check if the file already exists.
    if (Files.exists(path)) {
      if (!isClassFile(path)) {
        throw InternalCompilerException(s"Refusing to overwrite non-class file: '$path'.")
      }
    }

    // Write the bytecode.
    Files.write(path, clazz.bytecode)
  }

  /**
    * Returns `true` if the given `path` exists and is a Java Virtual Machine class file.
    */
  def isClassFile(path: Path): Boolean = {
    if (Files.exists(path) && Files.isReadable(path) && Files.isRegularFile(path)) {
      // Read the first four bytes of the file.
      val is = Files.newInputStream(path)
      val b1 = is.read()
      val b2 = is.read()
      val b3 = is.read()
      val b4 = is.read()
      is.close()

      // Check if the four first bytes match CAFE BABE.
      return b1 == 0xCA && b2 == 0xFE && b3 == 0xBA && b4 == 0xBE
    }
    false
  }

}

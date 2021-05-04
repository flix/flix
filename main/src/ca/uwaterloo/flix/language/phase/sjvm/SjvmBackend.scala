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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.ErasedAst.Root
import ca.uwaterloo.flix.language.ast.{PType, Symbol}
import ca.uwaterloo.flix.language.phase.Phase
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.util.Validation.ToSuccess
import ca.uwaterloo.flix.util.{InternalCompilerException, InternalRuntimeException, Validation}
import flix.runtime.ProxyObject

import java.lang.reflect.InvocationTargetException
import java.nio.file.{Files, LinkOption, Path, Paths}

object SjvmBackend extends Phase[Root, CompilationResult] {
  /**
   * The directory where to place the generated class files.
   */
  val TargetDirectory: Path = Paths.get("./target/flix/")

  /**
   * Emits JVM bytecode for the given AST `root`.
   */
  def run(input: Root)(implicit flix: Flix): Validation[CompilationResult, CompilationError] = flix.phase("SjvmBackend") {

    //
    // Put the AST root into implicit scope.
    //
    implicit val r: Root = input

    val allClasses: Map[JvmName, JvmClass] = flix.subphase("CodeGen") {
      //
      // Immediately return if in verification mode.
      //
      if (flix.options.verifier) {
        return new CompilationResult(input, None, Map.empty).toSuccess
      }

      //
      // Compute the set of closures in the program.
      //
      // val closures = SjvmOps.closuresOf(input)

      //
      // Compute the set of namespaces in the program.
      //
      // val namespaces = SjvmOps.namespacesOf(input)

      //
      // Compute the set of instantiated tags in the program.
      //
      // val tags = SjvmOps.tagsOf(input)

      //
      // Compute the set of types in the program.
      //
      // val types = SjvmOps.typesOf(input)

      //
      // Generate the main class.
      //
      val mainClass = GenMainClass.gen()

      //
      // Generate references classes.
      //
      val refClasses = GenRefClasses.gen()

      //
      // Generate lazy classes.
      //
//      val lazyClasses = GenLazyClasses.gen()

      //
      // Collect all the classes and interfaces together.
      //
      List(
        mainClass,
        refClasses,
//        lazyClasses
      ).reduce(_ ++ _)
    }

    //
    // Write each class (and interface) to disk.
    //
    // NB: In interactive and test mode we skip writing the files to disk.
    if (flix.options.writeClassFiles && !flix.options.test) {
      flix.subphase("WriteClasses") {
        for ((jvmName, jvmClass) <- allClasses) {
          writeClass(TargetDirectory, jvmClass)
        }
      }
    }

    val loadClasses = flix.options.loadClassFiles

    if (!loadClasses) {
      //
      // Do not load any classes.
      //
      new CompilationResult(input, None, Map.empty).toSuccess
    } else {
      //
      // Loads all the generated classes into the JVM and decorates the AST.
      //
      Bootstrap.bootstrap(allClasses)

      //
      // Return the compilation result.
      //
      new CompilationResult(input, getCompiledMain(input), getCompiledDefs(input)).toSuccess
    }
  }

  /**
   * Optionally returns a reference to main.
   */
  private def getCompiledMain(root: Root)(implicit flix: Flix): Option[Array[String] => Int] = {
    root.defs.get(Symbol.Main) map { defn =>
      (actualArgs: Array[String]) => {
        val args: Array[AnyRef] = Array(actualArgs)
        val result = link(defn.sym, root).apply(args).getValue
        result.asInstanceOf[Integer].intValue()
      }
    }
  }

  /**
   * Returns a map from definition symbols to executable functions (backed by JVM backend).
   */
  private def getCompiledDefs(root: Root)(implicit flix: Flix): Map[Symbol.DefnSym, () => ProxyObject] = {
    root.defs.foldLeft(Map.empty[Symbol.DefnSym, () => ProxyObject]) {
      case (macc, (sym, defn)) =>
        val args: Array[AnyRef] = Array(null)
        macc + (sym -> (() => link(sym, root).apply(args)))
    }
  }

  /**
   * Returns a function object for the given definition symbol `sym`.
   */
  private def link(sym: Symbol.DefnSym, root: Root)(implicit flix: Flix): java.util.function.Function[Array[AnyRef], ProxyObject] = {
    (args: Array[AnyRef]) => {
      ///
      /// Retrieve the definition and its type.
      ///
      val defn = root.defs(sym)

      ///
      /// Construct the arguments array.
      ///
      val argsArray = if (args.isEmpty) Array(null) else args
      if (argsArray.length != defn.method.getParameterCount) {
        throw InternalRuntimeException(s"Expected ${defn.method.getParameterCount} arguments, but got: ${argsArray.length} for method ${defn.method.getName}.")
      }

      ///
      /// Perform the method call using reflection.
      ///
      try {
        // Call the method passing the arguments.
        val result = defn.method.invoke(null, argsArray: _*)

        // Construct a fresh proxy object.
        newProxyObj(result)
      } catch {
        case e: InvocationTargetException =>
          // Rethrow the underlying exception.
          throw e.getTargetException
      }
    }
  }

  /**
   * Returns a proxy object that wraps the given result value.
   */
  private def newProxyObj[T <: PType](result: AnyRef)(implicit flix: Flix): ProxyObject = {
    // Lookup the Equality method.
    val eq = null

    // Lookup the HashCode method.
    val hash = null

    // Lookup the ToString method.
    val toString = null

    // Create the proxy object.
    ProxyObject.of(result, eq, hash, toString)
  }

  /**
   * Writes the given JVM class `clazz` to a sub path under the given `prefixPath`.
   *
   * For example, if the prefix path is `/tmp/` and the class name is Foo.Bar.Baz
   * then the bytecode is written to the path `/tmp/Foo/Bar/Baz.class` provided
   * that this path either does not exist or is already a JVM class file.
   */
  private def writeClass(prefixPath: Path, clazz: JvmClass): Unit = {
    // Compute the absolute path of the class file to write.
    val path = prefixPath.resolve(clazz.name.toPath).toAbsolutePath

    // Create all parent directories (in case they don't exist).
    Files.createDirectories(path.getParent)

    // Check if the file already exists.
    if (Files.exists(path)) {
      // Check that the file is a regular file.
      if (!Files.isRegularFile(path, LinkOption.NOFOLLOW_LINKS)) {
        throw InternalCompilerException(s"Unable to write to non-regular file: '$path'.")
      }

      // Check if the file is writable.
      if (!Files.isWritable(path)) {
        throw InternalCompilerException(s"Unable to write to read-only file: '$path'.")
      }

      // Check that the file is empty or a class file.
      if (!(isEmpty(path) || isClassFile(path))) {
        throw InternalCompilerException(s"Refusing to overwrite non-empty, non-class file: '$path'.")
      }
    }

    // Write the bytecode.
    Files.write(path, clazz.bytecode)
  }

  /**
   * Returns `true` if the given `path` is non-empty (i.e. contains data).
   */
  private def isEmpty(path: Path): Boolean = {
    Files.size(path) == 0L
  }

  /**
   * Returns `true` if the given `path` exists and is a Java Virtual Machine class file.
   */
  private def isClassFile(path: Path): Boolean = {
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

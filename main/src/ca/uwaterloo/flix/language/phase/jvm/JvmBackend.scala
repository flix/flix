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

import java.lang.reflect.InvocationTargetException

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.FinalAst._
import ca.uwaterloo.flix.language.ast.{MonoType, Symbol}
import ca.uwaterloo.flix.language.phase.Phase
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{InternalRuntimeException, Validation}
import flix.runtime.ProxyObject


object JvmBackend extends Phase[Root, CompilationResult] {

  /**
    * Emits JVM bytecode for the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): Validation[CompilationResult, CompilationError] = flix.phase("JvmBackend") {

    //
    // Put the AST root into implicit scope.
    //
    implicit val r: Root = root

    // Generate all classes.
    val allClasses = flix.subphase("CodeGen") {

      //
      // Compute the set of closures in the program.
      //
      val closures = JvmOps.closuresOf(root)

      //
      // Compute the set of namespaces in the program.
      //
      val namespaces = JvmOps.namespacesOf(root)

      //
      // Compute the set of instantiated tags in the program.
      //
      val tags = JvmOps.tagsOf(root)

      //
      // Compute the set of types in the program.
      //
      val types = JvmOps.typesOf(root)

      //
      // Generate the main class.
      //
      val mainClass = GenMainClass.gen()

      //
      // Generate the Context class.
      //
      val contextClass = GenContext.gen(namespaces)

      //
      // Generate the namespace classes.
      //
      val namespaceClasses = GenNamespaces.gen(namespaces)

      //
      // Generate continuation interfaces for each function type in the program.
      //
      val continuationInterfaces = GenContinuationInterfaces.gen(types)

      //
      // Generate function interfaces for each function type in the program.
      //
      val functionInterfaces = GenFunctionInterfaces.gen(types)

      //
      // Generate function classes for each function in the program.
      //
      val functionClasses = GenFunctionClasses.gen(root.defs)

      //
      // Generate closure classes for each closure in the program.
      //
      val closureClasses = GenClosureClasses.gen(closures)

      //
      // Generate enum interfaces for each enum type in the program.
      //
      val enumInterfaces = GenEnumInterfaces.gen(types)

      //
      // Generate tag classes for each enum instantiation in the program.
      //
      val tagClasses = GenTagClasses.gen(tags)

      //
      // Generate tuple interfaces for each tuple type in the program.
      //
      val tupleInterfaces = GenTupleInterfaces.gen(types)

      //
      // Generate tuple classes for each tuple type in the program.
      //
      val tupleClasses = GenTupleClasses.gen(types)

      //
      // Generate record interface.
      //
      val recordInterfaces = GenRecordInterfaces.gen()

      //
      // Generate empty record class.
      //
      val recordEmptyClasses = GenRecordEmpty.gen()

      //
      // Generate extended record classes for each (different) RecordExtend type in the program
      //
      val recordExtendClasses = GenRecordExtend.gen(types)

      //
      // Generate references classes.
      //
      val refClasses = GenRefClasses.gen()

      //
      // Generate lazy classes.
      //
      val lazyClasses = GenLazyClasses.gen(types)

      //
      // Generate the Unit class.
      //
      val unitClass = GenUnitClass.gen()

      //
      // Generate the FlixError class.
      //
      val flixErrorClass = GenFlixErrorClass.gen()

      //
      // Generate the ReifiedSourceLocation class.
      //
      val rslClass = GenReifiedSourceLocationClass.gen()

      //
      // Generate the HoleError class.
      //
      val holeErrorClass = GenHoleErrorClass.gen()

      //
      // Generate the MatchError class.
      //
      val matchErrorClass = GenSimpleLocationErrorClass.gen(JvmName.MatchError, "Non-exhaustive match at ")

      //
      // Generate the MatchError class.
      //
      val notImplementedErrorClass = GenSimpleLocationErrorClass.gen(JvmName.NotImplementedError, "Implementation missing at ")

      //
      // Collect all the classes and interfaces together.
      //
      List(
        mainClass,
        contextClass,
        namespaceClasses,
        continuationInterfaces,
        functionInterfaces,
        functionClasses,
        closureClasses,
        enumInterfaces,
        tagClasses,
        tupleInterfaces,
        tupleClasses,
        recordInterfaces,
        recordEmptyClasses,
        recordExtendClasses,
        refClasses,
        lazyClasses,
        unitClass,
        flixErrorClass,
        rslClass,
        holeErrorClass,
        matchErrorClass,
        notImplementedErrorClass
      ).reduce(_ ++ _)
    }

    //
    // Write each class (and interface) to disk.
    //
    // NB: In interactive and test mode we skip writing the files to disk.
    if (flix.options.writeClassFiles && !flix.options.test) {
      flix.subphase("WriteClasses") {
        for ((_, jvmClass) <- allClasses) {
          JvmOps.writeClass(flix.options.targetDirectory, jvmClass)
        }
      }
    }

    val loadClasses = flix.options.loadClassFiles

    if (!loadClasses) {
      //
      // Do not load any classes.
      //
      new CompilationResult(root, None, Map.empty).toSuccess
    } else {
      //
      // Loads all the generated classes into the JVM and decorates the AST.
      //
      Bootstrap.bootstrap(allClasses)

      //
      // Return the compilation result.
      //
      new CompilationResult(root, getCompiledMain(root), getCompiledDefs(root)).toSuccess
    }
  }

  /**
    * Optionally returns a reference to main.
    */
  private def getCompiledMain(root: Root)(implicit flix: Flix): Option[Array[String] => Int] =
    root.defs.get(Symbol.Main) map { defn =>
        (actualArgs: Array[String]) => {
          val args: Array[AnyRef] = Array(actualArgs)
          val result = link(defn.sym, root).apply(args).getValue
          result.asInstanceOf[Integer].intValue()
        }
    }

  /**
    * Returns a map from definition symbols to executable functions (backed by JVM backend).
    */
  private def getCompiledDefs(root: Root)(implicit flix: Flix): Map[Symbol.DefnSym, () => ProxyObject] =
    root.defs.foldLeft(Map.empty[Symbol.DefnSym, () => ProxyObject]) {
      case (macc, (sym, _)) =>
        val args: Array[AnyRef] = Array(null)
        macc + (sym -> (() => link(sym, root).apply(args)))
    }

  /**
    * Returns a function object for the given definition symbol `sym`.
    */
  private def link(sym: Symbol.DefnSym, root: Root)(implicit flix: Flix): java.util.function.Function[Array[AnyRef], ProxyObject] =
    (args: Array[AnyRef]) => {
      ///
      /// Retrieve the definition and its type.
      ///
      val defn = root.defs(sym)
      val MonoType.Arrow(_, _) = defn.tpe

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

  /**
    * Returns a proxy object that wraps the given result value.
    */
  private def newProxyObj(result: AnyRef)(implicit flix: Flix): ProxyObject = {
    // Lookup the Equality method.
    val eq = null

    // Lookup the HashCode method.
    val hash = null

    // Lookup the ToString method.
    val toString = null

    // Create the proxy object.
    ProxyObject.of(result, eq, hash, toString)
  }

}

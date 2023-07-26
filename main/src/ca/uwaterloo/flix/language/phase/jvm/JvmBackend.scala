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
import ca.uwaterloo.flix.language.ast.ReducedAst._
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.runtime.CompilationResult

import java.lang.reflect.InvocationTargetException

object JvmBackend {

  /**
    * Emits JVM bytecode for the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): CompilationResult = flix.phase("JvmBackend") {

    //
    // Put the AST root into implicit scope.
    //
    implicit val r: Root = root

    // Generate all classes.
    val allClasses = flix.subphase("CodeGen") {

      //
      // Compute the set of namespaces in the program.
      //
      val namespaces = JvmOps.namespacesOf(root)

      //
      // Compute the set of types in the program.
      //
      val types = JvmOps.typesOf(root)

      val erasedRefTypes: Iterable[BackendObjType.Ref] = JvmOps.getRefsOf(types)
      val erasedExtendTypes: Iterable[BackendObjType.RecordExtend] = JvmOps.getRecordExtendsOf(types)
      val erasedFunctionTypes: Iterable[BackendObjType.Arrow] = JvmOps.getArrowsOf(types)
      val erasedContinuations: Iterable[BackendObjType.Continuation] =
        erasedFunctionTypes.map(f => BackendObjType.Continuation(f.result)).toSet
      // TODO: all the type collection above should maybe have its own subphase

      //
      // Compute the set of anonymous classes (NewObjects) in the program.
      //
      val anonClassDefs = root.anonClasses

      //
      // Generate the main class.
      //
      val mainClass = GenMainClass.gen()

      //
      // Generate the namespace classes.
      //
      val namespaceClasses = GenNamespaceClasses.gen(namespaces)

      //
      // Generate continuation classes for each function type in the program.
      //
      val continuationInterfaces = GenContinuationAbstractClasses.gen(erasedContinuations)

      //
      // Generate a function abstract class for each function type in the program.
      //
      val functionInterfaces = GenFunctionAbstractClasses.gen(erasedFunctionTypes)

      //
      // Generate function classes for each function in the program.
      //
      val functionClasses = GenFunctionClasses.gen(root.defs)

      //
      // Generate closure abstract classes for each function in the program.
      //
      val closureAbstractClasses = GenClosureAbstractClasses.gen(types)

      //
      // Generate closure classes for each closure in the program.
      //
      val closureClasses = GenClosureClasses.gen(root.defs)

      //
      // Generate enum interfaces for each enum type in the program.
      //
      val enumInterfaces = GenEnumInterfaces.gen(root.enums.values)

      //
      // Generate tag classes for each enum instantiation in the program.
      //
      val tagClasses = GenTagClasses.gen(root.enums.values.flatMap(_.cases.values))

      //
      // Generate tuple classes for each tuple type in the program.
      //
      val tupleClasses = GenTupleClasses.gen(types)

      //
      // Generate record interface.
      //
      val recordInterfaces = GenRecordInterface.gen()

      //
      // Generate empty record class.
      //
      val recordEmptyClasses = GenRecordEmptyClass.gen()

      //
      // Generate extended record classes for each (different) RecordExtend type in the program
      //
      val recordExtendClasses = GenRecordExtendClasses.gen(erasedExtendTypes)

      //
      // Generate references classes.
      //
      val refClasses = GenRefClasses.gen(erasedRefTypes)

      //
      // Generate lazy classes.
      //
      val lazyClasses = GenLazyClasses.gen(types)

      //
      // Generate anonymous classes.
      //
      val anonClasses = GenAnonymousClasses.gen(anonClassDefs)

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
      val matchErrorClass = GenMatchErrorClass.gen()

      //
      // Generate the Global class.
      //
      val globalClass = GenGlobalClass.gen()

      //
      // Generate the Region class.
      //
      val regionClass = GenRegionClass.gen()

      //
      // Generate the UncaughtExceptionHandler class.
      //
      val uncaughtExceptionHandlerClass = GenUncaughtExceptionHandlerClass.gen()

      //
      // Collect all the classes and interfaces together.
      //
      List(
        mainClass,
        namespaceClasses,
        continuationInterfaces,
        functionInterfaces,
        functionClasses,
        closureAbstractClasses,
        closureClasses,
        enumInterfaces,
        tagClasses,
        tupleClasses,
        recordInterfaces,
        recordEmptyClasses,
        recordExtendClasses,
        refClasses,
        lazyClasses,
        anonClasses,
        unitClass,
        flixErrorClass,
        rslClass,
        holeErrorClass,
        matchErrorClass,
        globalClass,
        regionClass,
        uncaughtExceptionHandlerClass
      ).reduce(_ ++ _)
    }

    //
    // Write each class (and interface) to disk.
    //
    // NB: In interactive and test mode we skip writing the files to disk.
    if (flix.options.output.nonEmpty) {
      flix.subphase("WriteClasses") {
        for ((_, jvmClass) <- allClasses) {
          flix.subtask(jvmClass.name.toBinaryName, sample = true)
          JvmOps.writeClass(flix.options.output.get, jvmClass)
        }
      }
    }

    val outputBytes = allClasses.map(_._2.bytecode.length).sum

    val loadClasses = flix.options.loadClassFiles

    if (!loadClasses) {
      //
      // Do not load any classes.
      //
      new CompilationResult(root, None, Map.empty, flix.getTotalTime, outputBytes)
    } else {
      //
      // Loads all the generated classes into the JVM and decorates the AST.
      // Returns the main of `Main.class` if it exists.
      //
      val main = Loader.load(allClasses)

      //
      // Return the compilation result.
      //
      new CompilationResult(root, main, getCompiledDefs(root), flix.getTotalTime, outputBytes)
    }
  }

  /**
    * Returns a map from non-closure definition symbols to executable functions (backed by JVM backend).
    */
  private def getCompiledDefs(root: Root)(implicit flix: Flix): Map[Symbol.DefnSym, () => AnyRef] =
    root.defs.foldLeft(Map.empty[Symbol.DefnSym, () => AnyRef]) {
      case (macc, (_, defn)) if defn.cparams.nonEmpty =>
        macc
      case (macc, (sym, _)) =>
        val args: Array[AnyRef] = Array(null)
        macc + (sym -> (() => link(sym, root).apply(args)))
    }

  /**
    * Returns a function object for the given definition symbol `sym`.
    */
  private def link(sym: Symbol.DefnSym, root: Root)(implicit flix: Flix): java.util.function.Function[Array[AnyRef], AnyRef] =
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
        throw new RuntimeException(s"Expected ${defn.method.getParameterCount} arguments, but got: ${argsArray.length} for method ${defn.method.getName}.")
      }

      ///
      /// Perform the method call using reflection.
      ///
      try {
        // Call the method passing the arguments.
        val result = defn.method.invoke(null, argsArray: _*)
        result
      } catch {
        case e: InvocationTargetException =>
          // Rethrow the underlying exception.
          throw e.getTargetException
      }
    }

}

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
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol}
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.util.InternalCompilerException

import java.lang.reflect.InvocationTargetException

object JvmBackend {

  /**
    * Emits JVM bytecode for the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): CompilationResult = flix.phase("JvmBackend") {

    // Put the AST root into implicit scope.
    implicit val r: Root = root

    // Generate all classes.
    val allClasses = flix.subphase("CodeGen") {

      //
      // First, collect information and types needed to generate classes.
      //

      // Compute the set of namespaces in the program.
      val namespaces = JvmOps.namespacesOf(root)

      // Compute the set of flattened types in the program.
      // All inner types will also be present in the set.
      val types = JvmOps.typesOf(root)

      // Filter the program types into different sets
      val erasedRefTypes = JvmOps.getErasedRefsOf(types)
      val erasedExtendTypes = JvmOps.getErasedRecordExtendsOf(types)
      val erasedFunctionTypes = JvmOps.getErasedArrowsOf(types)
      val erasedContinuationTypes = erasedFunctionTypes.map(f => BackendObjType.Continuation(f.result))

      //
      // Second, generate classes.
      //

      // Compute the set of anonymous classes (NewObjects) in the program.
      val anonClassDefs = root.anonClasses

      // Generate the main class.
      val mainClass = GenMainClass.gen()

      // Generate the namespace classes.
      val namespaceClasses = GenNamespaceClasses.gen(namespaces)

      // Generate function classes.
      val continuationInterfaces = GenContinuationAbstractClasses.gen(erasedContinuationTypes)
      val functionInterfaces = GenFunctionAbstractClasses.gen(erasedFunctionTypes)
      val functionClasses = GenFunctionClasses.gen(root.defs)
      val closureAbstractClasses = GenClosureAbstractClasses.gen(types)
      val closureClasses = GenClosureClasses.gen(root.defs)

      // Generate enum classes.
      val enumInterfaces = GenEnumInterfaces.gen(root.enums.values)
      val tagClasses = GenTagClasses.gen(root.enums.values.flatMap(_.cases.values))

      // Generate tuple classes for each tuple type in the program.
      val tupleClasses = GenTupleClasses.gen(types)

      // Generate record classes.
      val recordInterfaces = GenRecordInterface.gen()
      val recordEmptyClasses = GenRecordEmptyClass.gen()
      val recordExtendClasses = GenRecordExtendClasses.gen(erasedExtendTypes)

      // Generate references classes.
      val refClasses = GenRefClasses.gen(erasedRefTypes)

      // Generate lazy classes.
      val lazyClasses = GenLazyClasses.gen(types)

      // Generate anonymous classes.
      val anonClasses = GenAnonymousClasses.gen(anonClassDefs)

      // Generate the Unit class.
      val unitClass = GenUnitClass.gen()

      // Generate error classes.
      val flixErrorClass = GenFlixErrorClass.gen()
      val rslClass = GenReifiedSourceLocationClass.gen()
      val holeErrorClass = GenHoleErrorClass.gen()
      val matchErrorClass = GenMatchErrorClass.gen()

      // Generate the Global class.
      val globalClass = GenGlobalClass.gen()

      // Generate the Region class.
      val regionClass = GenRegionClass.gen()

      // Generate the UncaughtExceptionHandler class.
      val uncaughtExceptionHandlerClass = GenUncaughtExceptionHandlerClass.gen()

      // Generate new (unused) effect handler classes.
      val resultInterface = Map(BackendObjType.Result.jvmName -> JvmClass(BackendObjType.Result.jvmName, BackendObjType.Result.genByteCode()))
      val valueClass = Map(BackendObjType.Value.jvmName -> JvmClass(BackendObjType.Value.jvmName, BackendObjType.Value.genByteCode()))
      val frameInterface = Map(BackendObjType.Frame.jvmName -> JvmClass(BackendObjType.Frame.jvmName, BackendObjType.Frame.genByteCode()))
      val thunkInterface = Map(BackendObjType.Thunk.jvmName -> JvmClass(BackendObjType.Thunk.jvmName, BackendObjType.Thunk.genByteCode()))

      // Collect all the classes and interfaces together.
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
        uncaughtExceptionHandlerClass,
        resultInterface,
        valueClass,
        frameInterface,
        thunkInterface
      ).reduce(_ ++ _)
    }

    // Write each class (and interface) to disk.
    // NB: In interactive and test mode we skip writing the files to disk.
    if (flix.options.output.nonEmpty) {
      flix.subphase("WriteClasses") {
        for ((_, jvmClass) <- allClasses) {
          flix.subtask(jvmClass.name.toBinaryName, sample = true)
          JvmOps.writeClass(flix.options.output.get, jvmClass)
        }
      }
    }

    // Collect code size for performance tracking.
    val outputBytes = allClasses.map(_._2.bytecode.length).sum

    val loadClasses = flix.options.loadClassFiles

    if (!loadClasses) {
      // Do not load any classes.
      new CompilationResult(root, None, Map.empty, flix.getTotalTime, outputBytes)
    } else {
      // Loads all the generated classes into the JVM and initializes the method field of defs.
      // Returns the main of `Main.class` if it exists.
      val main = Loader.load(allClasses)

      // Return the compilation result.
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
  private def link(sym: Symbol.DefnSym, root: Root)(implicit flix: Flix): java.util.function.Function[Array[AnyRef], AnyRef] = {
    // Retrieve the definition and its type.
    val defn = root.defs.getOrElse(sym, throw InternalCompilerException(s"Linking error: '$sym' cannot be found in root defs", SourceLocation.Unknown))
    // Check that the method is initialized.
    if (defn.method == null) throw InternalCompilerException(s"Linking error: '$sym' has an uninitialized method.", SourceLocation.Unknown)

    // Construct the reflected function.
    (args: Array[AnyRef]) => {
      // Construct the arguments array.
      val argsArray = if (args.isEmpty) Array(null) else args
      val parameterCount = defn.method.getParameterCount
      val argumentCount = argsArray.length
      if (argumentCount != parameterCount) {
        throw new RuntimeException(s"Expected $parameterCount arguments, but got: $argumentCount for method ${defn.method.getName}.")
      }

      // Perform the method call using reflection.
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

}

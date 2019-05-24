/*
 * Copyright 2019 Miguel Fialho
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
package ca.uwaterloo.flix.language.phase.njvm

import java.lang.reflect.InvocationTargetException
import java.nio.file.{Path, Paths}
import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.FinalAst._
import ca.uwaterloo.flix.language.ast.{MonoType, SpecialOperator, Symbol}
import ca.uwaterloo.flix.language.phase.Phase
import ca.uwaterloo.flix.language.phase.jvm.{Bootstrap, GenFunctionClasses, JvmName, JvmOps}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics._
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.runtime.interpreter.Interpreter
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.{Evaluation, InternalRuntimeException, Validation}
import flix.runtime.ProxyObject

object NJvmBackend extends Phase[Root, CompilationResult] {

  /**
    * The directory where to place the generated class files.
    */
  val TargetDirectory: Path = Paths.get("./target/flix/")

  /**
    * Emits JVM bytecode for the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): Validation[CompilationResult, CompilationError] = flix.phase("JvmBackend") {
    //
    // Immediately return if in interpreted mode.
    //
    if (flix.options.evaluation == Evaluation.Interpreted) {
      return new CompilationResult(root, getInterpretedDefs(root)).toSuccess
    }

    //
    // Immediately return if in verification mode.
    //
    if (flix.options.verifier) {
      return new CompilationResult(root, Map.empty).toSuccess
    }

    //
    // Put the AST root into implicit scope.
    //
    implicit val _ = root

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
    // Generate function classes for each function in the program.
    //
    val functionClasses = GenFunctionClasses.gen(root.defs)


    /** Generated classes using NJVM */
    val map: Map[JvmName, MnemonicsClass] = Map()
    val classes: List[MnemonicsGenerator] =
    //Generate interfaces first
      List(
        GenContinuationInterfaces,
        GenFunctionInterfaces,
        GenEnumInterfaces,
        GenTupleInterfaces,
        GenRecordInterface,
        GenRecordEmpty,
        GenRecordExtend,
        GenRefClasses,
        GenTagClasses,
        GenTupleClasses,
        GenContextClass,
        GenNamespacesClasses,
        GenMainClass,
        GenClosuresClasses
      )


    val njvmClasses = classes.foldLeft(map) { (acc, i) => i.gen(acc, types, tags, namespaces, closures, root.defs) }
      .map(f => (f._1, f._2.getJvmClass))

    //
    // Collect all the classes and interfaces together.
    //
    val allClasses = List(
      functionClasses,
      njvmClasses
    ).reduce(_ ++ _)

    //
    // Write each class (and interface) to disk.
    //
    // NB: In interactive and test mode we skip writing the files to disk.
    if (flix.options.writeClassFiles && !flix.options.test) {
      flix.subphase("WriteClasses") {
        for ((_, jvmClass) <- allClasses) {
          JvmOps.writeClass(TargetDirectory, jvmClass)
        }
      }
    }

    //
    // Loads all the generated classes into the JVM and decorates the AST.
    //
    Bootstrap.bootstrap(allClasses)

    //
    // Return the compilation result.
    //
    new CompilationResult(root, getCompiledDefs(root)).toSuccess
  }

  /**
    * Returns a map from definition symbols to executable functions (backed by the interpreter).
    */
  private def getInterpretedDefs(root: Root)(implicit flix: Flix): Map[Symbol.DefnSym, () => ProxyObject] =
    root.defs.foldLeft(Map.empty[Symbol.DefnSym, () => ProxyObject]) {
      case (macc, (sym, defn)) =>
        val args: Array[AnyRef] = Array(null)
        macc + (sym -> (() => Interpreter.link(sym, root).apply(args)))
    }

  /**
    * Returns a map from definition symbols to executable functions (backed by JVM backend).
    */
  private def getCompiledDefs(root: Root)(implicit flix: Flix): Map[Symbol.DefnSym, () => ProxyObject] =
    root.defs.foldLeft(Map.empty[Symbol.DefnSym, () => ProxyObject]) {
      case (macc, (sym, defn)) =>
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
      val MonoType.Arrow(targs, tresult) = defn.tpe

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
        newProxyObj(result, tresult, root)
      } catch {
        case e: InvocationTargetException =>
          // Rethrow the underlying exception.
          throw e.getTargetException
      }
    }

  /**
    * Returns a proxy object that wraps the given result value.
    */
  private def newProxyObj(result: AnyRef, resultType: MonoType, root: Root)(implicit flix: Flix): ProxyObject = {
    // Lookup the Equality method.
    val eq = root.specialOps.getOrElse(SpecialOperator.Equality, Map.empty).get(resultType) match {
      case None => null
      case Some(defn) => link(defn, root)
    }

    // Lookup the HashCode method.
    val hash = root.specialOps.getOrElse(SpecialOperator.HashCode, Map.empty).get(resultType) match {
      case None => null
      case Some(defn) => link(defn, root)
    }

    // Lookup the ToString method.
    val toString = root.specialOps.getOrElse(SpecialOperator.ToString, Map.empty).get(resultType) match {
      case None => null
      case Some(defn) => link(defn, root)
    }

    // Create the proxy object.
    ProxyObject.of(result, eq, hash, toString)
  }

}

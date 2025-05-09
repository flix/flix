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
import ca.uwaterloo.flix.language.ast.ReducedAst.*
import ca.uwaterloo.flix.language.ast.{MonoType, SourceLocation, Symbol}
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugNoOp
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.util.InternalCompilerException

import java.lang.reflect.InvocationTargetException

object JvmBackend {

  /** Emits JVM bytecode for `root`. */
  def run(root: Root)(implicit flix: Flix): CompilationResult = flix.phase("JvmBackend") {
    implicit val r: Root = root

    // Types/classes required for Flix runtime.
    val requiredTypes = Set(
      MonoType.Arrow(List(MonoType.Bool), MonoType.Object), // by resumptionWrappers
      MonoType.Arrow(List(MonoType.Char), MonoType.Object), // by resumptionWrappers
      MonoType.Arrow(List(MonoType.Int8), MonoType.Object), // by resumptionWrappers
      MonoType.Arrow(List(MonoType.Int16), MonoType.Object), // by resumptionWrappers
      MonoType.Arrow(List(MonoType.Int32), MonoType.Object), // by resumptionWrappers
      MonoType.Arrow(List(MonoType.Int64), MonoType.Object), // by resumptionWrappers
      MonoType.Arrow(List(MonoType.Float32), MonoType.Object), // by resumptionWrappers
      MonoType.Arrow(List(MonoType.Float64), MonoType.Object), // by resumptionWrappers
      MonoType.Arrow(List(MonoType.Object), MonoType.Object), // by resumptionWrappers
    )
    val allTypes = root.types ++ requiredTypes

    val mainClass = root.getMain.map(
      main => Map(genClass(BackendObjType.Main(main.sym)))
    ).getOrElse(Map.empty)

    val namespaceClasses = GenNamespaceClasses.gen(JvmOps.namespacesOf(root))

    // Generate function classes.
    val functionAndClosureClasses = GenFunAndClosureClasses.gen(root.defs)
    val erasedFunctionTypes = JvmOps.getErasedArrowsOf(allTypes)
    val functionInterfaces = erasedFunctionTypes.map(genClass).toMap
    val closureAbstractClasses = erasedFunctionTypes.map {
      case BackendObjType.Arrow(args, result) => BackendObjType.AbstractArrow(args, result)
    }.map(genClass).toMap

    val taggedAbstractClass = Map(genClass(BackendObjType.Tagged))
    val tagClasses = JvmOps.getErasedTagTypesOf(root, allTypes).map(genClass).toMap

    val tupleClasses = JvmOps.getErasedTupleTypesOf(allTypes).map(genClass).toMap
    val structClasses = JvmOps.getErasedStructTypesOf(root, allTypes).map(genClass).toMap

    val recordInterfaces = Map(genClass(BackendObjType.Record))
    val recordEmptyClasses = Map(genClass(BackendObjType.RecordEmpty))
    val recordExtendClasses = JvmOps.getErasedRecordExtendsOf(allTypes).map(genClass).toMap

    val lazyClasses = JvmOps.getErasedLazyTypesOf(allTypes).map(genClass).toMap

    val anonClasses = GenAnonymousClasses.gen(root.anonClasses)

    val unitClass = Map(genClass(BackendObjType.Unit))

    val flixErrorClass = Map(genClass(BackendObjType.FlixError))
    val rslClass = Map(genClass(BackendObjType.ReifiedSourceLocation))
    val holeErrorClass = Map(genClass(BackendObjType.HoleError))
    val matchErrorClass = Map(genClass(BackendObjType.MatchError))
    val castErrorClass = Map(genClass(BackendObjType.CastError))
    val unhandledEffectErrorClass = Map(genClass(BackendObjType.UnhandledEffectError))

    val globalClass = Map(genClass(BackendObjType.Global))

    val regionClass = Map(genClass(BackendObjType.Region))

    val uncaughtExceptionHandlerClass = Map(genClass(BackendObjType.UncaughtExceptionHandler))

    // Effect runtime classes.
    val resultInterface = Map(genClass(BackendObjType.Result))
    val valueClass = Map(genClass(BackendObjType.Value))
    val frameInterface = Map(genClass(BackendObjType.Frame))
    val thunkAbstractClass = Map(genClass(BackendObjType.Thunk))
    val suspensionClass = Map(genClass(BackendObjType.Suspension))
    val framesInterface = Map(genClass(BackendObjType.Frames))
    val framesConsClass = Map(genClass(BackendObjType.FramesCons))
    val framesNilClass = Map(genClass(BackendObjType.FramesNil))
    val resumptionInterface = Map(genClass(BackendObjType.Resumption))
    val resumptionConsClass = Map(genClass(BackendObjType.ResumptionCons))
    val resumptionNilClass = Map(genClass(BackendObjType.ResumptionNil))
    val handlerInterface = Map(genClass(BackendObjType.Handler))
    val effectCallClass = Map(genClass(BackendObjType.EffectCall))
    val effectClasses = GenEffectClasses.gen(root.effects.values)
    val resumptionWrappers = BackendType.erasedTypes.map(BackendObjType.ResumptionWrapper.apply).map(genClass).toMap

    val allClasses = List(
      mainClass,
      namespaceClasses,
      functionInterfaces,
      functionAndClosureClasses,
      closureAbstractClasses,
      taggedAbstractClass,
      tagClasses,
      tupleClasses,
      structClasses,
      recordInterfaces,
      recordEmptyClasses,
      recordExtendClasses,
      lazyClasses,
      anonClasses,
      unitClass,
      flixErrorClass,
      rslClass,
      holeErrorClass,
      matchErrorClass,
      castErrorClass,
      unhandledEffectErrorClass,
      globalClass,
      regionClass,
      uncaughtExceptionHandlerClass,
      resultInterface,
      valueClass,
      frameInterface,
      thunkAbstractClass,
      suspensionClass,
      framesInterface,
      framesConsClass,
      framesNilClass,
      resumptionInterface,
      resumptionConsClass,
      resumptionNilClass,
      handlerInterface,
      effectCallClass,
      effectClasses,
      resumptionWrappers
    ).reduce(_ ++ _)

    // Write each class (and interface) to disk if enabled.
    if (flix.options.output.nonEmpty) {
      for ((_, jvmClass) <- allClasses) {
        flix.subtask(jvmClass.name.toBinaryName, sample = true)
        JvmOps.writeClass(flix.options.output.get.resolve("class/"), jvmClass)
      }
    }

    // Collect code size for performance tracking.
    val outputBytes = allClasses.map(_._2.bytecode.length).sum

    if (flix.options.loadClassFiles) {
      val main = Loader.load(allClasses)
      new CompilationResult(root, main, getCompiledDefs(root), flix.getTotalTime, outputBytes)
    } else {
      new CompilationResult(root, None, Map.empty, flix.getTotalTime, outputBytes)
    }

  }(DebugNoOp())

  /** Unpacks `g` into a name and a class. */
  private def genClass(g: Generatable)(implicit flix: Flix): (JvmName, JvmClass) =
    (g.jvmName, JvmClass(g.jvmName, g.genByteCode()))

  /** Returns the non-closure, executable jvm functions of `root`. */
  private def getCompiledDefs(root: Root): Map[Symbol.DefnSym, () => AnyRef] = {
    root.defs.filter(_._2.cparams.isEmpty).map {
      case (sym, _) =>
        val args: Array[AnyRef] = Array(null)
        (sym, () => link(sym, root)(args))
    }
  }

  /** Returns a function object for `sym`. */
  private def link(sym: Symbol.DefnSym, root: Root): java.util.function.Function[Array[AnyRef], AnyRef] = {
    val defn = root.defs(sym)
    // Check that the method has been initialized.
    if (defn.method == null) throw InternalCompilerException(s"Linking error: '$sym' has an uninitialized method.", SourceLocation.Unknown)

    (args: Array[AnyRef]) => {
      // Convert and verify `args`.
      val argsArray = if (args.isEmpty) Array(null: AnyRef) else args
      val parameterCount = defn.method.getParameterCount
      val argumentCount = argsArray.length
      if (argumentCount != parameterCount) {
        throw new RuntimeException(s"Expected $parameterCount arguments, but got: $argumentCount for method ${defn.method.getName}.")
      }

      // Perform the method call using reflection.
      try {
        val result = defn.method.invoke(null, argsArray *)
        result
      } catch {
        case e: InvocationTargetException =>
          // Rethrow the underlying exception.
          throw e.getTargetException
      }
    }
  }

}

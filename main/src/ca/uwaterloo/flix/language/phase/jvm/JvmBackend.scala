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
      main => Map(genClass(BackendObjType.Main, BackendObjType.Main.genByteCode(main.sym)))
    ).getOrElse(Map.empty)

    val namespaceClasses = JvmOps.namespacesOf(root).map(
      ns => {
        val nsClass = BackendObjType.Namespace(ns.ns)
        val entrypointDefs = ns.defs.values.toList.filter(defn => root.entryPoints.contains(defn.sym))
        genClass(nsClass, nsClass.genByteCode(entrypointDefs))
      }).toMap

    // Generate function classes.
    val functionAndClosureClasses = GenFunAndClosureClasses.gen(root.defs)
    val erasedFunctionTypes = JvmOps.getErasedArrowsOf(allTypes)
    val functionInterfaces = erasedFunctionTypes.map(bt => genClass(bt, bt.genByteCode())).toMap
    val closureAbstractClasses = erasedFunctionTypes.map {
      case BackendObjType.Arrow(args, result) => BackendObjType.AbstractArrow(args, result)
    }.map(bt => genClass(bt, bt.genByteCode())).toMap

    val taggedAbstractClass = Map(genClass(BackendObjType.Tagged, BackendObjType.Tagged.genByteCode()))
    val tagClasses = JvmOps.getErasedTagTypesOf(allTypes).map(bt => genClass(bt, bt.genByteCode())).toMap
    val extensibleTagClasses = JvmOps.getErasedExtensibleTagTypesOf(allTypes).map(bt => genClass(bt, bt.genByteCode())).toMap

    val tupleClasses = JvmOps.getErasedTupleTypesOf(allTypes).map(bt => genClass(bt, bt.genByteCode())).toMap
    val structClasses = JvmOps.getErasedStructTypesOf(root, allTypes).map(bt => genClass(bt, bt.genByteCode())).toMap

    val recordInterfaces = Map(genClass(BackendObjType.Record, BackendObjType.Record.genByteCode()))
    val recordEmptyClasses = Map(genClass(BackendObjType.RecordEmpty, BackendObjType.RecordEmpty.genByteCode()))
    val recordExtendClasses = JvmOps.getErasedRecordExtendsOf(allTypes).map(bt => genClass(bt, bt.genByteCode())).toMap

    val lazyClasses = JvmOps.getErasedLazyTypesOf(allTypes).map(bt => genClass(bt, bt.genByteCode())).toMap

    val anonClasses = GenAnonymousClasses.gen(root.anonClasses)

    val unitClass = Map(genClass(BackendObjType.Unit, BackendObjType.Unit.genByteCode()))

    val flixErrorClass = Map(genClass(BackendObjType.FlixError, BackendObjType.FlixError.genByteCode()))
    val rslClass = Map(genClass(BackendObjType.ReifiedSourceLocation, BackendObjType.ReifiedSourceLocation.genByteCode()))
    val holeErrorClass = Map(genClass(BackendObjType.HoleError, BackendObjType.HoleError.genByteCode()))
    val matchErrorClass = Map(genClass(BackendObjType.MatchError, BackendObjType.MatchError.genByteCode()))
    val castErrorClass = Map(genClass(BackendObjType.CastError, BackendObjType.CastError.genByteCode()))
    val unhandledEffectErrorClass = Map(genClass(BackendObjType.UnhandledEffectError, BackendObjType.UnhandledEffectError.genByteCode()))

    val globalClass = Map(genClass(BackendObjType.Global, BackendObjType.Global.genByteCode()))

    val regionClass = Map(genClass(BackendObjType.Region, BackendObjType.Region.genByteCode()))

    val uncaughtExceptionHandlerClass = Map(genClass(BackendObjType.UncaughtExceptionHandler, BackendObjType.UncaughtExceptionHandler.genByteCode()))

    // Effect runtime classes.
    val resultInterface = Map(genClass(BackendObjType.Result, BackendObjType.Result.genByteCode()))
    val valueClass = Map(genClass(BackendObjType.Value, BackendObjType.Value.genByteCode()))
    val frameInterface = Map(genClass(BackendObjType.Frame, BackendObjType.Frame.genByteCode()))
    val thunkAbstractClass = Map(genClass(BackendObjType.Thunk, BackendObjType.Thunk.genByteCode()))
    val suspensionClass = Map(genClass(BackendObjType.Suspension, BackendObjType.Suspension.genByteCode()))
    val framesInterface = Map(genClass(BackendObjType.Frames, BackendObjType.Frames.genByteCode()))
    val framesConsClass = Map(genClass(BackendObjType.FramesCons, BackendObjType.FramesCons.genByteCode()))
    val framesNilClass = Map(genClass(BackendObjType.FramesNil, BackendObjType.FramesNil.genByteCode()))
    val resumptionInterface = Map(genClass(BackendObjType.Resumption, BackendObjType.Resumption.genByteCode()))
    val resumptionConsClass = Map(genClass(BackendObjType.ResumptionCons, BackendObjType.ResumptionCons.genByteCode()))
    val resumptionNilClass = Map(genClass(BackendObjType.ResumptionNil, BackendObjType.ResumptionNil.genByteCode()))
    val handlerInterface = Map(genClass(BackendObjType.Handler, BackendObjType.Handler.genByteCode()))
    val effectCallClass = Map(genClass(BackendObjType.EffectCall, BackendObjType.EffectCall.genByteCode()))
    val effectClasses = GenEffectClasses.gen(root.effects.values)
    val resumptionWrappers = BackendType.erasedTypes.map(BackendObjType.ResumptionWrapper.apply).map(bt => genClass(bt, bt.genByteCode())).toMap

    val allClasses = List(
      mainClass,
      namespaceClasses,
      functionInterfaces,
      functionAndClosureClasses,
      closureAbstractClasses,
      taggedAbstractClass,
      tagClasses,
      extensibleTagClasses,
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

  /** Unpacks `g` into a name and a class with `code`. */
  private def genClass(g: BackendObjType, code: Array[Byte]): (JvmName, JvmClass) =
    (g.jvmName, JvmClass(g.jvmName, code))

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

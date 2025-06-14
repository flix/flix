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
import ca.uwaterloo.flix.language.ast.MonoType
import ca.uwaterloo.flix.language.ast.ReducedAst.*
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugNoOp

object JvmBackend {

  /** Emits JVM bytecode for `root`. */
  def run(root: Root)(implicit flix: Flix): (Root, List[JvmClass]) = flix.phase("JvmBackend") {
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
      main => JvmClass(BackendObjType.Main.jvmName, BackendObjType.Main.genByteCode(main.sym))
    ).toList

    val namespaceClasses = JvmOps.namespacesOf(root).map(
      ns => {
        val nsClass = BackendObjType.Namespace(ns.ns)
        val entrypointDefs = ns.defs.values.toList.filter(defn => root.entryPoints.contains(defn.sym))
        JvmClass(nsClass.jvmName, nsClass.genByteCode(entrypointDefs))
      }).toList

    // Generate function classes.
    val functionAndClosureClasses = GenFunAndClosureClasses.gen(root.defs).values.toList
    val erasedFunctionTypes = JvmOps.getErasedArrowsOf(allTypes)
    val functionInterfaces = erasedFunctionTypes.map(bt => JvmClass(bt.jvmName, bt.genByteCode()))
    val closureAbstractClasses = erasedFunctionTypes.map {
      case BackendObjType.Arrow(args, result) => BackendObjType.AbstractArrow(args, result)
    }.map(bt => JvmClass(bt.jvmName, bt.genByteCode())).toList

    val taggedAbstractClass = List(JvmClass(BackendObjType.Tagged.jvmName, BackendObjType.Tagged.genByteCode()))
    val tagClasses = JvmOps.getErasedTagTypesOf(allTypes).map(bt => JvmClass(bt.jvmName, bt.genByteCode())).toList
    val extensibleTagClasses = JvmOps.getExtensibleTagTypesOf(allTypes).map(bt => JvmClass(bt.jvmName, bt.genByteCode())).toList

    val tupleClasses = JvmOps.getTupleTypesOf(allTypes).map(bt => JvmClass(bt.jvmName, bt.genByteCode())).toList
    val structClasses = JvmOps.getErasedStructTypesOf(root, allTypes).map(bt => JvmClass(bt.jvmName, bt.genByteCode())).toList

    val recordInterfaces = List(JvmClass(BackendObjType.Record.jvmName, BackendObjType.Record.genByteCode()))
    val recordEmptyClasses = List(JvmClass(BackendObjType.RecordEmpty.jvmName, BackendObjType.RecordEmpty.genByteCode()))
    val recordExtendClasses = JvmOps.getRecordExtendsOf(allTypes).map(bt => JvmClass(bt.jvmName, bt.genByteCode())).toList

    val lazyClasses = JvmOps.getLazyTypesOf(allTypes).map(bt => JvmClass(bt.jvmName, bt.genByteCode())).toList

    val anonClasses = GenAnonymousClasses.gen(root.anonClasses)

    val unitClass = List(JvmClass(BackendObjType.Unit.jvmName, BackendObjType.Unit.genByteCode()))

    val flixErrorClass = List(JvmClass(JvmName.FlixError, ClassConstants.FlixError.genByteCode()))
    val rslClass = List(JvmClass(BackendObjType.ReifiedSourceLocation.jvmName, BackendObjType.ReifiedSourceLocation.genByteCode()))
    val holeErrorClass = List(JvmClass(BackendObjType.HoleError.jvmName, BackendObjType.HoleError.genByteCode()))
    val matchErrorClass = List(JvmClass(BackendObjType.MatchError.jvmName, BackendObjType.MatchError.genByteCode()))
    val castErrorClass = List(JvmClass(BackendObjType.CastError.jvmName, BackendObjType.CastError.genByteCode()))
    val unhandledEffectErrorClass = List(JvmClass(BackendObjType.UnhandledEffectError.jvmName, BackendObjType.UnhandledEffectError.genByteCode()))

    val globalClass = List(JvmClass(BackendObjType.Global.jvmName, BackendObjType.Global.genByteCode()))

    val regionClass = List(JvmClass(BackendObjType.Region.jvmName, BackendObjType.Region.genByteCode()))

    val uncaughtExceptionHandlerClass = List(JvmClass(BackendObjType.UncaughtExceptionHandler.jvmName, BackendObjType.UncaughtExceptionHandler.genByteCode()))

    // Effect runtime classes.
    val resultInterface = List(JvmClass(BackendObjType.Result.jvmName, BackendObjType.Result.genByteCode()))
    val valueClass = List(JvmClass(BackendObjType.Value.jvmName, BackendObjType.Value.genByteCode()))
    val frameInterface = List(JvmClass(BackendObjType.Frame.jvmName, BackendObjType.Frame.genByteCode()))
    val thunkAbstractClass = List(JvmClass(BackendObjType.Thunk.jvmName, BackendObjType.Thunk.genByteCode()))
    val suspensionClass = List(JvmClass(BackendObjType.Suspension.jvmName, BackendObjType.Suspension.genByteCode()))
    val framesInterface = List(JvmClass(BackendObjType.Frames.jvmName, BackendObjType.Frames.genByteCode()))
    val framesConsClass = List(JvmClass(BackendObjType.FramesCons.jvmName, BackendObjType.FramesCons.genByteCode()))
    val framesNilClass = List(JvmClass(BackendObjType.FramesNil.jvmName, BackendObjType.FramesNil.genByteCode()))
    val resumptionInterface = List(JvmClass(BackendObjType.Resumption.jvmName, BackendObjType.Resumption.genByteCode()))
    val resumptionConsClass = List(JvmClass(BackendObjType.ResumptionCons.jvmName, BackendObjType.ResumptionCons.genByteCode()))
    val resumptionNilClass = List(JvmClass(BackendObjType.ResumptionNil.jvmName, BackendObjType.ResumptionNil.genByteCode()))
    val handlerInterface = List(JvmClass(BackendObjType.Handler.jvmName, BackendObjType.Handler.genByteCode()))
    val effectCallClass = List(JvmClass(BackendObjType.EffectCall.jvmName, BackendObjType.EffectCall.genByteCode()))
    val effectClasses = GenEffectClasses.gen(root.effects.values)
    val resumptionWrappers = BackendType.erasedTypes.map(BackendObjType.ResumptionWrapper.apply).map(bt => JvmClass(bt.jvmName, bt.genByteCode()))

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
    ).flatten

    (root, allClasses)
  }(DebugNoOp())

}

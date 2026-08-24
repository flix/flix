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
import ca.uwaterloo.flix.language.ast.{BytecodeAst, SimpleType, SourceLocation}
import ca.uwaterloo.flix.language.ast.JvmAst.*
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugNoOp
import ca.uwaterloo.flix.util.{ClassDescs, InternalCompilerException}
import ca.uwaterloo.flix.util.collection.MapOps


object CodeGen {

  /** Emits JVM bytecode for `root`. */
  def run(root: Root)(implicit flix: Flix): BytecodeAst.Root = flix.phase("CodeGen") {
    implicit val r: Root = root

    // Types/classes required for Flix runtime.
    val requiredTypes = Set(
      SimpleType.Arrow(List(SimpleType.Bool), SimpleType.Object), // by resumptionWrappers
      SimpleType.Arrow(List(SimpleType.Char), SimpleType.Object), // by resumptionWrappers
      SimpleType.Arrow(List(SimpleType.Int8), SimpleType.Object), // by resumptionWrappers
      SimpleType.Arrow(List(SimpleType.Int16), SimpleType.Object), // by resumptionWrappers
      SimpleType.Arrow(List(SimpleType.Int32), SimpleType.Object), // by resumptionWrappers
      SimpleType.Arrow(List(SimpleType.Int64), SimpleType.Object), // by resumptionWrappers
      SimpleType.Arrow(List(SimpleType.Float32), SimpleType.Object), // by resumptionWrappers
      SimpleType.Arrow(List(SimpleType.Float64), SimpleType.Object), // by resumptionWrappers
      SimpleType.Arrow(List(SimpleType.Object), SimpleType.Object), // by resumptionWrappers
    )
    val allTypes = root.types ++ requiredTypes

    val mainClass = root.getMain.map(
      main => JvmClass(BackendObjType.Main.desc, BackendObjType.Main.genByteCode(main.sym))
    ).toList

    val namespaceClasses = JvmOps.namespacesOf(root).map(
      ns => {
        val nsClass = BackendObjType.Namespace(ns.ns)
        val entrypointDefs = ns.defs.values.toList.filter(defn => root.entryPoints.contains(defn.sym))
        JvmClass(nsClass.desc, nsClass.genByteCode(entrypointDefs))
      }).toList

    // Generate function classes.
    val functionAndClosureClasses = GenFunAndClosureClasses.gen(root.defs).values.toList
    val erasedFunctionTypes = JvmOps.getErasedArrowsOf(allTypes)
    val functionInterfaces = erasedFunctionTypes.map(bt => JvmClass(bt.desc, bt.genByteCode()))
    val closureAbstractClasses = erasedFunctionTypes.map {
      case BackendObjType.Arrow(args, result) => BackendObjType.AbstractArrow(args, result)
    }.map(bt => JvmClass(bt.desc, bt.genByteCode())).toList

    val taggedAbstractClass = List(JvmClass(BackendObjType.Tagged.desc, BackendObjType.Tagged.genByteCode()))
    val tagClasses = root.enums.values.flatMap(JvmOps.getTagsOf).toList.distinctBy(_.desc).map(bt => JvmClass(bt.desc, bt.genByteCode()))
    val extTaggedAbstractClass = List(JvmClass(BackendObjType.ExtTagged.desc, BackendObjType.ExtTagged.genByteCode()))
    val extensibleTagClasses = JvmOps.getExtensibleTagTypesOf(allTypes).map(bt => JvmClass(bt.desc, bt.genByteCode())).toList

    val tupleClasses = JvmOps.getTupleTypesOf(allTypes).map(bt => JvmClass(bt.desc, bt.genByteCode())).toList
    val structClasses = root.structs.values.map(JvmOps.getStructType).toList.distinctBy(_.desc).map(bt => JvmClass(bt.desc, bt.genByteCode()))

    val recordInterfaces = List(JvmClass(BackendObjType.Record.desc, BackendObjType.Record.genByteCode()))
    val recordEmptyClasses = List(JvmClass(BackendObjType.RecordEmpty.desc, BackendObjType.RecordEmpty.genByteCode()))
    val recordExtendClasses = JvmOps.getRecordExtendsOf(allTypes).map(bt => JvmClass(bt.desc, bt.genByteCode())).toList

    val lazyClasses = JvmOps.getLazyTypesOf(allTypes).map(bt => JvmClass(bt.desc, bt.genByteCode())).toList

    val anonClasses = GenAnonymousClasses.gen(root.anonClasses.distinctBy(_.name))

    val unitClass = List(JvmClass(BackendObjType.Unit.desc, BackendObjType.Unit.genByteCode()))

    val flixErrorClass = List(JvmClass(ClassConstants.FlixError.Desc, ClassConstants.FlixError.genByteCode()))
    val rslClass = List(JvmClass(BackendObjType.ReifiedSourceLocation.desc, BackendObjType.ReifiedSourceLocation.genByteCode()))
    val holeErrorClass = List(JvmClass(BackendObjType.HoleError.desc, BackendObjType.HoleError.genByteCode()))
    val matchErrorClass = List(JvmClass(BackendObjType.MatchError.desc, BackendObjType.MatchError.genByteCode()))
    val castErrorClass = List(JvmClass(BackendObjType.CastError.desc, BackendObjType.CastError.genByteCode()))
    val unhandledEffectErrorClass = List(JvmClass(BackendObjType.UnhandledEffectError.desc, BackendObjType.UnhandledEffectError.genByteCode()))

    val globalClass = List(JvmClass(BackendObjType.Global.desc, BackendObjType.Global.genByteCode()))

    val regionClass = List(JvmClass(BackendObjType.Region.desc, BackendObjType.Region.genByteCode()))

    val uncaughtExceptionHandlerClass = List(JvmClass(BackendObjType.UncaughtExceptionHandler.desc, BackendObjType.UncaughtExceptionHandler.genByteCode()))

    // Effect runtime classes.
    val resultInterface = List(JvmClass(BackendObjType.Result.desc, BackendObjType.Result.genByteCode()))
    val valueClass = List(JvmClass(BackendObjType.Value.desc, BackendObjType.Value.genByteCode()))
    val frameInterface = List(JvmClass(BackendObjType.Frame.desc, BackendObjType.Frame.genByteCode()))
    val thunkAbstractClass = List(JvmClass(BackendObjType.Thunk.desc, BackendObjType.Thunk.genByteCode()))
    val suspensionClass = List(JvmClass(BackendObjType.Suspension.desc, BackendObjType.Suspension.genByteCode()))
    val framesInterface = List(JvmClass(BackendObjType.Frames.desc, BackendObjType.Frames.genByteCode()))
    val framesConsClass = List(JvmClass(BackendObjType.FramesCons.desc, BackendObjType.FramesCons.genByteCode()))
    val framesNilClass = List(JvmClass(BackendObjType.FramesNil.desc, BackendObjType.FramesNil.genByteCode()))
    val resumptionInterface = List(JvmClass(BackendObjType.Resumption.desc, BackendObjType.Resumption.genByteCode()))
    val resumptionConsClass = List(JvmClass(BackendObjType.ResumptionCons.desc, BackendObjType.ResumptionCons.genByteCode()))
    val resumptionNilClass = List(JvmClass(BackendObjType.ResumptionNil.desc, BackendObjType.ResumptionNil.genByteCode()))
    val handlerInterface = List(JvmClass(BackendObjType.Handler.desc, BackendObjType.Handler.genByteCode()))
    val effectCallClass = List(JvmClass(BackendObjType.EffectCall.desc, BackendObjType.EffectCall.genByteCode()))
    val effectClasses = GenEffectClasses.gen(root.effects.values)
    val resumptionWrappers = BackendType.erasedTypes.map(BackendObjType.ResumptionWrapper.apply).map(bt => JvmClass(bt.desc, bt.genByteCode()))

    val allClasses = List(
      mainClass,
      namespaceClasses,
      functionInterfaces,
      functionAndClosureClasses,
      closureAbstractClasses,
      taggedAbstractClass,
      tagClasses,
      extTaggedAbstractClass,
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

    // Check for duplicate JVM class names.
    val duplicates = allClasses.groupBy(_.name).collect { case (name, classes) if classes.length > 1 => name }
    if (duplicates.nonEmpty) {
      val names = duplicates.map(ClassDescs.internalNameOf).mkString(", ")
      throw InternalCompilerException(s"Duplicate JVM class names: $names", SourceLocation.Unknown)
    }

    val classMap = allClasses.map(clazz => clazz.name -> clazz).toMap

    val tests = MapOps.mapValues(root.defs.filter(_._2.ann.isTest)) {
      case defn =>
        val nsType = BackendObjType.Namespace(defn.sym.namespace)
        BytecodeAst.Test(nsType.desc, nsType.ShimMethod(defn).name, defn.ann.isSkip)
    }
    val main = root.mainEntryPoint.map{
      case _ =>
        val mainType = BackendObjType.Main
        BytecodeAst.Def(mainType.desc, mainType.MainMethod.name)
    }
    BytecodeAst.Root(classMap, tests, main, root.sources)
  }(DebugNoOp())

}

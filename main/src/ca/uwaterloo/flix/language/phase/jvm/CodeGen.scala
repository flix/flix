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
import ca.uwaterloo.flix.language.ast.{BytecodeAst, SimpleType, SourceLocation, Symbol}
import ca.uwaterloo.flix.language.ast.JvmAst.*
import ca.uwaterloo.flix.language.dbg.AstPrinter.DebugNoOp
import ca.uwaterloo.flix.language.phase.jvm.classes.{GenAbstractArrow, GenArrow, GenCastError, GenEffectCall, GenExtTag, GenExtTagged, GenFrame, GenFrames, GenFramesCons, GenFramesNil, GenGlobal, GenHandler, GenHoleError, GenLazy, GenMain, GenMatchError, GenNamespace, GenNullaryTag, GenRecord, GenRecordEmpty, GenRecordExtend, GenRegion, GenReifiedSourceLocation, GenResult, GenResumption, GenResumptionCons, GenResumptionNil, GenResumptionWrapper, GenStruct, GenSuspension, GenTag, GenTagged, GenThunk, GenTuple, GenUncaughtExceptionHandler, GenUnhandledEffectError, GenUnit, GenValue}
import ca.uwaterloo.flix.util.{ClassDescs, InternalCompilerException}

import java.lang.constant.ClassDesc
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
      main => JvmClass(GenMain.Desc, GenMain.genByteCode(main.sym))
    ).toList

    val namespaceClasses = namespacesOf(root).map {
      case (ns, defs) =>
        val entrypointDefs = defs.values.toList.filter(defn => root.entryPoints.contains(defn.sym))
        JvmClass(GenNamespace.desc(ns), GenNamespace.genByteCode(ns, entrypointDefs))
    }.toList

    // Generate function classes.
    val functionAndClosureClasses = GenFunAndClosureClasses.gen(root.defs).values.toList
    val erasedFunctionTypes = getErasedArrowsOf(allTypes)
    val functionInterfaces = erasedFunctionTypes.map { case (args, result) => JvmClass(GenArrow.desc(args, result), GenArrow.genByteCode(args, result)) }
    val closureAbstractClasses = erasedFunctionTypes.map {
      case (args, result) => JvmClass(GenAbstractArrow.desc(args, result), GenAbstractArrow.genByteCode(args, result))
    }.toList

    val taggedAbstractClass = List(JvmClass(GenTagged.Desc, GenTagged.genByteCode()))
    val nullaryTagClasses = root.enums.values.flatMap(getNullaryTagsOf).toList.map { caze =>
      val enumName = caze.sym.enumSym.toString
      JvmClass(GenNullaryTag.desc(enumName, caze.sym.name), GenNullaryTag.genByteCode(enumName, caze.sym.name, caze.sym.ordinal))
    }
    val tagClasses = root.enums.values.flatMap(getTagsOf).toSet[List[ClassDesc]].toList.map(elms => JvmClass(GenTag.desc(elms), GenTag.genByteCode(elms)))
    val extTaggedAbstractClass = List(JvmClass(GenExtTagged.Desc, GenExtTagged.genByteCode()))
    val extensibleTagClasses = getExtensibleTagTypesOf(allTypes).map(elms => JvmClass(GenExtTag.desc(elms), GenExtTag.genByteCode(elms))).toList

    val tupleClasses = getTupleTypesOf(allTypes).map(elms => JvmClass(GenTuple.desc(elms), GenTuple.genByteCode(elms))).toList
    val structClasses = root.structs.values.map(TypeDescs.structFields).toSet[List[ClassDesc]].toList.map(elms => JvmClass(GenStruct.desc(elms), GenStruct.genByteCode(elms)))

    val recordInterfaces = List(JvmClass(GenRecord.Desc, GenRecord.genByteCode()))
    val recordEmptyClasses = List(JvmClass(GenRecordEmpty.Desc, GenRecordEmpty.genByteCode()))
    val recordExtendClasses = getRecordExtendsOf(allTypes).map(value => JvmClass(GenRecordExtend.desc(value), GenRecordExtend.genByteCode(value))).toList

    val lazyClasses = getLazyTypesOf(allTypes).map(tpe => JvmClass(GenLazy.desc(tpe), GenLazy.genByteCode(tpe))).toList

    val anonClasses = GenAnonymousClasses.gen(root.anonClasses.distinctBy(_.name))

    val unitClass = List(JvmClass(GenUnit.Desc, GenUnit.genByteCode()))

    val flixErrorClass = List(JvmClass(ClassConstants.FlixError.Desc, ClassConstants.FlixError.genByteCode()))
    val rslClass = List(JvmClass(GenReifiedSourceLocation.Desc, GenReifiedSourceLocation.genByteCode()))
    val holeErrorClass = List(JvmClass(GenHoleError.Desc, GenHoleError.genByteCode()))
    val matchErrorClass = List(JvmClass(GenMatchError.Desc, GenMatchError.genByteCode()))
    val castErrorClass = List(JvmClass(GenCastError.Desc, GenCastError.genByteCode()))
    val unhandledEffectErrorClass = List(JvmClass(GenUnhandledEffectError.Desc, GenUnhandledEffectError.genByteCode()))

    val globalClass = List(JvmClass(GenGlobal.Desc, GenGlobal.genByteCode()))

    val regionClass = List(JvmClass(GenRegion.Desc, GenRegion.genByteCode()))

    val uncaughtExceptionHandlerClass = List(JvmClass(GenUncaughtExceptionHandler.Desc, GenUncaughtExceptionHandler.genByteCode()))

    // Effect runtime classes.
    val resultInterface = List(JvmClass(GenResult.Desc, GenResult.genByteCode()))
    val valueClass = List(JvmClass(GenValue.Desc, GenValue.genByteCode()))
    val frameInterface = List(JvmClass(GenFrame.Desc, GenFrame.genByteCode()))
    val thunkAbstractClass = List(JvmClass(GenThunk.Desc, GenThunk.genByteCode()))
    val suspensionClass = List(JvmClass(GenSuspension.Desc, GenSuspension.genByteCode()))
    val framesInterface = List(JvmClass(GenFrames.Desc, GenFrames.genByteCode()))
    val framesConsClass = List(JvmClass(GenFramesCons.Desc, GenFramesCons.genByteCode()))
    val framesNilClass = List(JvmClass(GenFramesNil.Desc, GenFramesNil.genByteCode()))
    val resumptionInterface = List(JvmClass(GenResumption.Desc, GenResumption.genByteCode()))
    val resumptionConsClass = List(JvmClass(GenResumptionCons.Desc, GenResumptionCons.genByteCode()))
    val resumptionNilClass = List(JvmClass(GenResumptionNil.Desc, GenResumptionNil.genByteCode()))
    val handlerInterface = List(JvmClass(GenHandler.Desc, GenHandler.genByteCode()))
    val effectCallClass = List(JvmClass(GenEffectCall.Desc, GenEffectCall.genByteCode()))
    val effectClasses = GenEffectClasses.gen(root.effects.values)
    val resumptionWrappers = TypeDescs.erasedTypes.map(tpe => JvmClass(GenResumptionWrapper.desc(tpe), GenResumptionWrapper.genByteCode(tpe)))

    val allClasses = List(
      mainClass,
      namespaceClasses,
      functionInterfaces,
      functionAndClosureClasses,
      closureAbstractClasses,
      taggedAbstractClass,
      nullaryTagClasses,
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
        val ns = defn.sym.namespace
        BytecodeAst.Test(GenNamespace.desc(ns), GenNamespace.ShimMethod(ns, defn).name, defn.ann.isSkip)
    }
    val main = root.mainEntryPoint.map{
      case _ =>
        BytecodeAst.Def(GenMain.Desc, GenMain.MainMethod.name)
    }
    BytecodeAst.Root(classMap, tests, main, root.sources)
  }(DebugNoOp())

  /** Returns the defs of each namespace in the given AST `root`. */
  private def namespacesOf(root: Root): Map[List[String], Map[Symbol.DefnSym, Def]] =
    root.defs.groupBy(_._1.namespace)

  /** Returns the set of erased function types in `types` without searching recursively. */
  private def getErasedArrowsOf(types: Iterable[SimpleType]): Set[(List[ClassDesc], ClassDesc)] =
    types.foldLeft(Set.empty[(List[ClassDesc], ClassDesc)]) {
      case (acc, SimpleType.Arrow(args, result)) =>
        acc + ((args.map(TypeDescs.toErasedClassDesc), TypeDescs.toErasedClassDesc(result)))
      case (acc, _) => acc
    }

  /** Returns the nullary cases of `enm`, which each get their own singleton class. */
  private def getNullaryTagsOf(enm: Enum): Iterable[Case] =
    enm.cases.values.filter(_.tpes.isEmpty)

  /** Returns the erased term types of each non-nullary case in `enm`. */
  private def getTagsOf(enm: Enum): Set[List[ClassDesc]] =
    enm.cases.values.collect {
      case caze if caze.tpes.nonEmpty => caze.tpes.map(TypeDescs.toErasedClassDesc)
    }.toSet

  /** Returns the set of extensible tag types in `types` without searching recursively. */
  private def getExtensibleTagTypesOf(types: Iterable[SimpleType]): Set[List[ClassDesc]] =
    types.foldLeft(Set.empty[List[ClassDesc]]) {
      case (acc, SimpleType.ExtensibleExtend(_, targs, _)) =>
        acc + targs.map(TypeDescs.toErasedClassDesc)
      case (acc, _) => acc
    }

  /** Returns the set of tuple types in `types` without searching recursively. */
  private def getTupleTypesOf(types: Iterable[SimpleType]): Set[List[ClassDesc]] =
    types.foldLeft(Set.empty[List[ClassDesc]]) {
      case (acc, SimpleType.Tuple(elms)) =>
        acc + elms.map(TypeDescs.toErasedClassDesc)
      case (acc, _) => acc
    }

  /** Returns the set of record extend types in `types` without searching recursively. */
  private def getRecordExtendsOf(types: Iterable[SimpleType]): Set[ClassDesc] =
    types.foldLeft(Set.empty[ClassDesc]) {
      case (acc, SimpleType.RecordExtend(_, value, _)) =>
        acc + TypeDescs.toErasedClassDesc(value)
      case (acc, _) => acc
    }

  /** Returns the set of lazy types in `types` without searching recursively. */
  private def getLazyTypesOf(types: Iterable[SimpleType]): Set[ClassDesc] =
    types.foldLeft(Set.empty[ClassDesc]) {
      case (acc, SimpleType.Lazy(tpe)) => acc + TypeDescs.toErasedClassDesc(tpe)
      case (acc, _) => acc
    }

}

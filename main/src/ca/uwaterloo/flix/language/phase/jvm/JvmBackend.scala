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
import ca.uwaterloo.flix.util.ParOps

import scala.collection.mutable

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
    val erasedFunctionTypes = JvmOps.getErasedArrowsOf(allTypes)

    // Collect a list of bytecode generation jobs to do concurrently.
    val tasks = mutable.ArrayBuffer.empty[Unit => JvmClass]

    def addTask(f: => JvmClass): Unit = tasks.append(_ => f)

    for (main <- root.getMain) addTask(JvmClass(BackendObjType.Main.jvmName, BackendObjType.Main.genByteCode(main.sym)))
    for (ns <- JvmOps.namespacesOf(root)) addTask {
      val nsClass = BackendObjType.Namespace(ns.ns)
      val entrypointDefs = ns.defs.values.toList.filter(defn => root.entryPoints.contains(defn.sym))
      JvmClass(nsClass.jvmName, nsClass.genByteCode(entrypointDefs))
    }
    for (bt <- erasedFunctionTypes) {
      addTask(JvmClass(bt.jvmName, bt.genByteCode()))
      addTask {
        val arrowType = BackendObjType.AbstractArrow(bt.args, bt.result)
        JvmClass(arrowType.jvmName, arrowType.genByteCode())
      }
    }
    addTask(JvmClass(BackendObjType.Tagged.jvmName, BackendObjType.Tagged.genByteCode()))
    for (bt <- JvmOps.getErasedTagTypesOf(allTypes)) addTask(JvmClass(bt.jvmName, bt.genByteCode()))
    for (bt <- JvmOps.getExtensibleTagTypesOf(allTypes)) addTask(JvmClass(bt.jvmName, bt.genByteCode()))
    for (bt <- JvmOps.getTupleTypesOf(allTypes)) addTask(JvmClass(bt.jvmName, bt.genByteCode()))
    for (bt <- JvmOps.getErasedStructTypesOf(root, allTypes)) addTask(JvmClass(bt.jvmName, bt.genByteCode()))
    addTask(JvmClass(BackendObjType.Record.jvmName, BackendObjType.Record.genByteCode()))
    addTask(JvmClass(BackendObjType.RecordEmpty.jvmName, BackendObjType.RecordEmpty.genByteCode()))
    for (bt <- JvmOps.getRecordExtendsOf(allTypes)) addTask(JvmClass(bt.jvmName, bt.genByteCode()))
    for (bt <- JvmOps.getLazyTypesOf(allTypes)) addTask(JvmClass(bt.jvmName, bt.genByteCode()))
    addTask(JvmClass(BackendObjType.Unit.jvmName, BackendObjType.Unit.genByteCode()))
    addTask(JvmClass(BackendObjType.FlixError.jvmName, BackendObjType.FlixError.genByteCode()))
    addTask(JvmClass(BackendObjType.ReifiedSourceLocation.jvmName, BackendObjType.ReifiedSourceLocation.genByteCode()))
    addTask(JvmClass(BackendObjType.HoleError.jvmName, BackendObjType.HoleError.genByteCode()))
    addTask(JvmClass(BackendObjType.MatchError.jvmName, BackendObjType.MatchError.genByteCode()))
    addTask(JvmClass(BackendObjType.CastError.jvmName, BackendObjType.CastError.genByteCode()))
    addTask(JvmClass(BackendObjType.UnhandledEffectError.jvmName, BackendObjType.UnhandledEffectError.genByteCode()))
    addTask(JvmClass(BackendObjType.Global.jvmName, BackendObjType.Global.genByteCode()))
    addTask(JvmClass(BackendObjType.Region.jvmName, BackendObjType.Region.genByteCode()))
    addTask(JvmClass(BackendObjType.UncaughtExceptionHandler.jvmName, BackendObjType.UncaughtExceptionHandler.genByteCode()))
    addTask(JvmClass(BackendObjType.Result.jvmName, BackendObjType.Result.genByteCode()))
    addTask(JvmClass(BackendObjType.Value.jvmName, BackendObjType.Value.genByteCode()))
    addTask(JvmClass(BackendObjType.Frame.jvmName, BackendObjType.Frame.genByteCode()))
    addTask(JvmClass(BackendObjType.Thunk.jvmName, BackendObjType.Thunk.genByteCode()))
    addTask(JvmClass(BackendObjType.Suspension.jvmName, BackendObjType.Suspension.genByteCode()))
    addTask(JvmClass(BackendObjType.Frames.jvmName, BackendObjType.Frames.genByteCode()))
    addTask(JvmClass(BackendObjType.FramesCons.jvmName, BackendObjType.FramesCons.genByteCode()))
    addTask(JvmClass(BackendObjType.FramesNil.jvmName, BackendObjType.FramesNil.genByteCode()))
    addTask(JvmClass(BackendObjType.Resumption.jvmName, BackendObjType.Resumption.genByteCode()))
    addTask(JvmClass(BackendObjType.ResumptionCons.jvmName, BackendObjType.ResumptionCons.genByteCode()))
    addTask(JvmClass(BackendObjType.ResumptionNil.jvmName, BackendObjType.ResumptionNil.genByteCode()))
    addTask(JvmClass(BackendObjType.Handler.jvmName, BackendObjType.Handler.genByteCode()))
    addTask(JvmClass(BackendObjType.EffectCall.jvmName, BackendObjType.EffectCall.genByteCode()))
    for (bt <- BackendType.erasedTypes) addTask {
      val wrapper = BackendObjType.ResumptionWrapper(bt)
      JvmClass(wrapper.jvmName, wrapper.genByteCode())
    }

    // Generate the classes in parallel.
    val singleClasses = ParOps.parMap(tasks) { task => task(()) }.toList

    // Add the remaining classes not written to be split.
    val functionAndClosureClasses = GenFunAndClosureClasses.gen(root.defs).values.toList
    val anonClasses = GenAnonymousClasses.gen(root.anonClasses).values.toList
    val effectClasses = GenEffectClasses.gen(root.effects.values).values.toList

    val allClasses = List(singleClasses, functionAndClosureClasses, anonClasses, effectClasses).flatten

    (root, allClasses)
  }(DebugNoOp())

}

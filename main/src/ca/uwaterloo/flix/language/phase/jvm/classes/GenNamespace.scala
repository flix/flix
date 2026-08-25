/*
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

package ca.uwaterloo.flix.language.phase.jvm.classes

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.JvmAst
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.IsFinal
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.IsPublic
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{ConstructorMethod, InstanceField, StaticMethod}
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.Mangle.mkDesc
import ca.uwaterloo.flix.language.phase.jvm.MethodTypeDescs.mkDescriptor
import ca.uwaterloo.flix.language.phase.jvm.{BackendObjType, ClassConstants, ClassMaker, GenFunAndClosureClasses, Mangle, TypeDescs}
import org.objectweb.asm.MethodVisitor

import java.lang.constant.ClassDesc

/**
  * The namespace class of a Flix module, which holds the shim methods of the module's
  * entry points and tests.
  */
object GenNamespace {

  def desc(ns: List[String]): ClassDesc =
    mkDesc(ns.dropRight(1), ns.lastOption.getOrElse(s"Root${Flix.Delimiter}"))

  def genByteCode(ns: List[String], defs: List[JvmAst.Def])(implicit flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkClass(desc(ns), IsFinal)

    cm.mkConstructor(Constructor(ns), IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))

    for (defn <- defs) {
      cm.mkStaticMethod(ShimMethod(ns, defn), IsPublic, IsFinal, shimIns(defn)(_))
    }

    cm.closeClassMaker()
  }

  def Constructor(ns: List[String]): ConstructorMethod = ConstructorMethod(desc(ns), Nil)

  def ShimMethod(ns: List[String], defn: JvmAst.Def): StaticMethod = {
    val erasedArgs = defn.fparams.map(_.tpe).map(TypeDescs.toErasedClassDesc)
    val erasedResult = TypeDescs.toErasedClassDesc(defn.unboxedType.tpe)
    // Exported names are checked in Safety, so no mangling is needed.
    val name = if (defn.ann.isExport) defn.sym.name else "m_" + Mangle.mangle(defn.sym.name)
    StaticMethod(desc(ns), name, mkDescriptor(erasedArgs *)(erasedResult))
  }

  private def shimIns(defn: JvmAst.Def)(implicit mv: MethodVisitor): Unit = {
    val defnDesc = GenFunAndClosureClasses.defnDesc(defn.sym)
    val paramTypes = defn.fparams.map(fp => TypeDescs.toErasedClassDesc(fp.tpe))
    withNames(0, paramTypes) {
      case (_, args) =>
        val erasedResult = TypeDescs.toErasedClassDesc(defn.unboxedType.tpe)
        NEW(defnDesc)
        DUP()
        INVOKESPECIAL(ConstructorMethod(defnDesc, Nil))
        for ((arg, index) <- args.zipWithIndex) {
          DUP()
          arg.load()
          PUTFIELD(InstanceField(defnDesc, s"arg$index", paramTypes(index)))
        }
        GenResult.unwindSuspensionFreeThunkToType(erasedResult, s"in shim method of ${defn.sym}", defn.loc)
        xReturn(erasedResult)
    }
  }

}

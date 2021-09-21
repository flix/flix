/*
 * Copyright 2020-2021 Jonathan Lindegaard Starup
 * Copyright 2017 Magnus Madsen
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

// TODO(JLS): check copyright in all files

package ca.uwaterloo.flix.language.phase.sjvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ErasedAst.Root
import ca.uwaterloo.flix.language.ast.PRefType._
import ca.uwaterloo.flix.language.ast.PType._
import ca.uwaterloo.flix.language.ast.RRefType._
import ca.uwaterloo.flix.language.ast.RType._
import ca.uwaterloo.flix.language.ast.{PType, RType}
import ca.uwaterloo.flix.language.phase.sjvm.BytecodeCompiler._
import ca.uwaterloo.flix.language.phase.sjvm.ClassMaker.Mod
import ca.uwaterloo.flix.language.phase.sjvm.Instructions._
import ca.uwaterloo.flix.util.ParOps

object GenTupleClasses {

  def indexFieldName(index: Int): String = s"index$index"

  def gen(tuples: Set[RType[PReference[PTuple]]])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    // TODO(JLS): check parops for all gens
    ParOps.parAgg(tuples, Map[JvmName, JvmClass]())({
      case (macc, tpe) =>
        val tupleType = squeezeTuple(squeezeReference(tpe))
        macc + (tupleType.jvmName -> JvmClass(tupleType.jvmName, genByteCode(tupleType.jvmName, tupleType)))
    }, _ ++ _)
  }

  private def genByteCode[T <: PType](className: JvmName, tupleType: RTuple)(implicit root: Root, flix: Flix): Array[Byte] = {
    val classMaker = ClassMaker.mkClass(className, JvmName.Java.Object)
    classMaker.mkConstructor(START[StackNil] ~ THISINIT(JvmName.Java.Object) ~ RETURN)
    tupleType.elms.zipWithIndex.foreach {
      case (indexType, index) => classMaker.mkField(indexFieldName(index), indexType.erasedType, Mod.isPublic)
    }
    // TODO(JLS): does not seem to be used
    //classMaker.mkMethod(genBoxedArrayFunction(tupleType), GetBoxedArrayMethodName, GetBoxedArrayMethodDescriptor, Mod.isPublic)
    classMaker.closeClassMaker
  }

  def genBoxedArrayFunction[T <: PType](tupleType: RTuple): F[StackNil] => F[StackEnd] = {
    val refTuple = RReference(tupleType)
    START[StackNil] ~
      pushInt32(tupleType.elms.size) ~
      ANEWARRAY(RReference(RObject)) ~
      multiComposition(tupleType.elms.zipWithIndex) {
        case (indexType, index) =>
          START[StackNil ** PReference[PArray[PReference[PAnyObject]]]] ~
            DUP ~
            pushInt32(index) ~
            (indexType match {
              case RBool => START[StackNil ** PReference[PArray[PReference[PAnyObject]]] ** PReference[PArray[PReference[PAnyObject]]] ** PInt32] ~ THISLOAD(refTuple) ~ GETFIELD(RReference(tupleType), indexFieldName(index), RInt32, undoErasure = false) ~ BoxBool ~ SUBTYPE ~ AASTORE
              case RInt8 => START[StackNil ** PReference[PArray[PReference[PAnyObject]]] ** PReference[PArray[PReference[PAnyObject]]] ** PInt32] ~ THISLOAD(refTuple) ~ GETFIELD(RReference(tupleType), indexFieldName(index), RInt8, undoErasure = false) ~ BoxInt8 ~ SUBTYPE ~ AASTORE
              case RInt16 => START[StackNil ** PReference[PArray[PReference[PAnyObject]]] ** PReference[PArray[PReference[PAnyObject]]] ** PInt32] ~ THISLOAD(refTuple) ~ GETFIELD(RReference(tupleType), indexFieldName(index), RInt16, undoErasure = false) ~ BoxInt16 ~ SUBTYPE ~ AASTORE
              case RInt32 => START[StackNil ** PReference[PArray[PReference[PAnyObject]]] ** PReference[PArray[PReference[PAnyObject]]] ** PInt32] ~ THISLOAD(refTuple) ~ GETFIELD(RReference(tupleType), indexFieldName(index), RInt32, undoErasure = false) ~ BoxInt32 ~ SUBTYPE ~ AASTORE
              case RInt64 => START[StackNil ** PReference[PArray[PReference[PAnyObject]]] ** PReference[PArray[PReference[PAnyObject]]] ** PInt32] ~ THISLOAD(refTuple) ~ GETFIELD(RReference(tupleType), indexFieldName(index), RInt64, undoErasure = false) ~ BoxInt64 ~ SUBTYPE ~ AASTORE
              case RChar => START[StackNil ** PReference[PArray[PReference[PAnyObject]]] ** PReference[PArray[PReference[PAnyObject]]] ** PInt32] ~ THISLOAD(refTuple) ~ GETFIELD(RReference(tupleType), indexFieldName(index), RChar, undoErasure = false) ~ BoxChar ~ SUBTYPE ~ AASTORE
              case RFloat32 => START[StackNil ** PReference[PArray[PReference[PAnyObject]]] ** PReference[PArray[PReference[PAnyObject]]] ** PInt32] ~ THISLOAD(refTuple) ~ GETFIELD(RReference(tupleType), indexFieldName(index), RFloat32, undoErasure = false) ~ BoxFloat32 ~ SUBTYPE ~ AASTORE
              case RFloat64 => START[StackNil ** PReference[PArray[PReference[PAnyObject]]] ** PReference[PArray[PReference[PAnyObject]]] ** PInt32] ~ THISLOAD(refTuple) ~ GETFIELD(RReference(tupleType), indexFieldName(index), RFloat64, undoErasure = false) ~ BoxFloat64 ~ SUBTYPE ~ AASTORE
              case RReference(_) => START[StackNil ** PReference[PArray[PReference[PAnyObject]]] ** PReference[PArray[PReference[PAnyObject]]] ** PInt32] ~ THISLOAD(refTuple) ~ GETFIELD(RReference(tupleType), indexFieldName(index), RObject.rType, undoErasure = false) ~ SUBTYPE ~ AASTORE
            })
      } ~
      ARETURN
  }

}

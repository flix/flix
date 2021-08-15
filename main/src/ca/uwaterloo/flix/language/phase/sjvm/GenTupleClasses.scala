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

  val getBoxedArrayMethodName: String = "getBoxedArray"
  val getBoxedArrayMethodDescriptor: String = JvmName.getMethodDescriptor(Nil, RArray(RReference(RObject)))

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
    val classMaker = ClassMaker.mkClass(className, addSource = false, None)
    classMaker.mkSuperConstructor()
    tupleType.elms.zipWithIndex.foreach {
      case (indexType, index) => classMaker.mkField(indexFieldName(index), indexType.erasedType, Mod.isPublic)
    }
    classMaker.mkMethod(genBoxedArrayFunction(tupleType), getBoxedArrayMethodName, getBoxedArrayMethodDescriptor, Mod.isPublic)
    classMaker.closeClassMaker
  }

  def genBoxedArrayFunction[T <: PType](tupleType: RTuple): F[StackNil] => F[StackEnd] = {
    START[StackNil] ~
      pushInt32(tupleType.elms.size) ~
      ANEWARRAY(RReference(RObject)) ~
      multiComposition(tupleType.elms.zipWithIndex) {
        case (indexType, index) =>
          START[StackNil ** PReference[PArray[PReference[PAnyObject]]]] ~
            DUP ~
            pushInt32(index) ~
            (indexType match {
              // TODO(JLS): use Boolean class for bool
              case RType.RBool => START[StackNil ** PReference[PArray[PReference[PAnyObject]]] ** PReference[PArray[PReference[PAnyObject]]] ** PInt32] ~ BoxInt32(THISLOAD(tagOf[PTuple]) ~ GetInt32Field(RReference(tupleType), indexFieldName(index))) ~ SUBTYPE ~ AASTORE
              case RType.RInt8 => START[StackNil ** PReference[PArray[PReference[PAnyObject]]] ** PReference[PArray[PReference[PAnyObject]]] ** PInt32] ~ BoxInt8(THISLOAD(tagOf[PTuple]) ~ GetInt8Field(RReference(tupleType), indexFieldName(index))) ~ SUBTYPE ~ AASTORE
              case RType.RInt16 => START[StackNil ** PReference[PArray[PReference[PAnyObject]]] ** PReference[PArray[PReference[PAnyObject]]] ** PInt32] ~ BoxInt16(THISLOAD(tagOf[PTuple]) ~ GetInt16Field(RReference(tupleType), indexFieldName(index))) ~ SUBTYPE ~ AASTORE
              case RType.RInt32 => START[StackNil ** PReference[PArray[PReference[PAnyObject]]] ** PReference[PArray[PReference[PAnyObject]]] ** PInt32] ~ BoxInt32(THISLOAD(tagOf[PTuple]) ~ GetInt32Field(RReference(tupleType), indexFieldName(index))) ~ SUBTYPE ~ AASTORE
              case RType.RInt64 => START[StackNil ** PReference[PArray[PReference[PAnyObject]]] ** PReference[PArray[PReference[PAnyObject]]] ** PInt32] ~ BoxInt64(THISLOAD(tagOf[PTuple]) ~ GetInt64Field(RReference(tupleType), indexFieldName(index))) ~ SUBTYPE ~ AASTORE
              case RType.RChar => START[StackNil ** PReference[PArray[PReference[PAnyObject]]] ** PReference[PArray[PReference[PAnyObject]]] ** PInt32] ~ BoxChar(THISLOAD(tagOf[PTuple]) ~ GetCharField(RReference(tupleType), indexFieldName(index))) ~ SUBTYPE ~ AASTORE
              case RType.RFloat32 => START[StackNil ** PReference[PArray[PReference[PAnyObject]]] ** PReference[PArray[PReference[PAnyObject]]] ** PInt32] ~ BoxFloat32(THISLOAD(tagOf[PTuple]) ~ GetFloat32Field(RReference(tupleType), indexFieldName(index))) ~ SUBTYPE ~ AASTORE
              case RType.RFloat64 => START[StackNil ** PReference[PArray[PReference[PAnyObject]]] ** PReference[PArray[PReference[PAnyObject]]] ** PInt32] ~ BoxFloat64(THISLOAD(tagOf[PTuple]) ~ GetFloat64Field(RReference(tupleType), indexFieldName(index))) ~ SUBTYPE ~ AASTORE
              case RReference(referenceType) => START[StackNil ** PReference[PArray[PReference[PAnyObject]]] ** PReference[PArray[PReference[PAnyObject]]] ** PInt32] ~ THISLOAD(tagOf[PTuple]) ~ GetObjectField(RReference(tupleType), indexFieldName(index), RObject, undoErasure = false /* does nothing here */) ~ SUBTYPE ~ AASTORE
            })
      } ~
      ARETURN
  }

}

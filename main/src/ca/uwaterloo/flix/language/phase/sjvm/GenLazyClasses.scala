/*
 * Copyright 2020-2021 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.phase.sjvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ErasedAst.Root
import ca.uwaterloo.flix.language.ast.PRefType._
import ca.uwaterloo.flix.language.ast.PType._
import ca.uwaterloo.flix.language.ast.RRefType._
import ca.uwaterloo.flix.language.ast.RType._
import ca.uwaterloo.flix.language.ast.{ErasedAst, PType, RType, SourceLocation}
import ca.uwaterloo.flix.language.phase.sjvm.BytecodeCompiler._
import ca.uwaterloo.flix.language.phase.sjvm.ClassMaker.Mod
import ca.uwaterloo.flix.language.phase.sjvm.Instructions._

object GenLazyClasses {

  val InitializedFieldName: String = "initialized"
  val InitializedFieldType: RType[PInt32] = RBool
  val ExpressionFieldName: String = "expression"
  val ValueFieldName: String = "value"
  val ForceMethod: String = "force"

  def constructorDescriptor[T <: PType](valueType: RType[T]): String = expressionFieldType(valueType).thisToNothingMethodDescriptor

  def expressionFieldType[T <: PType](valueType: RType[T]): RType[PReference[PFunction[T]]] = RReference(RArrow(RReference(RObject) :: Nil, valueType))

  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    //Type that we need a cell class for
    RType.baseTypes.foldLeft(Map[JvmName, JvmClass]()) {
      case (macc, tpe) =>
        val lazyType = RReference(RLazy(tpe))
        val bytecode = genByteCode(lazyType, tpe)
        macc + (lazyType.jvmName -> JvmClass(lazyType.jvmName, bytecode))
    }
  }

  private def genByteCode[T <: PType](lazyType: RReference[PLazy[T]], valueFieldType: RType[T])(implicit root: Root, flix: Flix): Array[Byte] = {
    val classMaker = ClassMaker.mkClass(lazyType.jvmName, JvmName.Java.Object)

    classMaker.mkField(InitializedFieldName, InitializedFieldType)
    classMaker.mkField(ExpressionFieldName, expressionFieldType(valueFieldType))
    classMaker.mkField(ValueFieldName, valueFieldType)
    classMaker.mkMethod(compileForceMethod(lazyType, valueFieldType), ForceMethod, valueFieldType.nothingToThisMethodDescriptor, Mod.isFinal.isPublic)
    classMaker.mkConstructor(compileLazyConstructor(lazyType, valueFieldType))
    classMaker.closeClassMaker
  }

  private def compileForceMethod[T <: PType](lazyType: RReference[PLazy[T]], valueFieldType: RType[T])(implicit root: Root, flix: Flix): F[StackNil] => F[StackEnd] = {
    /*
    force() :=

    lock(this)
    if (!this.initialized) {
      this.value = this.expression()
      this.initialized = true
    }
    tpe result = this.value
    unlock(this)
    return result
     */
    THISLOAD(lazyType) ~
      (WITHMONITOR(valueFieldType) {
        START[StackNil ** PReference[PLazy[T]]] ~
          THISLOAD(lazyType) ~
          GETFIELD(lazyType, InitializedFieldName, RBool, undoErasure = false) ~
          (IFNE {
            START[StackNil ** PReference[PLazy[T]]] ~
              THISLOAD(lazyType) ~
              GETFIELD(lazyType, ExpressionFieldName, expressionFieldType(valueFieldType), undoErasure = true) ~
              CALL(ErasedAst.Expression.Unit(SourceLocation.Unknown) :: Nil, RArrow(RReference(RUnit) :: Nil, valueFieldType)) ~
              THISLOAD(lazyType) ~
              XSWAP(lazyType, valueFieldType) ~
              PUTFIELD(lazyType, ValueFieldName, valueFieldType, erasedType = true) ~
              THISLOAD(lazyType) ~
              pushBool(true) ~
              PUTFIELD(lazyType, InitializedFieldName, RInt32, erasedType = false /* does not do anything */)
          }(NOP)) ~
          THISLOAD(lazyType) ~
          GETFIELD(lazyType, ValueFieldName, valueFieldType, undoErasure = true)
      }) ~
      XRETURN(valueFieldType)
  }

  /**
    * The constructor takes a expression object, which should be a function that takes
    * no argument and returns something of type tpe, related to the type of the lazy class.
    */
  def compileLazyConstructor[T <: PType](lazyType: RReference[PLazy[T]], valueFieldType: RType[T])(implicit root: Root, flix: Flix): F[StackNil] => F[StackEnd] = {
    /*
    Lazy$tpe(expression) :=

    this.initialized = false
     */
    START[StackNil] ~
      THISINIT(JvmName.Java.Object) ~
      preInitALOAD(0, lazyType) ~
      pushBool(false) ~
      PUTFIELD(lazyType, InitializedFieldName, InitializedFieldType, erasedType = false) ~
      RETURN
  }
}

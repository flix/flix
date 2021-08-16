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

/**
 * Generates bytecode for the lazy classes.
 */
object GenLazyClasses {

  val initializedFieldName: String = "initialized"
  val initializedFieldType: RType[PInt32] = RBool
  val expressionFieldName: String = "expression"
  val valueFieldName: String = "value"
  val forceMethod: String = "force"

  def constructorDescriptor[T <: PType](valueType: RType[T]): String = expressionFieldType(valueType).thisToNothingMethodDescriptor

  def expressionFieldType[T <: PType](valueType: RType[T]): RType[PReference[PFunction[T]]] = RReference(RArrow(RReference(RObject) :: Nil, valueType))

  /**
   * Returns the set of lazy classes for the given set of types `ts`.
   */
  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    //Type that we need a cell class for
    RType.baseTypes.foldLeft(Map[JvmName, JvmClass]()) {
      case (macc, tpe) =>
        val lazyType = RReference(RLazy(tpe))
        val bytecode = genByteCode(lazyType, tpe)
        macc + (lazyType.jvmName -> JvmClass(lazyType.jvmName, bytecode))
    }
  }

  /**
   * This method creates the class for each lazy value.
   * The specific lazy class has an associated value type (tpe) which
   * is either a jvm primitive or object.
   *
   * The lazy class has three fields - initialized: bool, expression: () -> tpe,
   * and value: tpe. These are all private. force(context) is the only public
   * method, which retuns a value of type tpe given a context to call the
   * expression closure in.
   *
   * force will only evaluate the expression the first time, based on the flag initialized.
   * After that point it will store the result in value and just return that.
   */
  private def genByteCode[T <: PType](lazyType: RReference[PLazy[T]], valueFieldType: RType[T])(implicit root: Root, flix: Flix): Array[Byte] = {
    val classMaker = ClassMaker.mkClass(lazyType.jvmName, addSource = false, None)

    classMaker.mkField(initializedFieldName, initializedFieldType)
    classMaker.mkField(expressionFieldName, expressionFieldType(valueFieldType))
    classMaker.mkField(valueFieldName, valueFieldType)
    classMaker.mkMethod(compileForceMethod(lazyType, valueFieldType), forceMethod, valueFieldType.nothingToThisMethodDescriptor, Mod.isFinal.isPublic)
    classMaker.mkConstructor(compileLazyConstructor(lazyType, valueFieldType), constructorDescriptor(valueFieldType))
    classMaker.closeClassMaker
  }

  /**
   * The force method takes a context as argument to call the expression closure in.
   * The result of the expression given in the constructor is then returned.
   * This is only actually evaluated the first time, and saved to return directly
   * afterwards.
   *
   * If lazy has associated type of Obj, the returned object needs to be casted
   * to whatever expected type.
   */
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
          GetBoolField(lazyType, initializedFieldName) ~
          (IFNE {
            START[StackNil ** PReference[PLazy[T]]] ~
              THISLOAD(lazyType) ~
              GetObjectField(lazyType, expressionFieldName, expressionFieldType(valueFieldType), undoErasure = true) ~
              CALL(ErasedAst.Expression.Unit(SourceLocation.Unknown) :: Nil, RArrow(RReference(RUnit) :: Nil, valueFieldType)) ~
              THISLOAD(lazyType) ~
              XSWAP(lazyType, valueFieldType) ~
              PUTFIELD(lazyType, valueFieldName, valueFieldType, erasedType = true) ~
              THISLOAD(lazyType) ~
              pushBool(true) ~
              PUTFIELD(lazyType, initializedFieldName, RInt32, erasedType = false /* does not do anything */)
          } (NOP)) ~
          THISLOAD(lazyType) ~
          XGETFIELD(lazyType, valueFieldName, valueFieldType, undoErasure = true)
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
    this.expression = expression.
     */
    START[StackNil] ~
      THISLOAD(lazyType) ~
      INVOKEOBJECTCONSTRUCTOR ~
      THISLOAD(lazyType) ~
      pushBool(false) ~
      PUTFIELD(lazyType, initializedFieldName, initializedFieldType, erasedType = false) ~
      THISLOAD(lazyType) ~
      ALOAD(1, expressionFieldType(valueFieldType)) ~
      PUTFIELD(lazyType, expressionFieldName, expressionFieldType(valueFieldType), erasedType = false) ~
      RETURN
  }
}

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
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.ast.PRefType._
import ca.uwaterloo.flix.language.ast.PType._
import ca.uwaterloo.flix.language.ast.RRefType._
import ca.uwaterloo.flix.language.ast.RType._
import ca.uwaterloo.flix.language.ast.{PType, RType}
import ca.uwaterloo.flix.language.phase.sjvm.BytecodeCompiler._
import ca.uwaterloo.flix.language.phase.sjvm.Instructions._

/**
 * Generates bytecode for the lazy classes.
 */
object GenLazyClasses {

  // TODO: Needs to use new call protocol and use type erasure

  val InitializedFieldName: String = "initialized"
  val InitializedFieldType: RType[PInt32] = RBool()
  val ExpressionFieldName: String = "expression"
  val ExpressionFieldType: RReference[PAnyObject] = RReference(RObject())
  val ExpressionToVoid: String = JvmName.objectToVoid
  val ValueFieldName: String = "value"
  val ForceMethod: String = "force"

  /**
   * Returns the set of lazy classes for the given set of types `ts`.
   */
  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {

    // Generating each lazy class
    def genAUX[T <: PType](valueFieldType: RType[T]): (JvmName, JvmClass) = {
      val lazyType = RReference(RLazy(valueFieldType))
      val bytecode = genByteCode(lazyType, valueFieldType)
      lazyType.jvmName -> JvmClass(lazyType.jvmName, bytecode)
    }

    //Type that we need a cell class for
    Map() +
      genAUX(RBool()) +
      genAUX(RInt8()) +
      genAUX(RInt16()) +
      genAUX(RInt32()) +
      genAUX(RInt64()) +
      genAUX(RChar()) +
      genAUX(RFloat32()) +
      genAUX(RFloat64()) +
      genAUX(RReference(RObject()))
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
    val classMaker = ClassMaker.openClassWriter(lazyType, isFinal = true)

    classMaker.mkField(InitializedFieldName, InitializedFieldType, isPublic = false)
    classMaker.mkField(ExpressionFieldName, ExpressionFieldType, isPublic = false)
    classMaker.mkField(ValueFieldName, valueFieldType, isPublic = false)
    // TODO: This is temporary, call method needs to be changed
    val methodDescriptor = s"(LContext;)${valueFieldType.toDescriptor}"
    classMaker.mkMethod(compileForceMethod(lazyType, valueFieldType), ForceMethod, methodDescriptor, isFinal = true, isPublic = true)
    classMaker.mkConstructor(compileLazyConstructor(lazyType, valueFieldType), ExpressionToVoid)
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
    force(context) :=

    lock(this)
    if (!this.initialized) {
      this.value = this.expression()
      this.initialized = true
    }
    tpe result = this.value
    unlock(this)
    return result
     */
    THISLOAD(tag[PLazy[T]]) ~
      (WITHMONITOR(valueFieldType) {
        START[StackNil ** PReference[PLazy[T]]] ~
          THISLOAD(tag[PLazy[T]]) ~
          GetBoolField(lazyType, InitializedFieldName) ~
          (RUNIFTRUE {
            START[StackNil ** PReference[PLazy[T]]] ~
              THISLOAD(tag[PLazy[T]]) ~
              GetObjectField(lazyType, ExpressionFieldName, tag[PAnyObject]) ~
              compileClosureApplication(valueFieldType) ~
              THISLOAD(tag[PLazy[T]]) ~
              XSWAP(lazyType, valueFieldType) ~
              PUTFIELD(lazyType, ValueFieldName, valueFieldType) ~
              THISLOAD(tag[PLazy[T]]) ~
              pushInt32(1) ~
              PUTFIELD(lazyType, InitializedFieldName, RInt32())
          }) ~
          THISLOAD(tag[PLazy[T]]) ~
          XGETFIELD(lazyType, ValueFieldName, valueFieldType)
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
      THISLOAD(tag[PLazy[T]]) ~
      INVOKEOBJECTCONSTRUCTOR ~
      THISLOAD(tag[PLazy[T]]) ~
      pushBool(false) ~
      PUTFIELD(lazyType, InitializedFieldName, InitializedFieldType) ~
      THISLOAD(tag[PLazy[T]]) ~
      ALOAD(1, tag[PAnyObject]) ~
      PUTFIELD(lazyType, ExpressionFieldName, ExpressionFieldType) ~
      RETURN
  }
}

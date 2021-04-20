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
import org.objectweb.asm.{ClassWriter, Opcodes}

/**
  * Generates bytecode for the lazy classes.
  */
object GenLazyClasses {

  val initializedField = "initialized"
  val initializedFieldType: RType[PInt32] = RBool()
  val expressionField = "expression"
  val expressionFieldType: RType[PReference[PAnyObject]] = RReference(null)
  val valueField = "value"
  val forceMethod = "force"

  /**
    * Returns the set of lazy classes for the given set of types `ts`.
    */
  def gen()(implicit root: Root, flix: Flix): Map[String, JvmClass] = {

    // Generating each lazy class
    def genAUX[T <: PType](tpe: RType[T]): (String, JvmClass) = {
      val eeType = RReference(RLazy(tpe))
      val className = getInternalName(eeType)
      val bytecode = genByteCode(className, tpe)
      className -> JvmClass(className, bytecode)
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
      genAUX(RReference(null))
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
  private def genByteCode[T <: PType](className: String, innerType: RType[T])(implicit root: Root, flix: Flix): Array[Byte] = {
    // class writer
    val visitor = AsmOps.mkClassWriter()

    // Initialize the visitor to create a class.
    visitor.visit(AsmOps.JavaVersion, Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL, className, null, objectName, null)

    AsmOps.compileField(visitor, initializedField, getDescriptor(RBool()), isStatic = false, isPrivate = true)
    AsmOps.compileField(visitor, expressionField, getDescriptor(expressionFieldType), isStatic = false, isPrivate = true)
    AsmOps.compileField(visitor, valueField, s"L$className;", isStatic = false, isPrivate = true)
    compileForceMethod(visitor, className, innerType)

    // Emit the code for the constructor
    compileLazyConstructor(className, innerType)

    visitor.visitEnd()
    visitor.toByteArray
  }

  private def compileForceMethod[T <: PType](visitor: ClassWriter, className: String, innerType: RType[T])(implicit root: Root, flix: Flix): Unit = {
    val erasedValueTypeDescriptor = getDescriptor(innerType)

    // Header of the method.
    val returnDescription = s"(LContext;)$erasedValueTypeDescriptor"
    val method = visitor.visitMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL, forceMethod, returnDescription, null, null)
    method.visitCode()
    val t = compileForceMethod(className, innerType)
    t(F(method))
    // Parameters of visit max are thrown away because visitor will calculate the frame and variable stack size
    method.visitMaxs(1, 1)
    method.visitEnd()
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
  private def compileForceMethod[T <: PType](className: String, innerType: RType[T])(implicit root: Root, flix: Flix): F[StackNil] => F[StackEnd] = {
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
    THISLOAD[StackNil, PLazy[T]] ~
      (WITHMONITOR(innerType) {
        THISLOAD[StackNil ** PReference[PLazy[T]], PLazy[T]] ~
          GetBoolField(className, initializedField) ~
          (RUNIFTRUE {
            THISLOAD[StackNil ** PReference[PLazy[T]], PLazy[T]] ~[StackNil ** PReference[PLazy[T]] ** PReference[PAnyObject]]
              GetObjectField(className, expressionField) ~
              compileClosureApplication(innerType) ~
              THISLOAD[StackNil ** PReference[PLazy[T]] ** T, PLazy[T]] ~
              XSWAP(RReference(RLazy(innerType)), innerType) ~
              PUTFIELD(className, valueField, innerType) ~
              THISLOAD[StackNil ** PReference[PLazy[T]], PLazy[T]] ~
              pushInt32(1) ~
              PUTFIELD(className, initializedField, RInt32())
          }) ~
          THISLOAD[StackNil ** PReference[PLazy[T]], PLazy[T]] ~
          XGETFIELD(className, valueField, innerType)
      }) ~
      XRETURN(innerType)
  }

  def compileLazyConstructor[T <: PType](visitor: ClassWriter, className: String, innerType: RType[T])(implicit root: Root, flix: Flix): Unit = {
    val constructor = visitor.visitMethod(Opcodes.ACC_PUBLIC, constructorMethod, s"($objectDescriptor)V", null, null)
    constructor.visitCode()
    val c = compileLazyConstructor(className, innerType)
    c(F(constructor))
    // Parameters of visit max are thrown away because visitor will calculate the frame and variable stack size
    constructor.visitMaxs(65535, 65535)
    constructor.visitEnd()
  }

  /**
    * The constructor takes a expression object, which should be a function that takes
    * no argument and returns something of type tpe, related to the type of the lazy class.
    */
  def compileLazyConstructor[T <: PType](className: String, innerType: RType[T])(implicit root: Root, flix: Flix): F[StackNil] => F[StackEnd] = {
    /*
    Lazy$tpe(expression) :=

    this.initialized = false
    this.expression = expression.
     */
    THISLOAD[StackNil, PLazy[T]] ~
      Instructions.INVOKESPECIAL(objectName, nothingToVoid) ~
      THISLOAD[StackNil, PLazy[T]] ~
      pushBool(false) ~
      PUTFIELD(className, initializedField, initializedFieldType) ~[StackNil ** PReference[PLazy[T]]]
      THISLOAD ~[StackNil ** PReference[PLazy[T]] ** PReference[PAnyObject]]
      ALOAD(1) ~
      PUTFIELD(className, expressionField, expressionFieldType) ~
      RETURN
  }
}

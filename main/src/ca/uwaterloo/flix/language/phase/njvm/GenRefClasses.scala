/*
 * Copyright 2019 Miguel Fialho
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

package ca.uwaterloo.flix.language.phase.njvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.phase.jvm.JvmType
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.Instructions._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.JvmModifier._




/**
  * Generates bytecode for the cell classes.
  */
object GenRefClasses {
  // TODO: incrementally copy over.


  def gen()(implicit root: Root, flix: Flix): Unit = {
//    genRefClass()
  }
  /**
    * Generating class `classType` with value of type `tpe`
    */
  def genRefClass(classType : JvmType.Reference)(implicit root: Root, flix: Flix): Array[Byte] = {

    val frame = new ClassGenerator(classType)

    frame.compile()
  }

  /**
    * Generating constructor for the class `classType` with value of type `cellType`
    */
  def genConstructor(classType: JvmType.Reference, cellType: JvmType, cg: ClassGenerator)(implicit root: Root, flix: Flix): Unit =
  {
//    val iLoad = AsmOps.getLoadInstruction(cellType)
//    val initMethod = cw.visitMethod(ACC_PUBLIC, "<init>", AsmOps.getMethodDescriptor(List(cellType), JvmType.Void), null, null)
//    initMethod.visitCode()
//    initMethod.visitVarInsn(ALOAD, 0)
//    initMethod.visitMethodInsn(INVOKESPECIAL, JvmName.Object.toInternalName, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)
//    initMethod.visitVarInsn(ALOAD, 0)
//    initMethod.visitVarInsn(iLoad, 1)
//    initMethod.visitFieldInsn(PUTFIELD, classType.name.toInternalName, "value", cellType.toDescriptor)
//    initMethod.visitInsn(RETURN)
//    initMethod.visitMaxs(2, 2)
//    initMethod.visitEnd()
//
//
//    val JvmInstructions : F[StackNil] => F[StackNil] =
//      THIS[StackNil] |>>
//      INVOKE(INVOKESPECIAL, JvmName.Object.toInternalName, Nil, JvmType.Void)|>>
//      THIS[StackNil] |>>
//      UNCHECKED_LOAD(cellType, 1) |>>
//      UNCHECKED_PUTFIELD("value", cellType) |>>
//      RETURN
//
//    cg.GenMethod(List(PUBLIC), "<init>", Nil, JvmType.Void,
//      JvmInstructions)

  }

  /**
    * Generating `getValue` method for the class `classType` with value of type `cellType`
    */
  def genGetValue[T](funLocals: FunLocals0, field1 : Field[T], cellType: JvmType, cg: ClassGenerator)(implicit root: Root, flix: Flix): Unit =
  {

    val JvmInstructions : F[StackNil] => F[StackNil] =
        funLocals.LOAD[StackNil]() |>>
        field1.GET_FIELD() |>>
        UNCHECKED_RETURN(cellType)

    cg.GenMethod(List(Public), "getValue", Nil, cellType,
      JvmInstructions)
  }

  /**
    * Generating `setValue` method for the class `classType` with value of type `cellType`
    */
  def genSetValue[T](funLocals: FunLocals1[T], field1 : Field[T], cellType: JvmType, cg: ClassGenerator)(implicit root: Root, flix: Flix): Unit = {

    val JvmInstructions : F[StackNil] => F[StackNil] =
            funLocals._1.LOAD[StackNil]() |>>
            funLocals._2.LOAD() |>>
            field1.PUT_FIELD() |>>
            RETURN


    cg.GenMethod(List(Public), "setValue", List(cellType), JvmType.Void,
      JvmInstructions)
  }

}

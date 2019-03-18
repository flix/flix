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
import ca.uwaterloo.flix.language.phase.jvm.{JvmClass, JvmName, JvmType}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.Instructions._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.JvmModifier._


/**
  * Generates bytecode for the cell classes.
  */
object GenRefClasses1 {
  // TODO: incrementally copy over.


  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass]  = {
    // Type that we need a cell class for
    val types = List(JvmType.PrimBool, JvmType.PrimChar, JvmType.PrimFloat, JvmType.PrimDouble,
      JvmType.PrimByte, JvmType.PrimShort, JvmType.PrimInt, JvmType.PrimLong, JvmType.Object)

    // Generating each cell class
    types.map { tpe =>
      val classType = JvmName.getCellClassType(tpe)
      classType.name -> JvmClass(classType.name, genRefClass(classType, tpe))
    }.toMap
  }

  /**
    * Generating class `classType` with value of type `tpe`
    */
  def genRefClass(classType : JvmType.Reference, cellType : JvmType)(implicit root: Root, flix: Flix): Array[Byte] = {

    val cg = new ClassGenerator(classType,List(Public,Final), JvmName.Object.toInternalName, null)

    val field0 = cg.compileField(List(Private),"field0", cellType)
    val constructorLocals = new FunSig1(List(Public), "<init>", JvmType.Void, classType, cellType)
    val getValueLocals = new FunSig0(List(Public,Final), "getValue", cellType, classType)
    val setValueLocals = new FunSig1(List(Public,Final), "setValue", JvmType.Void, classType, cellType)

    genConstructor(constructorLocals, field0, cg)
    genGetValue(getValueLocals, field0,cg)
    genSetValue(setValueLocals, field0, cg)


    cg.compile()
  }

  /**
    * Generating constructor for the class `classType` with value of type `cellType`
    */
  def genConstructor[T,U](funSig: FunSig1[T,U], field0 : Field[T], cg: ClassGenerator)(implicit root: Root, flix: Flix): Unit =
  {
    val constructorHandler = mkMethodHandler0Void(InvokeSpecial, JvmName.Object.toInternalName, "<init>" ,Nil, JvmType.Void)

    val JvmInstructions : F[StackNil] => F[StackNil] =
      funSig.arg0.LOAD[StackNil]()|>>
      constructorHandler.invoke()|>>
      funSig.arg0.LOAD() |>>
      funSig.arg1.LOAD() |>>
      field0.PUT_FIELD() |>>
      RETURN

    cg.GenMethod(funSig, JvmInstructions)

  }

  /**
    * Generating `getValue` method for the class `classType` with value of type `cellType`
    */
  def genGetValue[T](funSig: FunSig0[T], field0 : Field[T], cg: ClassGenerator)(implicit root: Root, flix: Flix): Unit =
  {

    val JvmInstructions : F[StackNil] => F[StackNil] =
        funSig.arg0.LOAD[StackNil]() |>>
        field0.GET_FIELD() |>>
        UNCHECKED_RETURN(funSig.returnType)

    cg.GenMethod(funSig, JvmInstructions)
  }

  /**
    * Generating `setValue` method for the class `classType` with value of type `cellType`
    */
  def genSetValue[T,U](funSig: FunSig1[T,U], field0 : Field[T], cg: ClassGenerator)(implicit root: Root, flix: Flix): Unit = {

    val JvmInstructions : F[StackNil] => F[StackNil] =
            funSig.arg0.LOAD[StackNil]() |>>
            funSig.arg1.LOAD() |>>
            field0.PUT_FIELD() |>>
            RETURN


    cg.GenMethod(funSig, JvmInstructions)
  }

}

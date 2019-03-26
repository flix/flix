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
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.JvmModifier._

import scala.reflect.runtime.universe._


/**
  * Generates bytecode for the cell classes.
  */

class RefClass[T : TypeTag](implicit root: Root, flix: Flix) {

  //Setup
  private val classType : JvmType.Reference = JvmName.getCellClassType(getJvmType[T])
  private val cg : ClassGenerator =  new ClassGenerator(classType, List(Public,Final), JvmName.Object.toInternalName, null)

  //Declare Fields
  private val field0 : Field[T] = cg.compileField[T](List(Private),"field0")

  //Declare methods
  //Constructor
  val constructor : MethodHandler1[T, JvmType.Void.type ] = {

    val funSig = cg.mkFunSig1[T, JvmType.Void.type](List(Public), "<init>")

    val superConstructor = Api.JavaRuntimeFunction.ObjectConstructor

    val ins : F[StackNil] => F[StackNil] =
        funSig.getArg0.LOAD[StackNil]|>>
        superConstructor.INVOKE |>>
        funSig.getArg0.LOAD[StackNil]|>>
        funSig.getArg1.LOAD |>>
        field0.PUT_FIELD

    funSig.genVoidMethod(ins)
  }

  //getValue
  val getValue : MethodHandler0[T] = {

    val funSig = cg.mkFunSig0[T](List(Public,Final), "getValue")


    val ins : F[StackNil] => F[StackNil ** T] =
        funSig.getArg0.LOAD[StackNil] |>>
        field0.GET_FIELD

    funSig.genMethod(ins)
  }


  //setValue
  val setValue : MethodHandler1[T, JvmType.Void.type ] = {

    val funSig = cg.mkFunSig1[T, JvmType.Void.type](List(Public,Final), "setValue")

    val ins : F[StackNil] => F[StackNil] =
        funSig.getArg0.LOAD[StackNil] |>>
        funSig.getArg1.LOAD |>>
        field0.PUT_FIELD

    funSig.genVoidMethod(ins)
  }

  def genClass : (JvmName, JvmClass) = {
    classType.name -> JvmClass(classType.name , cg.compile())
  }
}

class RefClasses(implicit root: Root, flix: Flix) {
  object PrimBool extends RefClass[JvmType.PrimBool.type]
  object PrimChar extends RefClass[JvmType.PrimChar.type]
  object PrimFloat extends RefClass[JvmType.PrimFloat.type]
  object PrimDouble extends RefClass[JvmType.PrimDouble.type]
  object PrimByte extends RefClass[JvmType.PrimByte.type]
  object PrimShort extends RefClass[JvmType.PrimShort.type]
  object PrimInt extends RefClass[JvmType.PrimInt.type]
  object PrimLong extends RefClass[JvmType.PrimLong.type]
  object Object extends RefClass[JvmType.Reference]
  val values = List(PrimBool, PrimChar, PrimFloat, PrimDouble, PrimByte, PrimShort, PrimInt, PrimLong, Object)
}

object GenRefClasses2 {

  def gen()(implicit root: Root, flix: Flix): Map[JvmName,JvmClass] = {
    (new RefClasses).values.map(refClass => refClass.genClass).toMap
  }
}


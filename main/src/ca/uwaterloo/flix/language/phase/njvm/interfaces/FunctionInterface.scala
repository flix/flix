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
package ca.uwaterloo.flix.language.phase.njvm.interfaces

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.phase.jvm.{JvmClass, JvmName}
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.MnemonicsTypes._
import ca.uwaterloo.flix.language.phase.njvm.Mnemonics.{InterfaceGenerator, _}
import ca.uwaterloo.flix.language.phase.njvm.NJvmType
import ca.uwaterloo.flix.language.phase.njvm.NJvmType._
import ca.uwaterloo.flix.util.InternalCompilerException

import scala.reflect.runtime.universe._

class FunctionInterface(elms: List[NJvmType], returnType: NJvmType)(implicit root: Root, flix: Flix) extends MnemonicsClass {
  //Setup
  private val it: Reference = getFunctionInterfaceType(elms, returnType)
  private val ig: InterfaceGenerator = {
    val continuationInterfaceType = returnType match {
      case PrimBool => getContinuationInterfaceType[MBool]
      case PrimChar => getContinuationInterfaceType[MChar]
      case PrimByte => getContinuationInterfaceType[MByte]
      case PrimShort => getContinuationInterfaceType[MShort]
      case PrimInt => getContinuationInterfaceType[MInt]
      case PrimLong => getContinuationInterfaceType[MLong]
      case PrimFloat => getContinuationInterfaceType[MFloat]
      case PrimDouble => getContinuationInterfaceType[MDouble]
      case Reference(_) => getContinuationInterfaceType[Ref[MObject]]
      case _ => throw InternalCompilerException(s"Unexpected type $returnType")
    }
    new InterfaceGenerator(it, List(continuationInterfaceType, NJvmType.Function))
  }

  //Fields
  //Interface has no fields ig doesn't even allow to compile fields

  //Methods each variable represents a method which can be called
  //there each of them holds the capability to call the corresponding method
  /**
    * Generate the setArg interface method
    */
  elms.zipWithIndex.map {
    case (arg, ind) => arg match {
      case PrimBool => ig.mkVoidMethod2[Ref[FunctionInterface], MBool]("setArg" + ind)
      case PrimChar => ig.mkVoidMethod2[Ref[FunctionInterface], MChar]("setArg" + ind)
      case PrimByte => ig.mkVoidMethod2[Ref[FunctionInterface], MByte]("setArg" + ind)
      case PrimShort => ig.mkVoidMethod2[Ref[FunctionInterface], MShort]("setArg" + ind)
      case PrimInt => ig.mkVoidMethod2[Ref[FunctionInterface], MInt]("setArg" + ind)
      case PrimLong => ig.mkVoidMethod2[Ref[FunctionInterface], MLong]("setArg" + ind)
      case PrimFloat => ig.mkVoidMethod2[Ref[FunctionInterface], MFloat]("setArg" + ind)
      case PrimDouble => ig.mkVoidMethod2[Ref[FunctionInterface], MDouble]("setArg" + ind)
      case Reference(_) => ig.mkVoidMethod2[Ref[FunctionInterface], Ref[MObject]]("setArg" + ind)
      case _ => throw InternalCompilerException(s"Unexpected type $arg")
    }
  }

  /**
    * Generate the capability to invoke setArg interface method
    */
  def setArgMethod[T1 <: MnemonicsTypes : TypeTag](index: Int): VoidMethod2[Ref[FunctionInterface], T1] =
    new VoidMethod2[Ref[FunctionInterface], T1](JvmModifier.InvokeInterface, it, "setArg" + index)

  private val jvmClass: JvmClass = JvmClass(it.name, ig.compile())

  def getJvmClass: JvmClass = jvmClass

  def getClassMapping: (JvmName, MnemonicsClass) =
    it.name -> this
}

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

package ca.uwaterloo.flix.language.phase.sjvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ErasedAst.Root
import ca.uwaterloo.flix.language.ast.PRefType.PFunction
import ca.uwaterloo.flix.language.ast.PType.PReference
import ca.uwaterloo.flix.language.ast.RRefType.RArrow
import ca.uwaterloo.flix.language.ast.RType.RReference
import ca.uwaterloo.flix.language.ast.{PType, RType}
import ca.uwaterloo.flix.language.phase.sjvm.ClassMaker.Mod

/**
  * Generates bytecode for the function interfaces.
  */
object GenFunctionInterfaces {
  def argFieldName(index: Int) = s"arg$index"

  /**
    * Returns the set of function interfaces for the given set of types `ts`.
    */
  def gen[T <: PType](tpe: Set[RType[PReference[PFunction]]])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    tpe.foldLeft(Map.empty[JvmName, JvmClass]) {
      case (macc, RReference(functionType@RArrow(_, _))) =>
        val bytecode = genByteCode(functionType)
        macc + (functionType.functionInterfaceName -> JvmClass(functionType.functionInterfaceName, bytecode))
    }
  }

  /**
    * Returns the function interface of the given type `tpe`.
    */
  private def genByteCode(functionType: RArrow)(implicit root: Root, flix: Flix): Array[Byte] = {

    // Class visitor
    // TODO(JLS): Add the two super interfaces
    //`JvmType` of the continuation interface for `tpe`
    val continuationSuperInterface = functionType.result.contName
    // `JvmType` of the java.util.functions.Function
    //        val javaFunctionSuperInterface = JvmType.Function
    val classMaker = ClassMaker.mkAbstractClass(functionType.functionInterfaceName, addSource = false, Some(continuationSuperInterface))

    val fieldMod = Mod.isAbstract.isPublic
    // Adding setters for each argument of the function
    for ((arg, index) <- functionType.args.zipWithIndex) {
      // `arg$index` field
      classMaker.mkField(argFieldName(index), arg.erasedType, fieldMod)
    }
    classMaker.mkSuperConstructor()

    classMaker.closeClassMaker
  }

}

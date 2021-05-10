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
import ca.uwaterloo.flix.language.ast.RRefType.{RArrow, RObject}
import ca.uwaterloo.flix.language.ast.RType.RReference
import ca.uwaterloo.flix.language.ast.{PRefType, PType, RType}
import ca.uwaterloo.flix.language.phase.sjvm.ClassMaker.Mod
import ca.uwaterloo.flix.util.InternalCompilerException
import org.objectweb.asm.Opcodes._

/**
 * Generates bytecode for the function interfaces.
 */
object GenFunctionInterfaces {
  val resultFieldName: String = "res"

  /**
   * Returns the set of function interfaces for the given set of types `ts`.
   */
  def gen[T <: PType](tpe: Set[RType[PReference[PFunction]]])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    tpe.foldLeft(Map.empty[JvmName, JvmClass]) {
      case (macc, tpe@RReference(RArrow(args, result))) =>
        val bytecode = genByteCode(tpe, args, result)
        macc + (tpe.jvmName -> JvmClass(tpe.jvmName, bytecode))
    }
  }

  /**
   * Returns the function interface of the given type `tpe`.
   */
  private def genByteCode(functionType: RReference[PFunction], args: List[RType[_ <: PType]], resultType: RType[_ <: PType])(implicit root: Root, flix: Flix): Array[Byte] = {

    // Class visitor
    // TODO(JLS): Add the two super interfaces
    //`JvmType` of the continuation interface for `tpe`
    val continuationSuperInterface = resultType.contName
    // `JvmType` of the java.util.functions.Function
    //        val javaFunctionSuperInterface = JvmType.Function
    val classMaker = ClassMaker.mkClass(functionType.jvmName, addSource = false)

    val fieldMod = Mod.isAbstract.isPublic
    // Adding setters for each argument of the function
    for ((arg, index) <- args.zipWithIndex) {
      // `arg$index` field
      classMaker.mkField(s"arg$index", arg.erasedType, fieldMod)
    }
    classMaker.mkField(resultFieldName, resultType.erasedType, fieldMod)

    classMaker.closeClassMaker
  }

}

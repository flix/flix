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
import ca.uwaterloo.flix.language.ast.PRefType.PAnyObject
import ca.uwaterloo.flix.language.ast.RRefType.{RObject, RRef}
import ca.uwaterloo.flix.language.ast.RType._
import ca.uwaterloo.flix.language.ast.{PType, RType}
import ca.uwaterloo.flix.language.phase.sjvm.ClassMaker.Mod
import ca.uwaterloo.flix.language.phase.sjvm.Instructions.RETURN

/**
 * Generates bytecode for the continuation interfaces.
 * TODO(JLS): fix comments in general
 */
object GenContinuationInterfaces {
  val resultFieldName: String = "result"
  val invokeMethodName: String = "invoke"

  /**
   * Returns the set of continuation interfaces for
   */
  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {

    // Generating each cont class
    def genAUX[T <: PType](resultType: RType[T]): (JvmName, JvmClass) = {
      val contName = resultType.contName
      contName -> JvmClass(contName, genByteCode(resultType))
    }

    // TODO(JLS): make this list once using List[RType[_ <: PType]]
    //Type that we need a cont interface for
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
   * Returns the bytecode for the given continuation interface.
   */
  private def genByteCode[T <: PType](resultType: RType[T])(implicit root: Root, flix: Flix): Array[Byte] = {

    // Pseudo code to generate:
    //
    // interface Cont_Bool {
    //   boolean getResult();
    //   Cont_Bool invoke();
    // }
    //
    // The names `getResult` and `apply` are fixed and can be used as strings.
    //
    // The type Cont$Bool is available as the jvmType argument.
    //
    // From the type, the JvmName can be extract and from this we can get the internal name of Cont$Bool.
    //
    // The result type (here bool) we is provided as an argument. It could be primitive or some compound type.
    // We can use `getErasedType` to map it down into one of the primitive types or to Object.
    //

    // Class visitor
    val classMaker = ClassMaker.mkAbstractClass(resultType.contName, addSource = false)
    classMaker.mkObjectConstructor[PAnyObject]()
    classMaker.mkField(resultFieldName, resultType, Mod.isPublic.isAbstract)
    classMaker.mkAbstractMethod(invokeMethodName, resultType.nothingToCont, Mod.isAbstract.isPublic)

    classMaker.closeClassMaker
  }

}

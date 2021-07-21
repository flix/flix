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
import ca.uwaterloo.flix.language.ast.{PType, RType}
import ca.uwaterloo.flix.language.phase.sjvm.BytecodeCompiler.{**, F, StackEnd, StackNil}
import ca.uwaterloo.flix.language.phase.sjvm.ClassMaker.Mod
import ca.uwaterloo.flix.language.phase.sjvm.Instructions._

/**
  * Generates bytecode for the continuation interfaces.
  * TODO(JLS): fix comments in general
  */
object GenContinuationInterfaces {
  val resultFieldName: String = "result"
  val invokeMethodName: String = "invoke"
  val unwindMethodName: String = "unwind"

  /**
    * Returns the set of continuation interfaces for
    */
  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    RType.baseTypes.foldLeft(Map[JvmName, JvmClass]()) {
      case (macc, tpe) =>
        val contName = tpe.contName
        macc + (contName -> JvmClass(contName, genByteCode(tpe)))
    }
  }

  /**
    * Returns the bytecode for the given continuation interface.
    */
  private def genByteCode[T <: PType](resultType: RType[T])(implicit root: Root, flix: Flix): Array[Byte] = {

    // Pseudo code to generate:
    //
    // abstract class Cont_Bool {
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
    val classMaker = ClassMaker.mkAbstractClass(resultType.contName, addSource = false, None)
    classMaker.mkSuperConstructor()
    classMaker.mkField(resultFieldName, resultType, Mod.isPublic.isAbstract)
    classMaker.mkAbstractMethod(invokeMethodName, resultType.nothingToContMethodDescriptor, Mod.isAbstract.isPublic)
    classMaker.mkMethod(compileUnwindMethod(resultType), unwindMethodName, resultType.nothingToThisMethodDescriptor, Mod.isPublic)

    classMaker.closeClassMaker
  }

  def compileUnwindMethod[T <: PType](resultType: RType[T]): F[StackNil] => F[StackEnd] = f => {
    import org.objectweb.asm.Label
    import org.objectweb.asm.Opcodes._

    f.visitor.visitVarInsn(ALOAD, 0)
    f.visitor.visitVarInsn(ASTORE, 1)

    f.visitor.visitInsn(ACONST_NULL)
    f.visitor.visitVarInsn(ASTORE, 2)

    val loopStart = new Label()
    f.visitor.visitLabel(loopStart)
    f.visitor.visitVarInsn(ALOAD, 1)
    f.visitor.visitVarInsn(ASTORE, 2)
    f.visitor.visitVarInsn(ALOAD, 1)
    f.visitor.visitMethodInsn(INVOKEVIRTUAL, resultType.contName.toInternalName, invokeMethodName, resultType.nothingToContMethodDescriptor, false)
    f.visitor.visitVarInsn(ASTORE, 1)
    f.visitor.visitVarInsn(ALOAD, 1)
    f.visitor.visitJumpInsn(IFNONNULL, loopStart)

    f.visitor.visitVarInsn(ALOAD, 2)
    f.visitor.visitFieldInsn(GETFIELD, resultType.contName.toInternalName, resultFieldName, resultType.toDescriptor)
    XRETURN(resultType)(f.asInstanceOf[F[StackNil ** T]])
  }

}

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
import ca.uwaterloo.flix.language.ast.PRefType._
import ca.uwaterloo.flix.language.ast.PType._
import ca.uwaterloo.flix.language.ast.RRefType._
import ca.uwaterloo.flix.language.ast.RType._
import ca.uwaterloo.flix.language.ast.{PType, RRefType, RType}
import ca.uwaterloo.flix.language.phase.sjvm.BytecodeCompiler.{**, F, StackEnd, StackNil}
import ca.uwaterloo.flix.language.phase.sjvm.ClassMaker.Mod
import ca.uwaterloo.flix.language.phase.sjvm.Instructions._

/**
  * Generates bytecode for the continuation interfaces.
  * TODO(JLS): fix comments in general
  */
object GenContinuationInterfaces {
  val ResultFieldName: String = "result"
  val InvokeMethodName: String = "invoke"
  val UnwindMethodName: String = "unwind"

  private val runName: String = "run"
  private val runDescriptor: Descriptor = JvmName.nothingToVoid

  /**
    * Returns the set of continuation interfaces for
    */
  def gen(functionTypes: Set[RType[PReference[PFunction[_ <: PType]]]])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    getReturnTypes(functionTypes).foldLeft(Map[JvmName, JvmClass]()) {
      case (macc, resultType) =>
        val contName = resultType.contName
        val superClass = JvmName.Java.Object
        val interface = JvmName.Java.Runnable
        macc + (contName -> JvmClass(contName, genByteCode(resultType, contName, superClass, interface)))
    }
  }

  private def getReturnTypes(types: Set[RType[PReference[PFunction[_ <: PType]]]]): Set[RType[_ <: PType]] = {
    def innerMatch[T <: PType](tpe: RRefType[PFunction[T]], setAcc: Set[RType[_ <: PType]]): Set[RType[_ <: PType]] = tpe match {
      case RArrow(_, result) => setAcc + result.erasedType
      case _ => setAcc
    }

    val init = Set.empty[RType[_ <: PType]]
    types.foldLeft(init) { (setAcc, rType) =>
      rType match {
        case RReference(referenceType) => innerMatch(referenceType.asInstanceOf[RRefType[PFunction[PType]]], setAcc)
        case _ => setAcc
      }
    }
  }

  /**
    * Returns the bytecode for the given continuation interface.
    */
  private def genByteCode[T <: PType](resultType: RType[T], contName: JvmName, superClass: JvmName, interface: JvmName)(implicit root: Root, flix: Flix): Array[Byte] = {

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
    val classMaker = ClassMaker.mkAbstractClass(contName, superClass, interface)
    classMaker.mkConstructor(START[StackNil] ~ THISINIT(superClass) ~ RETURN)
    classMaker.mkField(ResultFieldName, resultType, Mod.isPublic.isAbstract)
    classMaker.mkAbstractMethod(InvokeMethodName, resultType.nothingToContDescriptor, Mod.isAbstract.isPublic)
    classMaker.mkMethod(genUnwindMethod(resultType), UnwindMethodName, resultType.nothingToThisDescriptor, Mod.isPublic.isFinal)
    classMaker.mkMethod(genRunMethod(resultType, contName), runName, runDescriptor, Mod.isPublic.isFinal)

    classMaker.closeClassMaker
  }

  private def genUnwindMethod[T <: PType](resultType: RType[T]): F[StackNil] => F[StackEnd] = f => {
    import org.objectweb.asm.Label
    import org.objectweb.asm.Opcodes._

    // TODO(JLS): write this with Instructions
    f.visitVarInsn(ALOAD, 0)
    f.visitVarInsn(ASTORE, 1)

    f.visitInsn(ACONST_NULL)
    f.visitVarInsn(ASTORE, 2)

    val loopStart = new Label()
    f.visitLabel(loopStart)
    f.visitVarInsn(ALOAD, 1)
    f.visitVarInsn(ASTORE, 2)
    f.visitVarInsn(ALOAD, 1)
    f.visitMethodInsn(INVOKEVIRTUAL, resultType.contName.internalName, InvokeMethodName, resultType.nothingToContDescriptor)
    f.visitVarInsn(ASTORE, 1)
    f.visitVarInsn(ALOAD, 1)
    f.visitJumpInsn(IFNONNULL, loopStart)

    f.visitVarInsn(ALOAD, 2)
    f.visitFieldInsn(GETFIELD, resultType.contName.internalName, ResultFieldName, resultType.descriptor)
    XRETURN(resultType)(f.asInstanceOf[F[StackNil ** T]])
  }

  private def genRunMethod[T <: PType](resultType: RType[T], contName: JvmName): F[StackNil] => F[StackEnd] = {
    START[StackNil] ~
      THISLOAD(contName, tagOf[PFunction[T]]) ~
      unwindCont(resultType) ~
      XPOP(resultType) ~
      RETURN
  }

}

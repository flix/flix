/*
 * Copyright 2021 Jonathan Lindegaard Starup
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
import ca.uwaterloo.flix.language.ast.RType
import ca.uwaterloo.flix.language.ast.RType._
import ca.uwaterloo.flix.language.phase.sjvm.BytecodeCompiler._
import ca.uwaterloo.flix.language.phase.sjvm.ClassMaker.Mod
import ca.uwaterloo.flix.language.phase.sjvm.Instructions._
import org.objectweb.asm.Opcodes

object GenGlobalCounterClass {

  val NewIdMethodName: String = "newId"
  val NewIdReturnType: RType[PInt64] = RInt64

  private val counterFieldName: String = "counter"
  private val counterFieldType: JvmName = JvmName.Java.AtomicLong

  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    val className = JvmName.Flix.GlobalCounter
    val superClass = JvmName.Java.Object
    Map() + (className -> JvmClass(className, genByteCode(className, superClass)))
  }

  private def genByteCode(className: JvmName, superClass: JvmName)(implicit flix: Flix): Array[Byte] = {
    val classMaker = ClassMaker.mkClass(className, superClass)

    classMaker.mkConstructor(genConstructor(superClass), JvmName.nothingToVoid, Mod.isPrivate)
    classMaker.mkStaticConstructor(genStaticConstructor(className))
    classMaker.mkField(counterFieldName, counterFieldType, Mod.isPrivate.isFinal.isStatic)
    classMaker.mkMethod(genNewIdMethod(className), NewIdMethodName, RInt64.nothingToThisDescriptor, Mod.isPublic.isStatic.isFinal)

    classMaker.closeClassMaker
  }

  private def genConstructor(superClass: JvmName): F[StackNil] => F[StackEnd] = {
    START[StackNil] ~
      preInitALOAD(0, tagOf[PAnyObject]) ~
      invokeSimpleConstructor(superClass) ~
      RETURN
  }

  private def genStaticConstructor(name: JvmName): F[StackNil] => F[StackEnd] = {
    START[StackNil] ~
      NEW(counterFieldType, tagOf[PAnyObject]) ~
      DUP ~
      invokeSimpleConstructor(counterFieldType) ~
      PUTSTATIC(name, counterFieldName, counterFieldType, erasedType = false) ~
      RETURN
  }

  private def genNewIdMethod(name: JvmName)(implicit flix: Flix): F[StackNil] => F[StackEnd] = {
    START[StackNil] ~
      GETSTATIC(name, counterFieldName, counterFieldType, undoErasure = false, tagOf[PAnyObject]) ~
      getAndIncrementAtomicCounter ~
      XRETURN(NewIdReturnType)
  }

  private def getAndIncrementAtomicCounter[R <: Stack]: F[R ** PReference[PAnyObject]] => F[R ** PInt64] = f => {
    f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, JvmName.Java.AtomicLong.internalName, "getAndIncrement", RInt64.nothingToThisDescriptor)
    f.asInstanceOf[F[R ** PInt64]]
  }

}

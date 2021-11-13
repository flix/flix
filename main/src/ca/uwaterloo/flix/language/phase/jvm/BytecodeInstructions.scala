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

package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor
import org.objectweb.asm.{MethodVisitor, Opcodes}

object BytecodeInstructions {

  /**
    * A Frame that represents the Jvm state and contains a visitor to emit code
    */
  sealed class F(visitor: MethodVisitor) {
    def visitTypeInstruction(opcode: Int, tpe: JvmName): Unit =
      visitor.visitTypeInsn(opcode, tpe.toInternalName)

    def visitInstruction(opcode: Int): Unit = visitor.visitInsn(opcode)

    def visitMethodInstruction(opcode: Int, owner: JvmName, methodName: String, descriptor: MethodDescriptor): Unit =
      visitor.visitMethodInsn(opcode, owner.toInternalName, methodName, descriptor.toString, false)

    def visitFieldInstruction(opcode: Int, owner: JvmName, fieldName: String, fieldType: JvmType): Unit =
      visitor.visitFieldInsn(opcode, owner.toInternalName, fieldName, fieldType.toDescriptor)

    def visitVarInstruction(opcode: Int, v: Int): Unit =
      visitor.visitVarInsn(opcode, v)
  }

  type Instruction = F => F

  implicit class ComposeOps(i1: Instruction) {
    def ~(i2: Instruction): Instruction =
      f => i2(i1(f))
  }

  //
  // ~~~~~~~~~~~~~~~~~~~~~~~~ Direct JVM Instructions ~~~~~~~~~~~~~~~~~~~~~~~~
  //

  def ALOAD(index: Int): Instruction = f => {
    f.visitVarInstruction(Opcodes.ALOAD, index)
    f
  }

  def DUP: Instruction = f => {
    f.visitInstruction(Opcodes.DUP)
    f
  }

  def NEW(className: JvmName): Instruction = f => {
    f.visitTypeInstruction(Opcodes.NEW, className)
    f
  }

  def PUTSTATIC(className: JvmName, fieldName: String, fieldType: JvmType): Instruction = f => {
    f.visitFieldInstruction(Opcodes.PUTSTATIC, className, fieldName, fieldType)
    f
  }

  def RETURN: Instruction = f => {
    f.visitInstruction(Opcodes.RETURN)
    f
  }

  //
  // ~~~~~~~~~~~~~~~~~~~~~~~~~ Meta JVM Instructions ~~~~~~~~~~~~~~~~~~~~~~~~~
  //

  def InvokeSimpleConstructor(className: JvmName): Instruction = f => {
    f.visitMethodInstruction(Opcodes.INVOKESPECIAL, className, JvmName.ConstructorMethod, MethodDescriptor.NothingToVoid)
    f
  }
}

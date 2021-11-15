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

  type InstructionSet = F => F

  implicit class ComposeOps(i1: InstructionSet) {
    def ~(i2: InstructionSet): InstructionSet =
      f => i2(i1(f))
  }

  //
  // ~~~~~~~~~~~~~~~~~~~~~~~~ Direct JVM Instructions ~~~~~~~~~~~~~~~~~~~~~~~~
  //

  def ALOAD(index: Int): InstructionSet = f => {
    f.visitVarInstruction(Opcodes.ALOAD, index)
    f
  }

  def DUP(): InstructionSet = f => {
    f.visitInstruction(Opcodes.DUP)
    f
  }

  def GETSTATIC(className: JvmName, fieldName: String, fieldType: JvmType): InstructionSet = f => {
    f.visitFieldInstruction(Opcodes.GETSTATIC, className, fieldName, fieldType)
    f
  }

  def INVOKEVIRTUAL(className: JvmName, methodName: String, descriptor: MethodDescriptor): InstructionSet = f => {
    f.visitMethodInstruction(Opcodes.INVOKEVIRTUAL, className, methodName, descriptor)
    f
  }

  def LRETURN(): InstructionSet = f => {
    f.visitInstruction(Opcodes.LRETURN)
    f
  }

  def NEW(className: JvmName): InstructionSet = f => {
    f.visitTypeInstruction(Opcodes.NEW, className)
    f
  }

  def PUTSTATIC(className: JvmName, fieldName: String, fieldType: JvmType): InstructionSet = f => {
    f.visitFieldInstruction(Opcodes.PUTSTATIC, className, fieldName, fieldType)
    f
  }

  def RETURN(): InstructionSet = f => {
    f.visitInstruction(Opcodes.RETURN)
    f
  }

  //
  // ~~~~~~~~~~~~~~~~~~~~~~~~~ Meta JVM Instructions ~~~~~~~~~~~~~~~~~~~~~~~~~
  //

  def InvokeSimpleConstructor(className: JvmName): InstructionSet = f => {
    f.visitMethodInstruction(Opcodes.INVOKESPECIAL, className, JvmName.ConstructorMethod, MethodDescriptor.NothingToVoid)
    f
  }

  def XReturn(tpe: JvmType): InstructionSet = f => {
    f.visitInstruction(AsmOps.getReturnInstruction(tpe))
    f
  }
}

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
import org.objectweb.asm.{Label, MethodVisitor, Opcodes}

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

    def visitFieldInstruction(opcode: Int, owner: JvmName, fieldName: String, fieldType: BackendType): Unit =
      visitor.visitFieldInsn(opcode, owner.toInternalName, fieldName, fieldType.toDescriptor)

    def visitVarInstruction(opcode: Int, v: Int): Unit =
      visitor.visitVarInsn(opcode, v)

    def visitJumpInstruction(opcode: Int, label: Label): Unit =
      visitor.visitJumpInsn(opcode, label)

    def visitLabel(label: Label): Unit =
      visitor.visitLabel(label)

    def visitLoadConstantInstruction(v: Any): Unit =
      visitor.visitLdcInsn(v)

    def cheat(command: MethodVisitor => Unit): Unit = command(visitor)
  }

  type InstructionSet = F => F

  implicit class ComposeOps(i1: InstructionSet) {
    def ~(i2: InstructionSet): InstructionSet =
      f => i2(i1(f))
  }

  //
  // ~~~~~~~~~~~~~~~~~~~~~~~~ Direct JVM Instructions ~~~~~~~~~~~~~~~~~~~~~~~~~
  //

  def AASTORE(): InstructionSet = f => {
    f.visitInstruction(Opcodes.AASTORE)
    f
  }

  def ALOAD(index: Int): InstructionSet = f => {
    f.visitVarInstruction(Opcodes.ALOAD, index)
    f
  }

  // TODO: All JvmNames could be BackendObjTypes
  def ANEWARRAY(className: JvmName): InstructionSet = f => {
    f.visitTypeInstruction(Opcodes.ANEWARRAY, className)
    f
  }

  def ARETURN(): InstructionSet = f => {
    f.visitInstruction(Opcodes.ARETURN)
    f
  }

  def ASTORE(index: Int): InstructionSet = f => {
    f.visitVarInstruction(Opcodes.ASTORE, index)
    f
  }

  def CHECKCAST(className: JvmName): InstructionSet = f => {
    f.visitTypeInstruction(Opcodes.CHECKCAST, className)
    f
  }

  def DLOAD(index: Int): InstructionSet = f => {
    f.visitVarInstruction(Opcodes.DLOAD, index)
    f
  }

  def DRETURN(): InstructionSet = f => {
    f.visitInstruction(Opcodes.DRETURN)
    f
  }

  def DSTORE(index: Int): InstructionSet = f => {
    f.visitVarInstruction(Opcodes.DSTORE, index)
    f
  }

  def DUP(): InstructionSet = f => {
    f.visitInstruction(Opcodes.DUP)
    f
  }

  def FLOAD(index: Int): InstructionSet = f => {
    f.visitVarInstruction(Opcodes.FLOAD, index)
    f
  }

  def FRETURN(): InstructionSet = f => {
    f.visitInstruction(Opcodes.FRETURN)
    f
  }

  def FSTORE(index: Int): InstructionSet = f => {
    f.visitVarInstruction(Opcodes.FSTORE, index)
    f
  }

  def GETFIELD(className: JvmName, fieldName: String, fieldType: BackendType): InstructionSet = f => {
    f.visitFieldInstruction(Opcodes.GETFIELD, className, fieldName, fieldType)
    f
  }

  def GETSTATIC(className: JvmName, fieldName: String, fieldType: BackendType): InstructionSet = f => {
    f.visitFieldInstruction(Opcodes.GETSTATIC, className, fieldName, fieldType)
    f
  }

  def ICONST_0(): InstructionSet = f => {
    f.visitInstruction(Opcodes.ICONST_0)
    f
  }

  def ICONST_1(): InstructionSet = f => {
    f.visitInstruction(Opcodes.ICONST_1)
    f
  }

  def IF_ACMPNE(trueBranch: InstructionSet)(falseBranch: InstructionSet): InstructionSet =
    branch(Opcodes.IF_ACMPNE)(trueBranch)(falseBranch)

  def IFNULL(trueBranch: InstructionSet)(falseBranch: InstructionSet): InstructionSet =
    branch(Opcodes.IFNULL)(trueBranch)(falseBranch)

  def ILOAD(index: Int): InstructionSet = f => {
    f.visitVarInstruction(Opcodes.ILOAD, index)
    f
  }

  def INVOKESPECIAL(className: JvmName, methodName: String, descriptor: MethodDescriptor): InstructionSet = f => {
    f.visitMethodInstruction(Opcodes.INVOKESPECIAL, className, methodName, descriptor)
    f
  }

  def INVOKESTATIC(className: JvmName, methodName: String, descriptor: MethodDescriptor): InstructionSet = f => {
    f.visitMethodInstruction(Opcodes.INVOKESTATIC, className, methodName, descriptor)
    f
  }

  def INVOKEVIRTUAL(className: JvmName, methodName: String, descriptor: MethodDescriptor): InstructionSet = f => {
    f.visitMethodInstruction(Opcodes.INVOKEVIRTUAL, className, methodName, descriptor)
    f
  }

  def IRETURN(): InstructionSet = f => {
    f.visitInstruction(Opcodes.IRETURN)
    f
  }

  def ISTORE(index: Int): InstructionSet = f => {
    f.visitVarInstruction(Opcodes.ISTORE, index)
    f
  }

  def LLOAD(index: Int): InstructionSet = f => {
    f.visitVarInstruction(Opcodes.LLOAD, index)
    f
  }

  def LRETURN(): InstructionSet = f => {
    f.visitInstruction(Opcodes.LRETURN)
    f
  }

  def LSTORE(index: Int): InstructionSet = f => {
    f.visitVarInstruction(Opcodes.LSTORE, index)
    f
  }

  def NEW(className: JvmName): InstructionSet = f => {
    f.visitTypeInstruction(Opcodes.NEW, className)
    f
  }

  def PUTFIELD(className: JvmName, fieldName: String, fieldType: BackendType): InstructionSet = f => {
    f.visitFieldInstruction(Opcodes.PUTFIELD, className, fieldName, fieldType)
    f
  }

  def PUTSTATIC(className: JvmName, fieldName: String, fieldType: BackendType): InstructionSet = f => {
    f.visitFieldInstruction(Opcodes.PUTSTATIC, className, fieldName, fieldType)
    f
  }

  def RETURN(): InstructionSet = f => {
    f.visitInstruction(Opcodes.RETURN)
    f
  }

  //
  // ~~~~~~~~~~~~~~~~~~~~~~~~~ Meta JVM Instructions ~~~~~~~~~~~~~~~~~~~~~~~~~~
  //

  def cheat(command: MethodVisitor => Unit): InstructionSet = f => {
    f.cheat(command)
    f
  }

  def invokeConstructor(className: JvmName, descriptor: MethodDescriptor = MethodDescriptor.NothingToVoid): InstructionSet =
    INVOKESPECIAL(className, JvmName.ConstructorMethod, descriptor)

  def pushBool(b: Boolean): InstructionSet =
    if (b) ICONST_1() else ICONST_0()

  def pushString(s: String): InstructionSet = f => {
    f.visitLoadConstantInstruction(s)
    f
  }

  def xLoad(tpe: BackendType, index: Int): InstructionSet = tpe match {
    case BackendType.Bool | BackendType.Char | BackendType.Int8 | BackendType.Int16 | BackendType.Int32 => ILOAD(index)
    case BackendType.Int64 => LLOAD(index)
    case BackendType.Float32 => FLOAD(index)
    case BackendType.Float64 => DLOAD(index)
    case BackendType.Array(_) | BackendType.Reference(_) => ALOAD(index)
  }

  def xReturn(tpe: BackendType): InstructionSet = tpe match {
    case BackendType.Bool | BackendType.Char | BackendType.Int8 | BackendType.Int16 | BackendType.Int32 => IRETURN()
    case BackendType.Int64 => LRETURN()
    case BackendType.Float32 => FRETURN()
    case BackendType.Float64 => DRETURN()
    case BackendType.Array(_) | BackendType.Reference(_) => ARETURN()
  }

  //
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Private ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //

  private def branch(opcode: Int)(trueBranch: InstructionSet)(falseBranch: InstructionSet): InstructionSet = f0 => {
    var f = f0
    val jumpLabel = new Label()
    val skipLabel = new Label()
    f.visitJumpInstruction(opcode, jumpLabel)

    f = falseBranch(f)
    f.visitJumpInstruction(Opcodes.GOTO, skipLabel)

    f.visitLabel(jumpLabel)
    f = trueBranch(f)
    f.visitLabel(skipLabel)
    f
  }
}

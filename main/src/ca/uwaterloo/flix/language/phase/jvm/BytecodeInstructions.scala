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

import ca.uwaterloo.flix.language.phase.jvm.BytecodeInstructions.Branch.{FalseBranch, TrueBranch}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker._
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor.mkDescriptor
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
      visitor.visitMethodInsn(opcode, owner.toInternalName, methodName, descriptor.toDescriptor, opcode == Opcodes.INVOKEINTERFACE)

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
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Structures ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //

  sealed trait Condition

  object Condition {
    case object ACMPEQ extends Condition

    case object ACMPNE extends Condition

    case object Bool extends Condition

    case object EQ extends Condition

    case object ICMPEQ extends Condition

    case object ICMPNE extends Condition

    case object NE extends Condition

    case object NONNULL extends Condition

    case object NULL extends Condition
  }

  sealed trait Branch

  object Branch {
    case object TrueBranch extends Branch

    case object FalseBranch extends Branch
  }

  // TODO: do this for methods
  class Variable(tpe: BackendType, index: Int) {
    def load(): InstructionSet = xLoad(tpe, index)

    def store(): InstructionSet = xStore(tpe, index)
  }

  //
  // ~~~~~~~~~~~~~~~~~~~~~~~~ Direct JVM Instructions ~~~~~~~~~~~~~~~~~~~~~~~~~
  //

  def AASTORE(): InstructionSet = f => {
    f.visitInstruction(Opcodes.AASTORE)
    f
  }

  def ACONST_NULL(): InstructionSet = f => {
    f.visitInstruction(Opcodes.ACONST_NULL)
    f
  }

  def ALOAD(index: Int): InstructionSet = f => {
    f.visitVarInstruction(Opcodes.ALOAD, index)
    f
  }

  def ANEWARRAY(className: JvmName): InstructionSet = f => {
    f.visitTypeInstruction(Opcodes.ANEWARRAY, className)
    f
  }

  def ARETURN(): InstructionSet = f => {
    f.visitInstruction(Opcodes.ARETURN)
    f
  }

  def ARRAYLENGTH(): InstructionSet = f => {
    f.visitInstruction(Opcodes.ARRAYLENGTH)
    f
  }

  def ASTORE(index: Int): InstructionSet = f => {
    f.visitVarInstruction(Opcodes.ASTORE, index)
    f
  }

  def ATHROW(): InstructionSet = f => {
    f.visitInstruction(Opcodes.ATHROW)
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

  def GETFIELD(field: InstanceField): InstructionSet = f => {
    f.visitFieldInstruction(Opcodes.GETFIELD, field.clazz, field.name, field.tpe)
    f
  }

  def GETSTATIC(field: StaticField): InstructionSet = f => {
    f.visitFieldInstruction(Opcodes.GETSTATIC, field.clazz, field.name, field.tpe)
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

  def ICONST_2(): InstructionSet = f => {
    f.visitInstruction(Opcodes.ICONST_2)
    f
  }

  def ICONST_3(): InstructionSet = f => {
    f.visitInstruction(Opcodes.ICONST_3)
    f
  }

  def ICONST_4(): InstructionSet = f => {
    f.visitInstruction(Opcodes.ICONST_4)
    f
  }

  def ICONST_5(): InstructionSet = f => {
    f.visitInstruction(Opcodes.ICONST_5)
    f
  }

  def ILOAD(index: Int): InstructionSet = f => {
    f.visitVarInstruction(Opcodes.ILOAD, index)
    f
  }

  def INVOKEINTERFACE(interfaceName: JvmName, methodName: String, descriptor: MethodDescriptor): InstructionSet = f => {
    f.visitMethodInstruction(Opcodes.INVOKEINTERFACE, interfaceName, methodName, descriptor)
    f
  }

  def INVOKEINTERFACE(m: InterfaceMethod): InstructionSet = f => {
    f.visitMethodInstruction(Opcodes.INVOKEINTERFACE, m.clazz, m.name, m.d)
    f
  }

  def INVOKESPECIAL(className: JvmName, methodName: String, descriptor: MethodDescriptor): InstructionSet = f => {
    f.visitMethodInstruction(Opcodes.INVOKESPECIAL, className, methodName, descriptor)
    f
  }

  def INVOKESPECIAL(c: ConstructorMethod): InstructionSet = f => {
    f.visitMethodInstruction(Opcodes.INVOKESPECIAL, c.clazz, c.name, c.d)
    f
  }

  def INVOKESTATIC(className: JvmName, methodName: String, descriptor: MethodDescriptor): InstructionSet = f => {
    f.visitMethodInstruction(Opcodes.INVOKESTATIC, className, methodName, descriptor)
    f
  }

  def INVOKESTATIC(m: StaticMethod): InstructionSet = f => {
    f.visitMethodInstruction(Opcodes.INVOKESTATIC, m.clazz, m.name, m.d)
    f
  }

  def INVOKEVIRTUAL(className: JvmName, methodName: String, descriptor: MethodDescriptor): InstructionSet = f => {
    f.visitMethodInstruction(Opcodes.INVOKEVIRTUAL, className, methodName, descriptor)
    f
  }

  def INVOKEVIRTUAL(m: AbstractMethod): InstructionSet = f => {
    f.visitMethodInstruction(Opcodes.INVOKEVIRTUAL, m.clazz, m.name, m.d)
    f
  }

  def INVOKEVIRTUAL(m: InstanceMethod): InstructionSet = f => {
    f.visitMethodInstruction(Opcodes.INVOKEVIRTUAL, m.clazz, m.name, m.d)
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

  def POP(): InstructionSet = f => {
    f.visitInstruction(Opcodes.POP)
    f
  }

  def POP2(): InstructionSet = f => {
    f.visitInstruction(Opcodes.POP2)
    f
  }

  def PUTFIELD(field: InstanceField): InstructionSet = f => {
    f.visitFieldInstruction(Opcodes.PUTFIELD, field.clazz, field.name, field.tpe)
    f
  }

  def PUTSTATIC(field: StaticField): InstructionSet = f => {
    f.visitFieldInstruction(Opcodes.PUTSTATIC, field.clazz, field.name, field.tpe)
    f
  }

  def RETURN(): InstructionSet = f => {
    f.visitInstruction(Opcodes.RETURN)
    f
  }

  //
  // ~~~~~~~~~~~~~~~~~~~~~~~~~ Meta JVM Instructions ~~~~~~~~~~~~~~~~~~~~~~~~~~
  //

  def branch(c: Condition)(cases: Branch => InstructionSet): InstructionSet = f0 => {
    var f = f0
    val jumpLabel = new Label()
    val skipLabel = new Label()
    f.visitJumpInstruction(opcodeOf(c), jumpLabel)

    f = cases(FalseBranch)(f)
    f.visitJumpInstruction(Opcodes.GOTO, skipLabel)

    f.visitLabel(jumpLabel)
    f = cases(TrueBranch)(f)
    f.visitLabel(skipLabel)
    f
  }

  def cheat(command: MethodVisitor => Unit): InstructionSet = f => {
    f.cheat(command)
    f
  }

  def doWhile(c: Condition)(i: InstructionSet): InstructionSet = f0 => {
    var f = f0
    val start = new Label()
    f.visitLabel(start)
    f = i(f)
    f.visitJumpInstruction(opcodeOf(c), start)
    f
  }

  def ifTrue(c: Condition)(i: InstructionSet): InstructionSet = f0 => {
    var f = f0
    val jumpLabel = new Label()
    f.visitJumpInstruction(opcodeOf(negated(c)), jumpLabel)
    f = i(f)
    f.visitLabel(jumpLabel)
    f
  }

  def invokeConstructor(className: JvmName, descriptor: MethodDescriptor): InstructionSet =
    INVOKESPECIAL(className, JvmName.ConstructorMethod, descriptor)

  def nop(): InstructionSet =
    f => f

  def pushBool(b: Boolean): InstructionSet =
    if (b) ICONST_1() else ICONST_0()

  def pushNull(): InstructionSet =
    ACONST_NULL()

  def pushString(s: String): InstructionSet = f => {
    f.visitLoadConstantInstruction(s)
    f
  }

  def storeWithName(index: Int, tpe: BackendType)(body: Variable => InstructionSet): InstructionSet =
    xStore(tpe, index) ~ body(new Variable(tpe, index))

  // TODO: this should be "wrong" if used on F in a static context
  def thisLoad(): InstructionSet =
    ALOAD(0)

  def throwUnsupportedOperationException(msg: String): InstructionSet =
    NEW(JvmName.UnsupportedOperationException) ~
      DUP() ~
      pushString(msg) ~
      INVOKESPECIAL(JvmName.UnsupportedOperationException, JvmName.ConstructorMethod,
        mkDescriptor(BackendObjType.String.toTpe)(VoidableType.Void)) ~
      ATHROW()

  def withName(index: Int, tpe: BackendType)(body: Variable => InstructionSet): InstructionSet =
    body(new Variable(tpe, index))

  def xLoad(tpe: BackendType, index: Int): InstructionSet = tpe match {
    case BackendType.Bool | BackendType.Char | BackendType.Int8 | BackendType.Int16 | BackendType.Int32 => ILOAD(index)
    case BackendType.Int64 => LLOAD(index)
    case BackendType.Float32 => FLOAD(index)
    case BackendType.Float64 => DLOAD(index)
    case BackendType.Array(_) | BackendType.Reference(_) => ALOAD(index)
  }

  /**
    * Pops the top of the stack using `POP` or `POP2` depending on the value size.
    */
  def xPop(tpe: BackendType): InstructionSet = tpe match {
    case BackendType.Bool | BackendType.Char | BackendType.Int8 | BackendType.Int16 | BackendType.Int32 |
         BackendType.Float32 | BackendType.Array(_) | BackendType.Reference(_) => POP()
    case BackendType.Int64 | BackendType.Float64 => POP2()
  }

  def xReturn(tpe: BackendType): InstructionSet = tpe match {
    case BackendType.Bool | BackendType.Char | BackendType.Int8 | BackendType.Int16 | BackendType.Int32 => IRETURN()
    case BackendType.Int64 => LRETURN()
    case BackendType.Float32 => FRETURN()
    case BackendType.Float64 => DRETURN()
    case BackendType.Array(_) | BackendType.Reference(_) => ARETURN()
  }

  def xStore(tpe: BackendType, index: Int): InstructionSet = tpe match {
    case BackendType.Bool | BackendType.Char | BackendType.Int8 | BackendType.Int16 | BackendType.Int32 => ISTORE(index)
    case BackendType.Int64 => LSTORE(index)
    case BackendType.Float32 => FSTORE(index)
    case BackendType.Float64 => DSTORE(index)
    case BackendType.Array(_) | BackendType.Reference(_) => ASTORE(index)
  }

  //
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Private ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //

  private def opcodeOf(c: Condition): Int = c match {
    case Condition.ACMPEQ => Opcodes.IF_ACMPEQ
    case Condition.ACMPNE => Opcodes.IF_ACMPNE
    case Condition.Bool => opcodeOf(Condition.NE)
    case Condition.EQ => Opcodes.IFEQ
    case Condition.ICMPEQ => Opcodes.IF_ICMPEQ
    case Condition.ICMPNE => Opcodes.IF_ICMPNE
    case Condition.NE => Opcodes.IFNE
    case Condition.NONNULL => Opcodes.IFNONNULL
    case Condition.NULL => Opcodes.IFNULL
  }

  private def negated(c: Condition): Condition = c match {
    case Condition.ACMPEQ => Condition.ACMPNE
    case Condition.ACMPNE => Condition.ACMPEQ
    case Condition.Bool => negated(Condition.NE)
    case Condition.EQ => Condition.NE
    case Condition.ICMPEQ => Condition.ICMPNE
    case Condition.ICMPNE => Condition.ICMPEQ
    case Condition.NE => Condition.EQ
    case Condition.NONNULL => Condition.NULL
    case Condition.NULL => Condition.NONNULL
  }
}

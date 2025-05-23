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
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.*
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor.mkDescriptor
import ca.uwaterloo.flix.util.collection.Chain
import org.objectweb.asm
import org.objectweb.asm.{Label, MethodVisitor, Opcodes}

import scala.annotation.tailrec

object BytecodeInstructions {

  /**
    * A Frame that represents the Jvm state and contains a visitor to emit code
    */
  sealed class F(visitor: MethodVisitor) {
    def visitTypeInstruction(opcode: Int, tpe: JvmName): Unit =
      visitor.visitTypeInsn(opcode, tpe.toInternalName)

    def visitInstruction(opcode: Int): Unit = visitor.visitInsn(opcode)

    def visitMethodInstruction(opcode: Int, owner: JvmName, methodName: String, descriptor: MethodDescriptor, isInterface: Boolean): Unit =
      visitor.visitMethodInsn(opcode, owner.toInternalName, methodName, descriptor.toDescriptor, isInterface)

    // TODO: sanitize varags
    def visitInvokeDynamicInstruction(methodName: String, descriptor: MethodDescriptor, bootstrapMethodHandle: Handle, bootstrapMethodArguments: Any*): Unit =
      visitor.visitInvokeDynamicInsn(methodName, descriptor.toDescriptor, bootstrapMethodHandle.handle, bootstrapMethodArguments *)

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

    def visitIntInstruction(opcode: Int, v: Int): Unit =
      visitor.visitIntInsn(opcode, v)

    def visitTryCatchBlock(beforeTry: Label, afterTry: Label, handlerStart: Label): Unit =
      visitor.visitTryCatchBlock(beforeTry, afterTry, handlerStart, null)

    def cheat(command: MethodVisitor => Unit): Unit = command(visitor)
  }

  type InstructionSet = Chain[Instruction]

  sealed trait Instruction

  object Instruction {
    case class Cheat(command: MethodVisitor => Unit) extends Instruction

    case class FieldIns(opcode: Int, clazz: JvmName, name: String, tpe: BackendType) extends Instruction

    case class Ins(opcode: Int) extends Instruction

    case class IntIns(opcode: Int, value: Int) extends Instruction

    case class InvokeDynamicIns(methodName: String, descriptor: MethodDescriptor, bootstrapMethodHandle: Handle, bootstrapMethodArguments: List[Any]) extends Instruction

    case class JumpIns(opcode: Int, label: Label) extends Instruction

    case class LoadConstantIns(constant: Any) extends Instruction

    case class MethodIns(opcode: Int, clazz: JvmName, name: String, descriptor: MethodDescriptor, isInterface: Boolean) extends Instruction

    case class PlaceLabel(label: Label) extends Instruction

    case class TryCatchIns(beforeTry: Label, afterTry: Label, handlerStart: Label) extends Instruction

    case class TypeIns(opcode: Int, clazz: JvmName) extends Instruction

    case class VarIns(opcode: Int, index: Int) extends Instruction
  }

  /**
    * Returns the sequential composition of the two instructions.
    */
  def compose(i1: InstructionSet, i2: InstructionSet): InstructionSet =
    i1 ++ i2

  implicit class ComposeOps(i1: InstructionSet) {
    def ~(i2: InstructionSet): InstructionSet =
      compose(i1, i2)
  }

  def visit(f: F, ins: Instruction): Unit = ins match {
    case Instruction.Cheat(command) => f.cheat(command)
    case Instruction.FieldIns(opcode, clazz, name, tpe) => f.visitFieldInstruction(opcode, clazz, name, tpe)
    case Instruction.Ins(opcode) => f.visitInstruction(opcode)
    case Instruction.IntIns(opcode, value) => f.visitIntInstruction(opcode, value)
    case Instruction.InvokeDynamicIns(methodName, descriptor, bootstrapMethodHandle, bootstrapMethodArguments) => f.visitInvokeDynamicInstruction(methodName, descriptor, bootstrapMethodHandle, bootstrapMethodArguments *)
    case Instruction.JumpIns(opcode, label) => f.visitJumpInstruction(opcode, label)
    case Instruction.LoadConstantIns(constant) => f.visitLoadConstantInstruction(constant)
    case Instruction.MethodIns(opcode, clazz, name, descriptor, isInterface) => f.visitMethodInstruction(opcode, clazz, name, descriptor, isInterface)
    case Instruction.PlaceLabel(label) => f.visitLabel(label)
    case Instruction.TryCatchIns(beforeTry, afterTry, handlerStart) => f.visitTryCatchBlock(beforeTry, afterTry, handlerStart)
    case Instruction.TypeIns(opcode, clazz) => f.visitTypeInstruction(opcode, clazz)
    case Instruction.VarIns(opcode, index) => f.visitVarInstruction(opcode, index)
  }

  implicit class MethodEnricher(mv: MethodVisitor) {
    def visitByteIns(ins: InstructionSet): Unit = {
      val f = new F(mv)
      ins.foreach(visit(f, _))
    }
  }

  sealed case class Handle(handle: asm.Handle)

  def mkStaticHandle(m: StaticMethod): Handle = {
    Handle(new asm.Handle(Opcodes.H_INVOKESTATIC, m.clazz.toInternalName, m.name, m.d.toDescriptor, false))
  }

  def mkStaticHandle(m: StaticInterfaceMethod): Handle = {
    Handle(new asm.Handle(Opcodes.H_INVOKESTATIC, m.clazz.toInternalName, m.name, m.d.toDescriptor, true))
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

    case object LT extends Condition

    case object LE extends Condition

    case object GT extends Condition

    case object GE extends Condition

    case object NONNULL extends Condition

    case object NULL extends Condition
  }

  sealed trait Branch

  object Branch {
    case object TrueBranch extends Branch

    case object FalseBranch extends Branch
  }

  // TODO: do this for methods
  class Variable(val tpe: BackendType, index: Int) {
    def load(): InstructionSet = xLoad(tpe, index)

    def store(): InstructionSet = xStore(tpe, index)
  }

  //
  // ~~~~~~~~~~~~~~~~~~~~~~~~ Direct JVM Instructions ~~~~~~~~~~~~~~~~~~~~~~~~~
  //

  def AASTORE(): InstructionSet = Chain(Instruction.Ins(Opcodes.AASTORE))

  def ACONST_NULL(): InstructionSet = Chain(Instruction.Ins(Opcodes.ACONST_NULL))

  def ALOAD(index: Int): InstructionSet = Chain(Instruction.VarIns(Opcodes.ALOAD, index))

  def ANEWARRAY(className: JvmName): InstructionSet = Chain(Instruction.TypeIns(Opcodes.ANEWARRAY, className))

  def ARETURN(): InstructionSet = Chain(Instruction.Ins(Opcodes.ARETURN))

  def ARRAYLENGTH(): InstructionSet = Chain(Instruction.Ins(Opcodes.ARRAYLENGTH))

  def ASTORE(index: Int): InstructionSet = Chain(Instruction.VarIns(Opcodes.ASTORE, index))

  def ATHROW(): InstructionSet = Chain(Instruction.Ins(Opcodes.ATHROW))

  def BIPUSH(i: Byte): InstructionSet = Chain(Instruction.IntIns(Opcodes.BIPUSH, i))

  def CHECKCAST(className: JvmName): InstructionSet = Chain(Instruction.TypeIns(Opcodes.CHECKCAST, className))

  def DLOAD(index: Int): InstructionSet = Chain(Instruction.VarIns(Opcodes.DLOAD, index))

  def DRETURN(): InstructionSet = Chain(Instruction.Ins(Opcodes.DRETURN))

  def DSTORE(index: Int): InstructionSet = Chain(Instruction.VarIns(Opcodes.DSTORE, index))

  def DUP(): InstructionSet = Chain(Instruction.Ins(Opcodes.DUP))

  def DUP2(): InstructionSet = Chain(Instruction.Ins(Opcodes.DUP2))

  def DUP_X1(): InstructionSet = Chain(Instruction.Ins(Opcodes.DUP_X1))

  def DUP_X2(): InstructionSet = Chain(Instruction.Ins(Opcodes.DUP_X2))

  def DUP2_X1(): InstructionSet = Chain(Instruction.Ins(Opcodes.DUP2_X1))

  def DUP2_X2(): InstructionSet = Chain(Instruction.Ins(Opcodes.DUP2_X2))

  def FLOAD(index: Int): InstructionSet = Chain(Instruction.VarIns(Opcodes.FLOAD, index))

  def FRETURN(): InstructionSet = Chain(Instruction.Ins(Opcodes.FRETURN))

  def FSTORE(index: Int): InstructionSet = Chain(Instruction.VarIns(Opcodes.FSTORE, index))

  def GETFIELD(field: InstanceField): InstructionSet = Chain(Instruction.FieldIns(Opcodes.GETFIELD, field.clazz, field.name, field.tpe))

  def GETSTATIC(field: StaticField): InstructionSet = Chain(Instruction.FieldIns(Opcodes.GETSTATIC, field.clazz, field.name, field.tpe))

  def IADD(): InstructionSet = Chain(Instruction.Ins(Opcodes.IADD))

  def ICONST_0(): InstructionSet = Chain(Instruction.Ins(Opcodes.ICONST_0))

  def ICONST_1(): InstructionSet = Chain(Instruction.Ins(Opcodes.ICONST_1))

  def ICONST_2(): InstructionSet = Chain(Instruction.Ins(Opcodes.ICONST_2))

  def ICONST_3(): InstructionSet = Chain(Instruction.Ins(Opcodes.ICONST_3))

  def ICONST_4(): InstructionSet = Chain(Instruction.Ins(Opcodes.ICONST_4))

  def ICONST_5(): InstructionSet = Chain(Instruction.Ins(Opcodes.ICONST_5))

  def ICONST_M1(): InstructionSet = Chain(Instruction.Ins(Opcodes.ICONST_M1))

  def ILOAD(index: Int): InstructionSet = Chain(Instruction.VarIns(Opcodes.ILOAD, index))

  def INSTANCEOF(tpe: JvmName): InstructionSet = Chain(Instruction.TypeIns(Opcodes.INSTANCEOF, tpe))

  /**
    * Make an object which the functional interface of `lambdaMethod`. The
    * implementation of the functional method will be the static method
    * represented by `callHandle`. `callD` is the method descriptor of the
    * static method.
    *
    * `drop` is used for partial application of the static function.
    * Lets say you want to implement the functional interface method of
    * `Function<String, String>` with the partial application of the static
    * function `String example(String, String)` with `"Hi"`. Then you can
    * partially apply the leftmost argument by having `drop = 1`. This then
    * means that the instruction returned will expect the missing string
    * argument on the op stack.
    *
    * for a function with `k` arguments, `drop = n` means that given the first
    * `k-n` arguments on the op stack, this will represent a function of the
    * last `n` arguments to the original return type. This must of course
    * correspond to the type of `lambdaMethod`.
    */
  def mkStaticLambda(lambdaMethod: InterfaceMethod, callD: MethodDescriptor, callHandle: Handle, drop: Int): InstructionSet = Chain(
    Instruction.InvokeDynamicIns(
      lambdaMethod.name,
      mkDescriptor(callD.arguments.dropRight(drop) *)(lambdaMethod.clazz.toTpe),
      mkStaticHandle(BackendObjType.LambdaMetaFactory.MetaFactoryMethod),
      List(
        lambdaMethod.d.toAsmType,
        callHandle.handle,
        lambdaMethod.d.toAsmType
      )
    )
  )

  def mkStaticLambda(lambdaMethod: InterfaceMethod, call: StaticMethod, drop: Int): InstructionSet =
    mkStaticLambda(lambdaMethod, call.d, mkStaticHandle(call), drop)

  def mkStaticLambda(lambdaMethod: InterfaceMethod, call: StaticInterfaceMethod, drop: Int): InstructionSet =
    mkStaticLambda(lambdaMethod, call.d, mkStaticHandle(call), drop)

  def INVOKEINTERFACE(interfaceName: JvmName, methodName: String, descriptor: MethodDescriptor): InstructionSet = Chain(Instruction.MethodIns(Opcodes.INVOKEINTERFACE, interfaceName, methodName, descriptor, isInterface = true))

  def INVOKEINTERFACE(m: InterfaceMethod): InstructionSet = Chain(Instruction.MethodIns(Opcodes.INVOKEINTERFACE, m.clazz, m.name, m.d, isInterface = true))

  def INVOKESPECIAL(className: JvmName, methodName: String, descriptor: MethodDescriptor): InstructionSet = {
    val isInterface = false // OBS this is not technically true if you use it to call private interface methods(?)
    Chain(Instruction.MethodIns(Opcodes.INVOKESPECIAL, className, methodName, descriptor, isInterface = isInterface))
  }

  def INVOKESPECIAL(c: ConstructorMethod): InstructionSet = Chain(Instruction.MethodIns(Opcodes.INVOKESPECIAL, c.clazz, c.name, c.d, isInterface = false))

  def INVOKESTATIC(className: JvmName, methodName: String, descriptor: MethodDescriptor, isInterface: Boolean = false): InstructionSet = Chain(Instruction.MethodIns(Opcodes.INVOKESTATIC, className, methodName, descriptor, isInterface))

  def INVOKESTATIC(m: StaticMethod): InstructionSet = Chain(Instruction.MethodIns(Opcodes.INVOKESTATIC, m.clazz, m.name, m.d, isInterface = false))

  def INVOKESTATIC(m: StaticInterfaceMethod): InstructionSet = Chain(Instruction.MethodIns(Opcodes.INVOKESTATIC, m.clazz, m.name, m.d, isInterface = true))

  def INVOKEVIRTUAL(className: JvmName, methodName: String, descriptor: MethodDescriptor, isInterface: Boolean = false): InstructionSet = Chain(Instruction.MethodIns(Opcodes.INVOKEVIRTUAL, className, methodName, descriptor, isInterface))

  def INVOKEVIRTUAL(m: AbstractMethod): InstructionSet = Chain(Instruction.MethodIns(Opcodes.INVOKEVIRTUAL, m.clazz, m.name, m.d, isInterface = false))

  def INVOKEVIRTUAL(m: InstanceMethod): InstructionSet = Chain(Instruction.MethodIns(Opcodes.INVOKEVIRTUAL, m.clazz, m.name, m.d, isInterface = false))

  def IRETURN(): InstructionSet = Chain(Instruction.Ins(Opcodes.IRETURN))

  def ISTORE(index: Int): InstructionSet = Chain(Instruction.VarIns(Opcodes.ISTORE, index))

  def LCMP(): InstructionSet = Chain(Instruction.Ins(Opcodes.LCMP))

  def LCONST_0(): InstructionSet = Chain(Instruction.Ins(Opcodes.LCONST_0))

  def LCONST_1(): InstructionSet = Chain(Instruction.Ins(Opcodes.LCONST_1))

  def LLOAD(index: Int): InstructionSet = Chain(Instruction.VarIns(Opcodes.LLOAD, index))

  def LRETURN(): InstructionSet = Chain(Instruction.Ins(Opcodes.LRETURN))

  def LSTORE(index: Int): InstructionSet = Chain(Instruction.VarIns(Opcodes.LSTORE, index))

  def NEW(className: JvmName): InstructionSet = Chain(Instruction.TypeIns(Opcodes.NEW, className))

  def POP(): InstructionSet = Chain(Instruction.Ins(Opcodes.POP))

  def POP2(): InstructionSet = Chain(Instruction.Ins(Opcodes.POP2))

  def PUTFIELD(field: InstanceField): InstructionSet = Chain(Instruction.FieldIns(Opcodes.PUTFIELD, field.clazz, field.name, field.tpe))

  def PUTSTATIC(field: StaticField): InstructionSet = Chain(Instruction.FieldIns(Opcodes.PUTSTATIC, field.clazz, field.name, field.tpe))

  def RETURN(): InstructionSet = Chain(Instruction.Ins(Opcodes.RETURN))

  def SIPUSH(i: Short): InstructionSet = Chain(Instruction.IntIns(Opcodes.SIPUSH, i))

  def SWAP(): InstructionSet = Chain(Instruction.Ins(Opcodes.SWAP))

  //
  // ~~~~~~~~~~~~~~~~~~~~~~~~~ Meta JVM Instructions ~~~~~~~~~~~~~~~~~~~~~~~~~~
  //

  def branch(c: Condition)(cases: Branch => InstructionSet): InstructionSet = {
    var ins: Chain[Instruction] = Chain.empty
    val jumpLabel = new Label()
    val skipLabel = new Label()
    ins :+= Instruction.JumpIns(opcodeOf(c), jumpLabel)

    ins ++= cases(FalseBranch)
    ins :+= Instruction.JumpIns(Opcodes.GOTO, skipLabel)

    ins :+= Instruction.PlaceLabel(jumpLabel)
    ins ++= cases(TrueBranch)
    ins :+= Instruction.PlaceLabel(skipLabel)
    ins
  }

  def cheat(command: MethodVisitor => Unit): InstructionSet = Chain(Instruction.Cheat(command))

  /** do { body } while(c) */
  def doWhile(c: Condition)(body: InstructionSet): InstructionSet = {
    var ins: Chain[Instruction] = Chain.empty
    val start = new Label()
    ins :+= Instruction.PlaceLabel(start)
    ins ++= body
    ins :+= Instruction.JumpIns(opcodeOf(c), start)
    ins
  }

  /// while(c(t)) { body }
  def whileLoop(c: Condition)(t: InstructionSet)(body: InstructionSet): InstructionSet = {
    var ins: Chain[Instruction] = Chain.empty
    val startLabel = new Label()
    val doneLabel = new Label()
    ins :+= Instruction.PlaceLabel(startLabel)
    ins ++= t
    ins :+= Instruction.JumpIns(opcodeOf(negated(c)), doneLabel)
    ins ++= body
    ins :+= Instruction.JumpIns(Opcodes.GOTO, startLabel)
    ins :+= Instruction.PlaceLabel(doneLabel)
    ins
  }

  def ifCondition(c: Condition)(body: InstructionSet): InstructionSet = {
    var ins: Chain[Instruction] = Chain.empty
    val jumpLabel = new Label()
    ins :+= Instruction.JumpIns(opcodeOf(negated(c)), jumpLabel)
    ins ++= body
    ins :+= Instruction.PlaceLabel(jumpLabel)
    ins
  }

  /**
    * Using [[ifCondition]] uses less jumps, so use that if the conditional code
    * is returns or throws
    */
  def ifConditionElse(c: Condition)(thenn: InstructionSet)(otherwise: InstructionSet): InstructionSet = {
    var ins: Chain[Instruction] = Chain.empty
    val conditionLabel = new Label()
    val endLabel = new Label()
    ins :+= Instruction.JumpIns(opcodeOf(c), conditionLabel)
    ins ++= otherwise
    ins :+= Instruction.JumpIns(Opcodes.GOTO, endLabel)
    ins :+= Instruction.PlaceLabel(conditionLabel)
    ins ++= thenn
    ins :+= Instruction.PlaceLabel(endLabel)
    ins
  }

  def tryCatch(body: InstructionSet)(catchI: InstructionSet): InstructionSet = {
    var ins: Chain[Instruction] = Chain.empty
    val beforeTry = new Label()
    val afterTry = new Label()
    val handlerStart = new Label()
    val afterEverything = new Label()
    ins :+= Instruction.TryCatchIns(beforeTry, afterTry, handlerStart)
    ins :+= Instruction.PlaceLabel(beforeTry)
    ins ++= body
    ins :+= Instruction.PlaceLabel(afterTry)
    ins :+= Instruction.JumpIns(Opcodes.GOTO, afterEverything)
    ins :+= Instruction.PlaceLabel(handlerStart)
    ins ++= catchI
    ins :+= Instruction.PlaceLabel(afterEverything)
    ins
  }

  def invokeConstructor(className: JvmName, descriptor: MethodDescriptor): InstructionSet =
    INVOKESPECIAL(className, JvmName.ConstructorMethod, descriptor)

  def nop(): InstructionSet =
    Chain.empty

  def pushBool(b: Boolean): InstructionSet =
    if (b) ICONST_1() else ICONST_0()

  def pushNull(): InstructionSet =
    ACONST_NULL()

  def pushString(s: String): InstructionSet = Chain(Instruction.LoadConstantIns(s))

  def pushInt(i: Int): InstructionSet = i match {
    case -1 => ICONST_M1()
    case 0 => ICONST_0()
    case 1 => ICONST_1()
    case 2 => ICONST_2()
    case 3 => ICONST_3()
    case 4 => ICONST_4()
    case 5 => ICONST_5()
    case _ if scala.Byte.MinValue <= i && i <= scala.Byte.MaxValue => BIPUSH(i.toByte)
    case _ if scala.Short.MinValue <= i && i <= scala.Short.MaxValue => SIPUSH(i.toByte)
    case _ => Chain(Instruction.LoadConstantIns(i))
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

  def withNames(index: Int, tpes: List[BackendType])(body: (Int, List[Variable]) => InstructionSet): InstructionSet = {
    var runningIndex = index
    val variables = tpes.map(tpe => {
      val variable = new Variable(tpe, runningIndex)
      val stackSize = if (tpe.is64BitWidth) 2 else 1
      runningIndex = runningIndex + stackSize
      variable
    })
    body(runningIndex, variables)
  }

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

  def xSwap(lowerLarge: Boolean, higherLarge: Boolean): InstructionSet = (lowerLarge, higherLarge) match {
    case (true, true) => DUP2_X2() ~ POP2()
    case (true, false) => DUP_X2() ~ POP()
    case (false, true) => DUP2_X1() ~ POP2()
    case (false, false) => SWAP()
  }

  /**
    * Converts the top of the stack to a string (including null), assuming that
    * `tpe` accurately represents its type.
    */
  def xToString(tpe: BackendType): InstructionSet = tpe match {
    case BackendType.Bool => INVOKESTATIC(BackendObjType.String.BoolValueOf)
    case BackendType.Char => INVOKESTATIC(BackendObjType.String.CharValueOf)
    case BackendType.Int8 => INVOKESTATIC(BackendObjType.String.Int8ValueOf)
    case BackendType.Int16 => INVOKESTATIC(BackendObjType.String.Int16ValueOf)
    case BackendType.Int32 => INVOKESTATIC(BackendObjType.String.Int32ValueOf)
    case BackendType.Int64 => INVOKESTATIC(BackendObjType.String.Int64ValueOf)
    case BackendType.Float32 => INVOKESTATIC(BackendObjType.String.Float32ValueOf)
    case BackendType.Float64 => INVOKESTATIC(BackendObjType.String.Float64ValueOf)
    case BackendType.Reference(_) => INVOKESTATIC(BackendObjType.String.ObjectValueOf)

    case BackendType.Array(BackendType.Bool) => INVOKESTATIC(BackendObjType.Arrays.BoolArrToString)
    case BackendType.Array(BackendType.Char) => INVOKESTATIC(BackendObjType.Arrays.CharArrToString)
    case BackendType.Array(BackendType.Int8) => INVOKESTATIC(BackendObjType.Arrays.Int8ArrToString)
    case BackendType.Array(BackendType.Int16) => INVOKESTATIC(BackendObjType.Arrays.Int16ArrToString)
    case BackendType.Array(BackendType.Int32) => INVOKESTATIC(BackendObjType.Arrays.Int32ArrToString)
    case BackendType.Array(BackendType.Int64) => INVOKESTATIC(BackendObjType.Arrays.Int64ArrToString)
    case BackendType.Array(BackendType.Float32) => INVOKESTATIC(BackendObjType.Arrays.Float32ArrToString)
    case BackendType.Array(BackendType.Float64) => INVOKESTATIC(BackendObjType.Arrays.Float64ArrToString)
    case BackendType.Array(BackendType.Reference(_) | BackendType.Array(_)) => INVOKESTATIC(BackendObjType.Arrays.DeepToString)
  }

  def composeN(ins: IterableOnce[InstructionSet]): InstructionSet =
    ins.iterator.foldLeft(nop())(compose)

  /**
    * Sequential composition with `sep` between elements.
    */
  def joinN(ins: List[InstructionSet], sep: InstructionSet): InstructionSet = ins match {
    case Nil => nop()
    case head :: next => head ~ composeN(next.map(e => sep ~ e))
  }


  //
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Private ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //

  @tailrec
  private def opcodeOf(c: Condition): Int = c match {
    case Condition.ACMPEQ => Opcodes.IF_ACMPEQ
    case Condition.ACMPNE => Opcodes.IF_ACMPNE
    case Condition.Bool => opcodeOf(Condition.NE)
    case Condition.EQ => Opcodes.IFEQ
    case Condition.ICMPEQ => Opcodes.IF_ICMPEQ
    case Condition.ICMPNE => Opcodes.IF_ICMPNE
    case Condition.LT => Opcodes.IFLT
    case Condition.LE => Opcodes.IFLE
    case Condition.GT => Opcodes.IFGT
    case Condition.GE => Opcodes.IFGE
    case Condition.NE => Opcodes.IFNE
    case Condition.NONNULL => Opcodes.IFNONNULL
    case Condition.NULL => Opcodes.IFNULL
  }

  @tailrec
  private def negated(c: Condition): Condition = c match {
    case Condition.ACMPEQ => Condition.ACMPNE
    case Condition.ACMPNE => Condition.ACMPEQ
    case Condition.Bool => negated(Condition.NE)
    case Condition.EQ => Condition.NE
    case Condition.ICMPEQ => Condition.ICMPNE
    case Condition.ICMPNE => Condition.ICMPEQ
    case Condition.LT => Condition.GE
    case Condition.LE => Condition.GT
    case Condition.GT => Condition.LE
    case Condition.GE => Condition.LT
    case Condition.NE => Condition.EQ
    case Condition.NONNULL => Condition.NULL
    case Condition.NULL => Condition.NONNULL
  }

  object Util {

    /**
      * Returns a instructions `[] --> [prefix + "s1, s2, .." + suffix]`.
      *
      * @param prefix       `[] -> ["prefixString"]`
      * @param suffix       `[] -> ["suffixString"]`
      * @param length       `getNthString` will be called with the range `[0, length[`
      * @param getNthString `[] -> [si: String]`
      */
    def mkString(prefix: Option[InstructionSet], suffix: Option[InstructionSet], length: Int, getNthString: Int => InstructionSet): InstructionSet = {
      // [] --> [new String[length]] // Referred to as `elms`.
      pushInt(length) ~ ANEWARRAY(BackendObjType.String.jvmName) ~
        // [elms] --> [elms, -1] // Running index referred to as `i`.
        ICONST_M1() ~
        // [elms, -1] --> [elms, length]
        composeN((0 until length).map { i =>
          // [elms, i-1] -> [elms, i]
          ICONST_1() ~ IADD() ~
            // [elms, i] -> [elms, i, elms, i]
            DUP2() ~
            // [elms, i, elms, i] -> [elms, i, elms, i, nth(i)]
            getNthString(i) ~
            // [elms, i, elms, i, nth(i)] -> [elms, i]
            AASTORE()
        }) ~
        // [elms, length] --> [elms]
        POP() ~
        // [elms] -> [", ", elms]
        pushString(", ") ~ SWAP() ~
        // [", ", elms] --> ["s1, s2, .."]
        INVOKESTATIC(BackendObjType.String.JoinMethod) ~
        // ["s1, s2, .."] --> [prefix + "s1, s2, .."]
        prefix.map(ins => ins ~ SWAP() ~ INVOKEVIRTUAL(BackendObjType.String.Concat)).getOrElse(nop()) ~
        // [prefix + "s1, s2, .."] --> [prefix + "s1, s2, .." + suffix]
        suffix.map(ins => ins ~ INVOKEVIRTUAL(BackendObjType.String.Concat)).getOrElse(nop())
    }

  }
}

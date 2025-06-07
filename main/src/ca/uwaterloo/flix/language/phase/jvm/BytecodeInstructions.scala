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

import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.phase.jvm.BytecodeInstructions.Branch.{FalseBranch, TrueBranch}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.*
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor.mkDescriptor
import org.objectweb.asm
import org.objectweb.asm.{Label, MethodVisitor, Opcodes}

import scala.annotation.tailrec

object BytecodeInstructions {

  /** A wrapper of [[MethodVisitor]] to improve its interface. */
  final case class AsmWrapper(mv: MethodVisitor) {
    def visitTypeInstruction(opcode: Int, tpe: JvmName): Unit =
      mv.visitTypeInsn(opcode, tpe.toInternalName)

    def visitTypeInstructionDirect(opcode: Int, tpe: String): Unit =
      mv.visitTypeInsn(opcode, tpe)

    def visitInstruction(opcode: Int): Unit = mv.visitInsn(opcode)

    def visitMethodInstruction(opcode: Int, owner: JvmName, methodName: String, descriptor: MethodDescriptor, isInterface: Boolean): Unit =
      mv.visitMethodInsn(opcode, owner.toInternalName, methodName, descriptor.toDescriptor, isInterface)

    def visitInvokeDynamicInstruction(methodName: String, descriptor: MethodDescriptor, bootstrapMethodHandle: Handle, bootstrapMethodArguments: Any*): Unit =
      mv.visitInvokeDynamicInsn(methodName, descriptor.toDescriptor, bootstrapMethodHandle.handle, bootstrapMethodArguments *)

    def visitFieldInstruction(opcode: Int, owner: JvmName, fieldName: String, fieldType: BackendType): Unit =
      mv.visitFieldInsn(opcode, owner.toInternalName, fieldName, fieldType.toDescriptor)

    def visitVarInstruction(opcode: Int, v: Int): Unit =
      mv.visitVarInsn(opcode, v)

    def visitJumpInstruction(opcode: Int, label: Label): Unit =
      mv.visitJumpInsn(opcode, label)

    def visitLabel(label: Label): Unit =
      mv.visitLabel(label)

    def visitLineNumber(line: Int, label: Label): Unit =
      mv.visitLineNumber(line, label)

    def visitLoadConstantInstruction(v: Any): Unit =
      mv.visitLdcInsn(v)

    def visitIntInstruction(opcode: Int, v: Int): Unit =
      mv.visitIntInsn(opcode, v)

    def visitTryCatchBlock(beforeTry: Label, afterTry: Label, handlerStart: Label): Unit =
      mv.visitTryCatchBlock(beforeTry, afterTry, handlerStart, null)
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
    def load()(implicit f: AsmWrapper): Unit = xLoad(tpe, index)

    def store()(implicit f: AsmWrapper): Unit = xStore(tpe, index)
  }

  //
  // ~~~~~~~~~~~~~~~~~~~~~~~~ Direct JVM Instructions ~~~~~~~~~~~~~~~~~~~~~~~~~
  //

  def ACONST_NULL()(implicit f: AsmWrapper): Unit = f.visitInstruction(Opcodes.ACONST_NULL)

  def ALOAD(index: Int)(implicit f: AsmWrapper): Unit = f.visitVarInstruction(Opcodes.ALOAD, index)

  def ANEWARRAY(className: JvmName)(implicit f: AsmWrapper): Unit = f.visitTypeInstruction(Opcodes.ANEWARRAY, className)

  def ARETURN()(implicit f: AsmWrapper): Unit = f.visitInstruction(Opcodes.ARETURN)

  def ARRAYLENGTH()(implicit f: AsmWrapper): Unit = f.visitInstruction(Opcodes.ARRAYLENGTH)

  def ASTORE(index: Int)(implicit f: AsmWrapper): Unit =
    f.visitVarInstruction(Opcodes.ASTORE, index)

  def ATHROW()(implicit f: AsmWrapper): Unit =
    f.visitInstruction(Opcodes.ATHROW)

  def BIPUSH(i: Byte)(implicit f: AsmWrapper): Unit =
    f.visitIntInstruction(Opcodes.BIPUSH, i)

  def CHECKCAST(className: JvmName)(implicit f: AsmWrapper): Unit =
    f.visitTypeInstruction(Opcodes.CHECKCAST, className)

  def DLOAD(index: Int)(implicit f: AsmWrapper): Unit =
    f.visitVarInstruction(Opcodes.DLOAD, index)

  def DRETURN()(implicit f: AsmWrapper): Unit =
    f.visitInstruction(Opcodes.DRETURN)

  def DUP()(implicit f: AsmWrapper): Unit =
    f.visitInstruction(Opcodes.DUP)

  def DUP2()(implicit f: AsmWrapper): Unit =
    f.visitInstruction(Opcodes.DUP2)

  def DUP_X1()(implicit f: AsmWrapper): Unit =
    f.visitInstruction(Opcodes.DUP_X1)

  def DUP_X2()(implicit f: AsmWrapper): Unit =
    f.visitInstruction(Opcodes.DUP_X2)

  def GETFIELD(field: InstanceField)(implicit f: AsmWrapper): Unit =
    f.visitFieldInstruction(Opcodes.GETFIELD, field.clazz, field.name, field.tpe)

  def GETSTATIC(field: StaticField)(implicit f: AsmWrapper): Unit =
    f.visitFieldInstruction(Opcodes.GETSTATIC, field.clazz, field.name, field.tpe)

  def IADD()(implicit f: AsmWrapper): Unit =
    f.visitInstruction(Opcodes.IADD)

  def ICONST_0()(implicit f: AsmWrapper): Unit =
    f.visitInstruction(Opcodes.ICONST_0)

  def ICONST_1()(implicit f: AsmWrapper): Unit =
    f.visitInstruction(Opcodes.ICONST_1)

  def ICONST_2()(implicit f: AsmWrapper): Unit =
    f.visitInstruction(Opcodes.ICONST_2)

  def ICONST_3()(implicit f: AsmWrapper): Unit =
    f.visitInstruction(Opcodes.ICONST_3)

  def ICONST_4()(implicit f: AsmWrapper): Unit =
    f.visitInstruction(Opcodes.ICONST_4)

  def ICONST_5()(implicit f: AsmWrapper): Unit =
    f.visitInstruction(Opcodes.ICONST_5)

  def ICONST_M1()(implicit f: AsmWrapper): Unit =
    f.visitInstruction(Opcodes.ICONST_M1)

  def ILOAD(index: Int)(implicit f: AsmWrapper): Unit =
    f.visitVarInstruction(Opcodes.ILOAD, index)

  def INSTANCEOF(tpe: JvmName)(implicit f: AsmWrapper): Unit =
    f.visitTypeInstruction(Opcodes.INSTANCEOF, tpe)

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
  def mkStaticLambda(lambdaMethod: InterfaceMethod, callD: MethodDescriptor, callHandle: Handle, drop: Int)(implicit f: AsmWrapper): Unit =
    f.visitInvokeDynamicInstruction(
      lambdaMethod.name,
      mkDescriptor(callD.arguments.dropRight(drop) *)(lambdaMethod.clazz.toTpe),
      mkStaticHandle(BackendObjType.LambdaMetaFactory.MetaFactoryMethod),
      lambdaMethod.d.toAsmType,
      callHandle.handle,
      lambdaMethod.d.toAsmType
    )

  def mkStaticLambda(lambdaMethod: InterfaceMethod, call: StaticMethod, drop: Int)(implicit f: AsmWrapper): Unit =
    mkStaticLambda(lambdaMethod, call.d, mkStaticHandle(call), drop)

  def mkStaticLambda(lambdaMethod: InterfaceMethod, call: StaticInterfaceMethod, drop: Int)(implicit f: AsmWrapper): Unit =
    mkStaticLambda(lambdaMethod, call.d, mkStaticHandle(call), drop)

  def INVOKEINTERFACE(interfaceName: JvmName, methodName: String, descriptor: MethodDescriptor)(implicit f: AsmWrapper): Unit =
    f.visitMethodInstruction(Opcodes.INVOKEINTERFACE, interfaceName, methodName, descriptor, isInterface = true)

  def INVOKEINTERFACE(m: InterfaceMethod)(implicit f: AsmWrapper): Unit =
    f.visitMethodInstruction(Opcodes.INVOKEINTERFACE, m.clazz, m.name, m.d, isInterface = true)

  def INVOKESPECIAL(className: JvmName, methodName: String, descriptor: MethodDescriptor)(implicit f: AsmWrapper): Unit = {
    val isInterface = false // OBS this is not technically true if you use it to call private interface methods(?)
    f.visitMethodInstruction(Opcodes.INVOKESPECIAL, className, methodName, descriptor, isInterface = isInterface)
  }

  def INVOKESPECIAL(c: ConstructorMethod)(implicit f: AsmWrapper): Unit =
    f.visitMethodInstruction(Opcodes.INVOKESPECIAL, c.clazz, c.name, c.d, isInterface = false)

  def INVOKESTATIC(className: JvmName, methodName: String, descriptor: MethodDescriptor, isInterface: Boolean = false)(implicit f: AsmWrapper): Unit =
    f.visitMethodInstruction(Opcodes.INVOKESTATIC, className, methodName, descriptor, isInterface)

  def INVOKESTATIC(m: StaticMethod)(implicit f: AsmWrapper): Unit =
    f.visitMethodInstruction(Opcodes.INVOKESTATIC, m.clazz, m.name, m.d, isInterface = false)

  def INVOKESTATIC(m: StaticInterfaceMethod)(implicit f: AsmWrapper): Unit =
    f.visitMethodInstruction(Opcodes.INVOKESTATIC, m.clazz, m.name, m.d, isInterface = true)

  def INVOKEVIRTUAL(className: JvmName, methodName: String, descriptor: MethodDescriptor, isInterface: Boolean = false)(implicit f: AsmWrapper): Unit =
    f.visitMethodInstruction(Opcodes.INVOKEVIRTUAL, className, methodName, descriptor, isInterface)

  def INVOKEVIRTUAL(m: AbstractMethod)(implicit f: AsmWrapper): Unit =
    f.visitMethodInstruction(Opcodes.INVOKEVIRTUAL, m.clazz, m.name, m.d, isInterface = false)

  def INVOKEVIRTUAL(m: InstanceMethod)(implicit f: AsmWrapper): Unit =
    f.visitMethodInstruction(Opcodes.INVOKEVIRTUAL, m.clazz, m.name, m.d, isInterface = false)

  def IRETURN()(implicit f: AsmWrapper): Unit =
    f.visitInstruction(Opcodes.IRETURN)

  def LCMP()(implicit f: AsmWrapper): Unit =
    f.visitInstruction(Opcodes.LCMP)

  def LCONST_0()(implicit f: AsmWrapper): Unit =
    f.visitInstruction(Opcodes.LCONST_0)

  def LCONST_1()(implicit f: AsmWrapper): Unit =
    f.visitInstruction(Opcodes.LCONST_1)

  def LLOAD(index: Int)(implicit f: AsmWrapper): Unit =
    f.visitVarInstruction(Opcodes.LLOAD, index)

  def LRETURN()(implicit f: AsmWrapper): Unit =
    f.visitInstruction(Opcodes.LRETURN)

  def NEW(className: JvmName)(implicit f: AsmWrapper): Unit =
    f.visitTypeInstruction(Opcodes.NEW, className)

  def POP()(implicit f: AsmWrapper): Unit =
    f.visitInstruction(Opcodes.POP)

  def POP2()(implicit f: AsmWrapper): Unit =
    f.visitInstruction(Opcodes.POP2)

  def PUTFIELD(field: InstanceField)(implicit f: AsmWrapper): Unit =
    f.visitFieldInstruction(Opcodes.PUTFIELD, field.clazz, field.name, field.tpe)

  def PUTSTATIC(field: StaticField)(implicit f: AsmWrapper): Unit =
    f.visitFieldInstruction(Opcodes.PUTSTATIC, field.clazz, field.name, field.tpe)

  def RETURN()(implicit f: AsmWrapper): Unit =
    f.visitInstruction(Opcodes.RETURN)

  def SIPUSH(i: Short)(implicit f: AsmWrapper): Unit =
    f.visitIntInstruction(Opcodes.SIPUSH, i)

  def SWAP()(implicit f: AsmWrapper): Unit =
    f.visitInstruction(Opcodes.SWAP)

  //
  // ~~~~~~~~~~~~~~~~~~~~~~~~~ Meta JVM Instructions ~~~~~~~~~~~~~~~~~~~~~~~~~~
  //

  def addLoc(loc: SourceLocation)(implicit f: AsmWrapper): Unit = {
    val label = new Label()
    f.visitLabel(label)
    f.visitLineNumber(loc.beginLine, label)
  }

  def branch(c: Condition)(cases: Branch => Unit)(implicit f: AsmWrapper): Unit = {
    val jumpLabel = new Label()
    val skipLabel = new Label()
    f.visitJumpInstruction(opcodeOf(c), jumpLabel)

    cases(FalseBranch)
    f.visitJumpInstruction(Opcodes.GOTO, skipLabel)

    f.visitLabel(jumpLabel)
    cases(TrueBranch)
    f.visitLabel(skipLabel)
  }

  def castIfNotPrim(tpe: BackendType)(implicit f: AsmWrapper): Unit = {
    tpe match {
      case arr: BackendType.Array => f.visitTypeInstructionDirect(Opcodes.CHECKCAST, arr.toDescriptor)
      case BackendType.Reference(ref) => CHECKCAST(ref.jvmName)
      case _: BackendType.PrimitiveType => nop()
    }
  }

  /// while(c(t)) { i }
  def whileLoop(c: Condition)(t: => Unit)(i: => Unit)(implicit f: AsmWrapper): Unit = {
    val startLabel = new Label()
    val doneLabel = new Label()
    f.visitLabel(startLabel)
    t
    f.visitJumpInstruction(opcodeOf(negated(c)), doneLabel)
    i
    f.visitJumpInstruction(Opcodes.GOTO, startLabel)
    f.visitLabel(doneLabel)
  }

  def ifCondition(c: Condition)(i: => Unit)(implicit f: AsmWrapper): Unit = {
    val jumpLabel = new Label()
    f.visitJumpInstruction(opcodeOf(negated(c)), jumpLabel)
    i
    f.visitLabel(jumpLabel)
  }

  /**
    * Using [[ifCondition]] uses less jumps, so use that if the conditional code
    * is returns or throws
    */
  def ifConditionElse(c: Condition)(i: => Unit)(otherwise: => Unit)(implicit f: AsmWrapper): Unit = {
    val conditionLabel = new Label()
    val endLabel = new Label()
    f.visitJumpInstruction(opcodeOf(c), conditionLabel)
    otherwise
    f.visitJumpInstruction(Opcodes.GOTO, endLabel)
    f.visitLabel(conditionLabel)
    i
    f.visitLabel(endLabel)
  }

  def tryCatch(body: => Unit)(catchI: => Unit)(implicit f: AsmWrapper): Unit = {
    val beforeTry = new Label()
    val afterTry = new Label()
    val handlerStart = new Label()
    val afterEverything = new Label()
    f.visitTryCatchBlock(beforeTry, afterTry, handlerStart)
    f.visitLabel(beforeTry)
    body
    f.visitLabel(afterTry)
    f.visitJumpInstruction(Opcodes.GOTO, afterEverything)
    f.visitLabel(handlerStart)
    catchI
    f.visitLabel(afterEverything)
  }

  def invokeConstructor(className: JvmName, descriptor: MethodDescriptor)(implicit f: AsmWrapper): Unit =
    INVOKESPECIAL(className, JvmName.ConstructorMethod, descriptor)

  def nop(): Unit =
    ()

  def pushBool(b: Boolean)(implicit f: AsmWrapper): Unit =
    if (b) ICONST_1() else ICONST_0()

  def pushNull()(implicit f: AsmWrapper): Unit =
    ACONST_NULL()

  def pushString(s: String)(implicit f: AsmWrapper): Unit =
    f.visitLoadConstantInstruction(s)

  def pushInt(i: Int)(implicit f: AsmWrapper): Unit = i match {
    case -1 => ICONST_M1()
    case 0 => ICONST_0()
    case 1 => ICONST_1()
    case 2 => ICONST_2()
    case 3 => ICONST_3()
    case 4 => ICONST_4()
    case 5 => ICONST_5()
    case _ if scala.Byte.MinValue <= i && i <= scala.Byte.MaxValue => BIPUSH(i.toByte)
    case _ if scala.Short.MinValue <= i && i <= scala.Short.MaxValue => SIPUSH(i.toShort)
    case _ => f.visitLoadConstantInstruction(i)
  }

  def pushLoc(loc: SourceLocation)(implicit f: AsmWrapper): Unit = {
    NEW(BackendObjType.ReifiedSourceLocation.jvmName)
    DUP()
    pushString(loc.source.name)
    pushInt(loc.beginLine)
    pushInt(loc.beginCol)
    pushInt(loc.endLine)
    pushInt(loc.endCol)
    INVOKESPECIAL(BackendObjType.ReifiedSourceLocation.Constructor)
  }

  def storeWithName(index: Int, tpe: BackendType)(body: Variable => Unit)(implicit f: AsmWrapper): Unit = {
    xStore(tpe, index)
    body(new Variable(tpe, index))
  }

  def thisLoad()(implicit f: AsmWrapper): Unit = ALOAD(0)

  def throwUnsupportedOperationException(msg: String)(implicit f: AsmWrapper): Unit = {
    NEW(JvmName.UnsupportedOperationException)
    DUP()
    pushString(msg)
    INVOKESPECIAL(JvmName.UnsupportedOperationException, JvmName.ConstructorMethod,
      mkDescriptor(BackendObjType.String.toTpe)(VoidableType.Void))
    ATHROW()
  }

  def withName(index: Int, tpe: BackendType)(body: Variable => Unit): Unit =
    body(new Variable(tpe, index))

  def withNames(index: Int, tpes: List[BackendType])(body: (Int, List[Variable]) => Unit): Unit = {
    var runningIndex = index
    val variables = tpes.map(tpe => {
      val variable = new Variable(tpe, runningIndex)
      runningIndex = runningIndex + tpe.stackSlots
      variable
    })
    body(runningIndex, variables)
  }

  def xArrayLoad(elmTpe: BackendType)(implicit f: AsmWrapper): Unit = elmTpe match {
    case BackendType.Array(_) => f.visitInstruction(Opcodes.AALOAD)
    case BackendType.Reference(_) => f.visitInstruction(Opcodes.AALOAD)
    case BackendType.Bool => f.visitInstruction(Opcodes.BALOAD)
    case BackendType.Char => f.visitInstruction(Opcodes.CALOAD)
    case BackendType.Int8 => f.visitInstruction(Opcodes.BALOAD)
    case BackendType.Int16 => f.visitInstruction(Opcodes.SALOAD)
    case BackendType.Int32 => f.visitInstruction(Opcodes.IALOAD)
    case BackendType.Int64 => f.visitInstruction(Opcodes.LALOAD)
    case BackendType.Float32 => f.visitInstruction(Opcodes.FALOAD)
    case BackendType.Float64 => f.visitInstruction(Opcodes.DALOAD)
  }

  def xArrayStore(elmTpe: BackendType)(implicit f: AsmWrapper): Unit = elmTpe match {
    case BackendType.Array(_) => f.visitInstruction(Opcodes.AASTORE)
    case BackendType.Reference(_) => f.visitInstruction(Opcodes.AASTORE)
    case BackendType.Bool => f.visitInstruction(Opcodes.BASTORE)
    case BackendType.Char => f.visitInstruction(Opcodes.CASTORE)
    case BackendType.Int8 => f.visitInstruction(Opcodes.BASTORE)
    case BackendType.Int16 => f.visitInstruction(Opcodes.SASTORE)
    case BackendType.Int32 => f.visitInstruction(Opcodes.IASTORE)
    case BackendType.Int64 => f.visitInstruction(Opcodes.LASTORE)
    case BackendType.Float32 => f.visitInstruction(Opcodes.FASTORE)
    case BackendType.Float64 => f.visitInstruction(Opcodes.DASTORE)
  }

  def xLoad(tpe: BackendType, index: Int)(implicit f: AsmWrapper): Unit = tpe match {
    case BackendType.Bool | BackendType.Char | BackendType.Int8 | BackendType.Int16 | BackendType.Int32 => ILOAD(index)
    case BackendType.Int64 => LLOAD(index)
    case BackendType.Float32 => f.visitVarInstruction(Opcodes.FLOAD, index)
    case BackendType.Float64 => DLOAD(index)
    case BackendType.Array(_) | BackendType.Reference(_) => ALOAD(index)
  }

  def xNewArray(elmTpe: BackendType)(implicit f: AsmWrapper): Unit = elmTpe match {
    case BackendType.Array(_) => f.mv.visitTypeInsn(Opcodes.ANEWARRAY, elmTpe.toDescriptor)
    case BackendType.Reference(ref) => ANEWARRAY(ref.jvmName)
    case tpe: BackendType.PrimitiveType => tpe match {
      case BackendType.Bool => f.visitIntInstruction(Opcodes.NEWARRAY, Opcodes.T_BOOLEAN)
      case BackendType.Char => f.visitIntInstruction(Opcodes.NEWARRAY, Opcodes.T_CHAR)
      case BackendType.Int8 => f.visitIntInstruction(Opcodes.NEWARRAY, Opcodes.T_BYTE)
      case BackendType.Int16 => f.visitIntInstruction(Opcodes.NEWARRAY, Opcodes.T_SHORT)
      case BackendType.Int32 => f.visitIntInstruction(Opcodes.NEWARRAY, Opcodes.T_INT)
      case BackendType.Int64 => f.visitIntInstruction(Opcodes.NEWARRAY, Opcodes.T_LONG)
      case BackendType.Float32 => f.visitIntInstruction(Opcodes.NEWARRAY, Opcodes.T_FLOAT)
      case BackendType.Float64 => f.visitIntInstruction(Opcodes.NEWARRAY, Opcodes.T_DOUBLE)
    }
  }

  /**
    * Pops the top of the stack using `POP` or `POP2` depending on the value size.
    */
  def xPop(tpe: BackendType)(implicit f: AsmWrapper): Unit = tpe match {
    case BackendType.Bool | BackendType.Char | BackendType.Int8 | BackendType.Int16 | BackendType.Int32 |
         BackendType.Float32 | BackendType.Array(_) | BackendType.Reference(_) => POP()
    case BackendType.Int64 | BackendType.Float64 => POP2()
  }

  def xReturn(tpe: BackendType)(implicit f: AsmWrapper): Unit = tpe match {
    case BackendType.Bool | BackendType.Char | BackendType.Int8 | BackendType.Int16 | BackendType.Int32 => IRETURN()
    case BackendType.Int64 => LRETURN()
    case BackendType.Float32 => f.visitInstruction(Opcodes.FRETURN)
    case BackendType.Float64 => DRETURN()
    case BackendType.Array(_) | BackendType.Reference(_) => ARETURN()
  }

  def xStore(tpe: BackendType, index: Int)(implicit f: AsmWrapper): Unit = tpe match {
    case BackendType.Bool | BackendType.Char | BackendType.Int8 | BackendType.Int16 | BackendType.Int32 =>
      f.visitVarInstruction(Opcodes.ISTORE, index)
    case BackendType.Int64 => f.visitVarInstruction(Opcodes.LSTORE, index)
    case BackendType.Float32 => f.visitVarInstruction(Opcodes.FSTORE, index)
    case BackendType.Float64 => f.visitVarInstruction(Opcodes.DSTORE, index)
    case BackendType.Array(_) | BackendType.Reference(_) => ASTORE(index)
  }

  def xSwap(lowerLarge: Boolean, higherLarge: Boolean)(implicit f: AsmWrapper): Unit = (lowerLarge, higherLarge) match {
    case (true, true) =>
      f.visitInstruction(Opcodes.DUP2_X2)
      POP2()
    case (true, false) =>
      DUP_X2()
      POP()
    case (false, true) =>
      f.visitInstruction(Opcodes.DUP2_X1)
      POP2()
    case (false, false) =>
      SWAP()
  }

  /**
    * Converts the top of the stack to a string (including null), assuming that
    * `tpe` accurately represents its type.
    */
  def xToString(tpe: BackendType)(implicit f: AsmWrapper): Unit = tpe match {
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
    def mkString(prefix: Option[Unit => Unit], suffix: Option[Unit => Unit], length: Int, getNthString: Int => Unit)(implicit f: AsmWrapper): Unit = {
      // [] --> [new String[length]] // Referred to as `elms`.
      pushInt(length)
      ANEWARRAY(BackendObjType.String.jvmName)
      // [elms] --> [elms, -1] // Running index referred to as `i`.
      ICONST_M1()
      // [elms, -1] --> [elms, length]
      for (i <- 0 until length) {
        // [elms, i-1] -> [elms, i]
        ICONST_1()
        IADD()
        // [elms, i] -> [elms, i, elms, i]
        DUP2()
        // [elms, i, elms, i] -> [elms, i, elms, i, nth(i)]
        getNthString(i)
        // [elms, i, elms, i, nth(i)] -> [elms, i]
        f.visitInstruction(Opcodes.AASTORE)
      }
      // [elms, length] --> [elms]
      POP()
      // [elms] -> [", ", elms]
      pushString(", ")
      SWAP()
      // [", ", elms] --> ["s1, s2, .."]
      INVOKESTATIC(BackendObjType.String.JoinMethod)
      // ["s1, s2, .."] --> [prefix + "s1, s2, .."]
      prefix match {
        case Some(ins) =>
          ins(())
          SWAP()
          INVOKEVIRTUAL(BackendObjType.String.Concat)
        case None =>
          nop()
      }
      // [prefix + "s1, s2, .."] --> [prefix + "s1, s2, .." + suffix]
      suffix match {
        case Some(ins) =>
          ins(())
          INVOKEVIRTUAL(BackendObjType.String.Concat)
        case None =>
          nop()
      }
    }

  }
}

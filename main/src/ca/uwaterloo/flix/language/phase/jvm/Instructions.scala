/*
 * Copyright 2021 Jonathan Lindegaard Starup
 * Copyright 2026 Magnus Madsen
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
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.*
import ca.uwaterloo.flix.language.phase.jvm.Instructions.Branch.{FalseBranch, TrueBranch}
import ca.uwaterloo.flix.language.phase.jvm.classes.GenReifiedSourceLocation
import ca.uwaterloo.flix.util.{ClassDescs, InternalCompilerException}
import org.objectweb.asm
import org.objectweb.asm.{Label, MethodVisitor, Opcodes}

import java.lang.constant.{ClassDesc, ConstantDescs, MethodTypeDesc}
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.*

/** Instructions for emitting JVM bytecode via [[MethodVisitor]], expressed in terms of nominal JVM types ([[ClassDesc]], [[MethodTypeDesc]]). */
object Instructions {

  //
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ASM Façade ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //

  /** A wrapper of [[MethodVisitor]] that accepts nominal JVM types. */
  implicit class RichMethodVisitor(visitor: MethodVisitor) {

    /** Emits a type instruction for `tpe`. */
    def visitTypeInstruction(opcode: Int, tpe: ClassDesc): Unit =
      visitor.visitTypeInsn(opcode, ClassDescs.internalNameOf(tpe))

    /** Emits a type instruction using the internal JVM type name `tpe`. */
    def visitTypeInstructionDirect(opcode: Int, tpe: String): Unit =
      visitor.visitTypeInsn(opcode, tpe)

    /** Emits a zero-operand JVM instruction. */
    def visitInstruction(opcode: Int): Unit = visitor.visitInsn(opcode)

    /** Emits a method invocation instruction. */
    def visitMethodInstruction(opcode: Int, owner: ClassDesc, methodName: String, descriptor: MethodTypeDesc, isInterface: Boolean): Unit =
      visitor.visitMethodInsn(opcode, ClassDescs.internalNameOf(owner), methodName, descriptor.descriptorString(), isInterface)

    // TODO: sanitize varags
    /** Emits an `invokedynamic` instruction. */
    def visitInvokeDynamicInstruction(methodName: String, descriptor: MethodTypeDesc, bootstrapMethodHandle: Handle, bootstrapMethodArguments: Any*): Unit =
      visitor.visitInvokeDynamicInsn(methodName, descriptor.descriptorString(), bootstrapMethodHandle.handle, bootstrapMethodArguments *)

    /** Emits a field access instruction. */
    def visitFieldInstruction(opcode: Int, owner: ClassDesc, fieldName: String, fieldType: ClassDesc): Unit =
      visitor.visitFieldInsn(opcode, ClassDescs.internalNameOf(owner), fieldName, fieldType.descriptorString())

    /** Emits a local-variable instruction. */
    def visitVarInstruction(opcode: Int, v: Int): Unit =
      visitor.visitVarInsn(opcode, v)

    /** Emits a jump instruction targeting `label`. */
    def visitJumpInstruction(opcode: Int, label: Label): Unit =
      visitor.visitJumpInsn(opcode, label)

    /** Emits `label` at the current bytecode position. */
    def visitLabel(label: Label): Unit =
      visitor.visitLabel(label)

    /** Associates `line` with `label`. */
    def visitLineNumber(line: Int, label: Label): Unit =
      visitor.visitLineNumber(line, label)

    /** Emits an instruction that loads the constant `v`. */
    def visitLoadConstantInstruction(v: Any): Unit =
      visitor.visitLdcInsn(v)

    /** Emits an integer-operand instruction. */
    def visitIntInstruction(opcode: Int, v: Int): Unit =
      visitor.visitIntInsn(opcode, v)

    /** Registers a catch-all handler for the given bytecode interval. */
    def visitTryCatchBlock(beforeTry: Label, afterTry: Label, handlerStart: Label): Unit =
      visitor.visitTryCatchBlock(beforeTry, afterTry, handlerStart, null)
  }

  //
  // ~~~~~~~~~~~~~~~~~~~~~~~~ Direct JVM Instructions ~~~~~~~~~~~~~~~~~~~~~~~~~
  //

  /** Emits the JVM `ACONST_NULL` instruction. */
  def ACONST_NULL()(implicit mv: MethodVisitor): Unit = mv.visitInstruction(Opcodes.ACONST_NULL)

  /** Emits the JVM `ALOAD` instruction for local `index`. */
  def ALOAD(index: Int)(implicit mv: MethodVisitor): Unit = mv.visitVarInstruction(Opcodes.ALOAD, index)

  /** Emits the JVM `ANEWARRAY` instruction for `className`. */
  def ANEWARRAY(className: ClassDesc)(implicit mv: MethodVisitor): Unit = mv.visitTypeInstruction(Opcodes.ANEWARRAY, className)

  /** Emits the JVM `ARETURN` instruction. */
  def ARETURN()(implicit mv: MethodVisitor): Unit = mv.visitInstruction(Opcodes.ARETURN)

  /** Emits the JVM `ARRAYLENGTH` instruction. */
  def ARRAYLENGTH()(implicit mv: MethodVisitor): Unit = mv.visitInstruction(Opcodes.ARRAYLENGTH)

  /** Emits the JVM `ASTORE` instruction for local `index`. */
  def ASTORE(index: Int)(implicit mv: MethodVisitor): Unit =
    mv.visitVarInstruction(Opcodes.ASTORE, index)

  /** Emits the JVM `ATHROW` instruction. */
  def ATHROW()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.ATHROW)

  /** Emits the JVM `BIPUSH` instruction for `i`. */
  def BIPUSH(i: Byte)(implicit mv: MethodVisitor): Unit =
    mv.visitIntInstruction(Opcodes.BIPUSH, i)

  /** Emits the JVM `CHECKCAST` instruction for `className`. */
  def CHECKCAST(className: ClassDesc)(implicit mv: MethodVisitor): Unit =
    mv.visitTypeInstruction(Opcodes.CHECKCAST, className)

  /** Emits the JVM `DLOAD` instruction for local `index`. */
  def DLOAD(index: Int)(implicit mv: MethodVisitor): Unit =
    mv.visitVarInstruction(Opcodes.DLOAD, index)

  /** Emits the JVM `DRETURN` instruction. */
  def DRETURN()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.DRETURN)

  /** Emits the JVM `DUP` instruction. */
  def DUP()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.DUP)

  /** Emits the JVM `DUP2` instruction. */
  def DUP2()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.DUP2)

  /** Emits the JVM `DUP_X1` instruction. */
  def DUP_X1()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.DUP_X1)

  /** Emits the JVM `DUP_X2` instruction. */
  def DUP_X2()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.DUP_X2)

  /** Emits the JVM `GETFIELD` instruction for `field`. */
  def GETFIELD(field: InstanceField)(implicit mv: MethodVisitor): Unit =
    mv.visitFieldInstruction(Opcodes.GETFIELD, field.clazz, field.name, field.tpe)

  /** Emits the JVM `GETSTATIC` instruction for `field`. */
  def GETSTATIC(field: StaticField)(implicit mv: MethodVisitor): Unit =
    mv.visitFieldInstruction(Opcodes.GETSTATIC, field.clazz, field.name, field.tpe)

  /** Emits the JVM `IADD` instruction. */
  def IADD()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.IADD)

  /** Emits the JVM `ICONST_0` instruction. */
  def ICONST_0()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.ICONST_0)

  /** Emits the JVM `ICONST_1` instruction. */
  def ICONST_1()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.ICONST_1)

  /** Emits the JVM `ICONST_2` instruction. */
  def ICONST_2()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.ICONST_2)

  /** Emits the JVM `ICONST_3` instruction. */
  def ICONST_3()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.ICONST_3)

  /** Emits the JVM `ICONST_4` instruction. */
  def ICONST_4()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.ICONST_4)

  /** Emits the JVM `ICONST_5` instruction. */
  def ICONST_5()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.ICONST_5)

  /** Emits the JVM `ICONST_M1` instruction. */
  def ICONST_M1()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.ICONST_M1)

  /** Emits the JVM `ILOAD` instruction for local `index`. */
  def ILOAD(index: Int)(implicit mv: MethodVisitor): Unit =
    mv.visitVarInstruction(Opcodes.ILOAD, index)

  /** Emits the JVM `INSTANCEOF` instruction for `tpe`. */
  def INSTANCEOF(tpe: ClassDesc)(implicit mv: MethodVisitor): Unit =
    mv.visitTypeInstruction(Opcodes.INSTANCEOF, tpe)

  /** Emits the JVM `INVOKEINTERFACE` instruction for the given method. */
  def INVOKEINTERFACE(interfaceName: ClassDesc, methodName: String, descriptor: MethodTypeDesc)(implicit mv: MethodVisitor): Unit =
    mv.visitMethodInstruction(Opcodes.INVOKEINTERFACE, interfaceName, methodName, descriptor, isInterface = true)

  /** Emits the JVM `INVOKEINTERFACE` instruction for `m`. */
  def INVOKEINTERFACE(m: InterfaceMethod)(implicit mv: MethodVisitor): Unit =
    mv.visitMethodInstruction(Opcodes.INVOKEINTERFACE, m.clazz, m.name, m.d, isInterface = true)

  /** Emits the JVM `INVOKESPECIAL` instruction for the given method. */
  def INVOKESPECIAL(className: ClassDesc, methodName: String, descriptor: MethodTypeDesc)(implicit mv: MethodVisitor): Unit = {
    val isInterface = false // OBS this is not technically true if you use it to call private interface methods(?)
    mv.visitMethodInstruction(Opcodes.INVOKESPECIAL, className, methodName, descriptor, isInterface = isInterface)
  }

  /** Emits the JVM `INVOKESPECIAL` instruction for constructor `c`. */
  def INVOKESPECIAL(c: ConstructorMethod)(implicit mv: MethodVisitor): Unit =
    mv.visitMethodInstruction(Opcodes.INVOKESPECIAL, c.clazz, c.name, c.d, isInterface = false)

  /** Emits the JVM `INVOKESTATIC` instruction for the given method. */
  def INVOKESTATIC(className: ClassDesc, methodName: String, descriptor: MethodTypeDesc, isInterface: Boolean = false)(implicit mv: MethodVisitor): Unit =
    mv.visitMethodInstruction(Opcodes.INVOKESTATIC, className, methodName, descriptor, isInterface)

  /** Emits the JVM `INVOKESTATIC` instruction for `m`. */
  def INVOKESTATIC(m: StaticMethod)(implicit mv: MethodVisitor): Unit =
    mv.visitMethodInstruction(Opcodes.INVOKESTATIC, m.clazz, m.name, m.d, isInterface = false)

  /** Emits the JVM `INVOKESTATIC` instruction for interface method `m`. */
  def INVOKESTATIC(m: StaticInterfaceMethod)(implicit mv: MethodVisitor): Unit =
    mv.visitMethodInstruction(Opcodes.INVOKESTATIC, m.clazz, m.name, m.d, isInterface = true)

  /** Emits the JVM `INVOKEVIRTUAL` instruction for the given method. */
  def INVOKEVIRTUAL(className: ClassDesc, methodName: String, descriptor: MethodTypeDesc, isInterface: Boolean = false)(implicit mv: MethodVisitor): Unit =
    mv.visitMethodInstruction(Opcodes.INVOKEVIRTUAL, className, methodName, descriptor, isInterface)

  /** Emits the JVM `INVOKEVIRTUAL` instruction for abstract method `m`. */
  def INVOKEVIRTUAL(m: AbstractMethod)(implicit mv: MethodVisitor): Unit =
    mv.visitMethodInstruction(Opcodes.INVOKEVIRTUAL, m.clazz, m.name, m.d, isInterface = false)

  /** Emits the JVM `INVOKEVIRTUAL` instruction for instance method `m`. */
  def INVOKEVIRTUAL(m: InstanceMethod)(implicit mv: MethodVisitor): Unit =
    mv.visitMethodInstruction(Opcodes.INVOKEVIRTUAL, m.clazz, m.name, m.d, isInterface = false)

  /** Emits the JVM `IRETURN` instruction. */
  def IRETURN()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.IRETURN)

  /** Emits the JVM `LCMP` instruction. */
  def LCMP()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.LCMP)

  /** Emits the JVM `LCONST_0` instruction. */
  def LCONST_0()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.LCONST_0)

  /** Emits the JVM `LCONST_1` instruction. */
  def LCONST_1()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.LCONST_1)

  /** Emits the JVM `LLOAD` instruction for local `index`. */
  def LLOAD(index: Int)(implicit mv: MethodVisitor): Unit =
    mv.visitVarInstruction(Opcodes.LLOAD, index)

  /** Emits the JVM `LRETURN` instruction. */
  def LRETURN()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.LRETURN)

  /** Emits the JVM `NEW` instruction for `className`. */
  def NEW(className: ClassDesc)(implicit mv: MethodVisitor): Unit =
    mv.visitTypeInstruction(Opcodes.NEW, className)

  /** Emits the JVM `POP` instruction. */
  def POP()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.POP)

  /** Emits the JVM `POP2` instruction. */
  def POP2()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.POP2)

  /** Emits the JVM `PUTFIELD` instruction for `field`. */
  def PUTFIELD(field: InstanceField)(implicit mv: MethodVisitor): Unit =
    mv.visitFieldInstruction(Opcodes.PUTFIELD, field.clazz, field.name, field.tpe)

  /** Emits the JVM `PUTSTATIC` instruction for `field`. */
  def PUTSTATIC(field: StaticField)(implicit mv: MethodVisitor): Unit =
    mv.visitFieldInstruction(Opcodes.PUTSTATIC, field.clazz, field.name, field.tpe)

  /** Emits the JVM `RETURN` instruction. */
  def RETURN()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.RETURN)

  /** Emits the JVM `SIPUSH` instruction for `i`. */
  def SIPUSH(i: Short)(implicit mv: MethodVisitor): Unit =
    mv.visitIntInstruction(Opcodes.SIPUSH, i)

  /** Emits the JVM `SWAP` instruction. */
  def SWAP()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.SWAP)

  //
  // ~~~~~~~~~~~~~~~~~~~~~~~ Type-Directed Instructions ~~~~~~~~~~~~~~~~~~~~~~
  //

  /** Emits a `CHECKCAST` to `tpe` unless `tpe` is primitive. */
  def castIfNotPrim(tpe: ClassDesc)(implicit mv: MethodVisitor): Unit = {
    if (!tpe.isPrimitive) mv.visitTypeInsn(Opcodes.CHECKCAST, ClassDescs.internalNameOf(tpe))
  }

  /** Returns whether `tpe` occupies two JVM stack slots. */
  def isCategory2(tpe: ClassDesc): Boolean =
    tpe == ConstantDescs.CD_long || tpe == ConstantDescs.CD_double

  /** Emits the array-load instruction appropriate for `elmTpe`. */
  def xArrayLoad(elmTpe: ClassDesc)(implicit mv: MethodVisitor): Unit = {
    import java.lang.constant.ConstantDescs.*
    if (elmTpe == CD_boolean) mv.visitInsn(Opcodes.BALOAD)
    else if (elmTpe == CD_char) mv.visitInsn(Opcodes.CALOAD)
    else if (elmTpe == CD_byte) mv.visitInsn(Opcodes.BALOAD)
    else if (elmTpe == CD_short) mv.visitInsn(Opcodes.SALOAD)
    else if (elmTpe == CD_int) mv.visitInsn(Opcodes.IALOAD)
    else if (elmTpe == CD_long) mv.visitInsn(Opcodes.LALOAD)
    else if (elmTpe == CD_float) mv.visitInsn(Opcodes.FALOAD)
    else if (elmTpe == CD_double) mv.visitInsn(Opcodes.DALOAD)
    else mv.visitInsn(Opcodes.AALOAD)
  }

  /** Emits the array-store instruction appropriate for `elmTpe`. */
  def xArrayStore(elmTpe: ClassDesc)(implicit mv: MethodVisitor): Unit = {
    import java.lang.constant.ConstantDescs.*
    if (elmTpe == CD_boolean) mv.visitInsn(Opcodes.BASTORE)
    else if (elmTpe == CD_char) mv.visitInsn(Opcodes.CASTORE)
    else if (elmTpe == CD_byte) mv.visitInsn(Opcodes.BASTORE)
    else if (elmTpe == CD_short) mv.visitInsn(Opcodes.SASTORE)
    else if (elmTpe == CD_int) mv.visitInsn(Opcodes.IASTORE)
    else if (elmTpe == CD_long) mv.visitInsn(Opcodes.LASTORE)
    else if (elmTpe == CD_float) mv.visitInsn(Opcodes.FASTORE)
    else if (elmTpe == CD_double) mv.visitInsn(Opcodes.DASTORE)
    else mv.visitInsn(Opcodes.AASTORE)
  }

  /** Emits the local-load instruction appropriate for `tpe`. */
  def xLoad(tpe: ClassDesc, index: Int)(implicit mv: MethodVisitor): Unit = {
    import java.lang.constant.ConstantDescs.*
    if (tpe == CD_boolean || tpe == CD_char || tpe == CD_byte || tpe == CD_short || tpe == CD_int) mv.visitVarInsn(Opcodes.ILOAD, index)
    else if (tpe == CD_long) mv.visitVarInsn(Opcodes.LLOAD, index)
    else if (tpe == CD_float) mv.visitVarInsn(Opcodes.FLOAD, index)
    else if (tpe == CD_double) mv.visitVarInsn(Opcodes.DLOAD, index)
    else mv.visitVarInsn(Opcodes.ALOAD, index)
  }

  /** Emits an array-allocation instruction for `elmTpe`. */
  def xNewArray(elmTpe: ClassDesc)(implicit mv: MethodVisitor): Unit = {
    if (elmTpe.isPrimitive) mv.visitIntInsn(Opcodes.NEWARRAY, newArrayOperandOf(elmTpe))
    else mv.visitTypeInsn(Opcodes.ANEWARRAY, ClassDescs.internalNameOf(elmTpe))
  }

  /** Emits the pop instruction appropriate for `tpe`. */
  def xPop(tpe: ClassDesc)(implicit mv: MethodVisitor): Unit = {
    if (isCategory2(tpe)) mv.visitInsn(Opcodes.POP2)
    else mv.visitInsn(Opcodes.POP)
  }

  /** Emits the return instruction appropriate for `tpe`. */
  def xReturn(tpe: ClassDesc)(implicit mv: MethodVisitor): Unit = {
    import java.lang.constant.ConstantDescs.*
    if (tpe == CD_boolean || tpe == CD_char || tpe == CD_byte || tpe == CD_short || tpe == CD_int) mv.visitInsn(Opcodes.IRETURN)
    else if (tpe == CD_long) mv.visitInsn(Opcodes.LRETURN)
    else if (tpe == CD_float) mv.visitInsn(Opcodes.FRETURN)
    else if (tpe == CD_double) mv.visitInsn(Opcodes.DRETURN)
    else mv.visitInsn(Opcodes.ARETURN)
  }

  /** Emits the local-store instruction appropriate for `tpe`. */
  def xStore(tpe: ClassDesc, index: Int)(implicit mv: MethodVisitor): Unit = {
    import java.lang.constant.ConstantDescs.*
    if (tpe == CD_boolean || tpe == CD_char || tpe == CD_byte || tpe == CD_short || tpe == CD_int) mv.visitVarInsn(Opcodes.ISTORE, index)
    else if (tpe == CD_long) mv.visitVarInsn(Opcodes.LSTORE, index)
    else if (tpe == CD_float) mv.visitVarInsn(Opcodes.FSTORE, index)
    else if (tpe == CD_double) mv.visitVarInsn(Opcodes.DSTORE, index)
    else mv.visitVarInsn(Opcodes.ASTORE, index)
  }

  /** Swaps two stack values with the given category-2 widths. */
  def xSwap(lowerLarge: Boolean, higherLarge: Boolean)(implicit mv: MethodVisitor): Unit = (lowerLarge, higherLarge) match {
    case (true, true) =>
      mv.visitInstruction(Opcodes.DUP2_X2)
      POP2()
    case (true, false) =>
      DUP_X2()
      POP()
    case (false, true) =>
      mv.visitInstruction(Opcodes.DUP2_X1)
      POP2()
    case (false, false) =>
      SWAP()
  }

  //
  // ~~~~~~~~~~~~~~~~~~~~~~~~~ Local-Variable Utilities ~~~~~~~~~~~~~~~~~~~~~~
  //

  /** A local variable of type `tpe` at `index`. */
  class Variable(val tpe: ClassDesc, index: Int) {

    /** Loads this variable onto the operand stack. */
    def load()(implicit mv: MethodVisitor): Unit = xLoad(tpe, index)

    /** Stores the top operand-stack value in this variable. */
    def store()(implicit mv: MethodVisitor): Unit = xStore(tpe, index)
  }

  /** Loads the current receiver onto the operand stack. */
  def thisLoad()(implicit mv: MethodVisitor): Unit = ALOAD(0)

  /** Runs `body` with the variable of type `tpe` at `index`. */
  def withName(index: Int, tpe: ClassDesc)(body: Variable => Unit): Unit =
    body(new Variable(tpe, index))

  /** Runs `body` with consecutive variables for `tpes` and the next free index. */
  def withNames(index: Int, tpes: List[ClassDesc])(body: (Int, List[Variable]) => Unit): Unit = {
    var runningIndex = index
    val variables = tpes.map(tpe => {
      val variable = new Variable(tpe, runningIndex)
      runningIndex = runningIndex + stackSlotsOf(tpe)
      variable
    })
    body(runningIndex, variables)
  }

  /** Stores a `tpe` value at `index` and runs `body` with its variable. */
  def storeWithName(index: Int, tpe: ClassDesc)(body: Variable => Unit)(implicit mv: MethodVisitor): Unit = {
    xStore(tpe, index)
    body(new Variable(tpe, index))
  }

  //
  // ~~~~~~~~~~~~~~~~~~~~~ Constants and Value Construction ~~~~~~~~~~~~~~~~~~
  //

  /** Pushes the Boolean constant `b` onto the operand stack. */
  def pushBool(b: Boolean)(implicit mv: MethodVisitor): Unit =
    if (b) ICONST_1() else ICONST_0()

  /** Pushes the integer constant `i` using its shortest encoding. */
  def pushInt(i: Int)(implicit mv: MethodVisitor): Unit = i match {
    case -1 => ICONST_M1()
    case 0 => ICONST_0()
    case 1 => ICONST_1()
    case 2 => ICONST_2()
    case 3 => ICONST_3()
    case 4 => ICONST_4()
    case 5 => ICONST_5()
    case _ if scala.Byte.MinValue <= i && i <= scala.Byte.MaxValue => BIPUSH(i.toByte)
    case _ if scala.Short.MinValue <= i && i <= scala.Short.MaxValue => SIPUSH(i.toShort)
    case _ => mv.visitLoadConstantInstruction(i)
  }

  /** Pushes the null reference onto the operand stack. */
  def pushNull()(implicit mv: MethodVisitor): Unit =
    ACONST_NULL()

  /** Pushes the string constant `s` onto the operand stack. */
  def pushString(s: String)(implicit mv: MethodVisitor): Unit =
    mv.visitLoadConstantInstruction(s)

  /** Pushes a reified representation of `loc` onto the operand stack. */
  def pushLoc(loc: SourceLocation)(implicit mv: MethodVisitor): Unit = {
    NEW(GenReifiedSourceLocation.desc)
    DUP()
    pushString(loc.source.name)
    pushInt(loc.startLine)
    pushInt(loc.startCol)
    pushInt(loc.endLine)
    pushInt(loc.endCol)
    INVOKESPECIAL(GenReifiedSourceLocation.Constructor)
  }

  //
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~ Invocation Utilities ~~~~~~~~~~~~~~~~~~~~~~~~
  //

  /** A nominal wrapper around an ASM method handle. */
  sealed case class Handle(handle: asm.Handle)

  /** Returns an invocation handle for static method `m`. */
  def mkStaticHandle(m: StaticMethod): Handle = {
    Handle(new asm.Handle(Opcodes.H_INVOKESTATIC, ClassDescs.internalNameOf(m.clazz), m.name, m.d.descriptorString(), false))
  }

  /** Returns an invocation handle for static interface method `m`. */
  def mkStaticHandle(m: StaticInterfaceMethod): Handle = {
    Handle(new asm.Handle(Opcodes.H_INVOKESTATIC, ClassDescs.internalNameOf(m.clazz), m.name, m.d.descriptorString(), true))
  }

  /** Emits a partially applied static lambda backed by `callHandle`. */
  def mkStaticLambda(lambdaMethod: InterfaceMethod, callD: MethodTypeDesc, callHandle: Handle, drop: Int)(implicit mv: MethodVisitor): Unit = {
    val lambdaAsmType = asm.Type.getMethodType(lambdaMethod.d.descriptorString())
    mv.visitInvokeDynamicInstruction(
      lambdaMethod.name,
      MethodTypeDesc.of(lambdaMethod.clazz, callD.parameterList().asScala.dropRight(drop).toSeq *),
      mkStaticHandle(ClassConstants.LambdaMetafactory.MetafactoryMethod),
      lambdaAsmType,
      callHandle.handle,
      lambdaAsmType
    )
  }

  /** Emits a partially applied static lambda backed by `call`. */
  def mkStaticLambda(lambdaMethod: InterfaceMethod, call: StaticMethod, drop: Int)(implicit mv: MethodVisitor): Unit =
    mkStaticLambda(lambdaMethod, call.d, mkStaticHandle(call), drop)

  /** Emits a partially applied static lambda backed by interface method `call`. */
  def mkStaticLambda(lambdaMethod: InterfaceMethod, call: StaticInterfaceMethod, drop: Int)(implicit mv: MethodVisitor): Unit =
    mkStaticLambda(lambdaMethod, call.d, mkStaticHandle(call), drop)

  /** Invokes the constructor with the given owner and descriptor. */
  def invokeConstructor(className: ClassDesc, descriptor: MethodTypeDesc)(implicit mv: MethodVisitor): Unit =
    INVOKESPECIAL(className, ConstructorMethodName, descriptor)

  //
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~ Structured Control Flow ~~~~~~~~~~~~~~~~~~~~~~
  //

  /** A condition consumed by a structured control-flow emitter. */
  sealed trait Condition

  /** Conditions supported by the structured control-flow emitters. */
  object Condition {

    /** Tests whether the top integer value represents true. */
    case object Bool extends Condition

    /** Tests whether the top integer value equals zero. */
    case object EQ extends Condition

    /** Tests whether the top integer value does not equal zero. */
    case object NE extends Condition

    /** Tests whether the top integer value is less than zero. */
    case object LT extends Condition

    /** Tests whether the top integer value is less than or equal to zero. */
    case object LE extends Condition

    /** Tests whether the top integer value is greater than zero. */
    case object GT extends Condition

    /** Tests whether the top integer value is greater than or equal to zero. */
    case object GE extends Condition

    /** Tests whether the top two integer values are equal. */
    case object ICMPEQ extends Condition

    /** Tests whether the top two integer values are unequal. */
    case object ICMPNE extends Condition

    /** Tests whether the top two references are equal. */
    case object ACMPEQ extends Condition

    /** Tests whether the top two references are unequal. */
    case object ACMPNE extends Condition

    /** Tests whether the top reference is null. */
    case object NULL extends Condition

    /** Tests whether the top reference is non-null. */
    case object NONNULL extends Condition
  }

  /** A side of a two-way bytecode branch. */
  sealed trait Branch

  /** Sides exposed while emitting a two-way bytecode branch. */
  object Branch {

    /** The branch taken when the condition is true. */
    case object TrueBranch extends Branch

    /** The branch taken when the condition is false. */
    case object FalseBranch extends Branch
  }

  /** Emits `i` when `c` holds. */
  def ifCondition(c: Condition)(i: => Unit)(implicit mv: MethodVisitor): Unit = {
    val jumpLabel = new Label()
    mv.visitJumpInstruction(opcodeOf(negated(c)), jumpLabel)
    i
    mv.visitLabel(jumpLabel)
  }

  /** Emits one of two instruction blocks according to `c`. */
  def ifConditionElse(c: Condition)(i: => Unit)(otherwise: => Unit)(implicit mv: MethodVisitor): Unit = {
    val conditionLabel = new Label()
    val endLabel = new Label()
    mv.visitJumpInstruction(opcodeOf(c), conditionLabel)
    otherwise
    mv.visitJumpInstruction(Opcodes.GOTO, endLabel)
    mv.visitLabel(conditionLabel)
    i
    mv.visitLabel(endLabel)
  }

  /** Emits both sides of `c` through the `cases` callback. */
  def branch(c: Condition)(cases: Branch => Unit)(implicit mv: MethodVisitor): Unit = {
    val jumpLabel = new Label()
    val skipLabel = new Label()
    mv.visitJumpInstruction(opcodeOf(c), jumpLabel)

    cases(FalseBranch)
    mv.visitJumpInstruction(Opcodes.GOTO, skipLabel)

    mv.visitLabel(jumpLabel)
    cases(TrueBranch)
    mv.visitLabel(skipLabel)
  }

  /** Emits a loop that executes `i` while evaluating `t` satisfies `c`. */
  def whileLoop(c: Condition)(t: => Unit)(i: => Unit)(implicit mv: MethodVisitor): Unit = {
    val startLabel = new Label()
    val doneLabel = new Label()
    mv.visitLabel(startLabel)
    t
    mv.visitJumpInstruction(opcodeOf(negated(c)), doneLabel)
    i
    mv.visitJumpInstruction(Opcodes.GOTO, startLabel)
    mv.visitLabel(doneLabel)
  }

  /** Emits `body` with a catch-all handler that emits `catchI`. */
  def tryCatch(body: => Unit)(catchI: => Unit)(implicit mv: MethodVisitor): Unit = {
    val beforeTry = new Label()
    val afterTry = new Label()
    val handlerStart = new Label()
    val afterEverything = new Label()
    mv.visitTryCatchBlock(beforeTry, afterTry, handlerStart)
    mv.visitLabel(beforeTry)
    body
    mv.visitLabel(afterTry)
    mv.visitJumpInstruction(Opcodes.GOTO, afterEverything)
    mv.visitLabel(handlerStart)
    catchI
    mv.visitLabel(afterEverything)
  }

  //
  // ~~~~~~~~~~~~~~~~~~~~~ Metadata and Bytecode Recipes ~~~~~~~~~~~~~~~~~~~~~
  //

  /** Associates subsequent bytecode with the start line of `loc`. */
  def addLoc(loc: SourceLocation)(implicit mv: MethodVisitor): Unit = {
    val label = new Label()
    mv.visitLabel(label)
    mv.visitLineNumber(loc.startLine, label)
  }

  /** Emits no bytecode. */
  def nop(): Unit =
    ()

  /** Emits a constructor body that only invokes `superClass`. */
  def nullarySuperConstructor(superClass: ConstructorMethod)(implicit mv: MethodVisitor): Unit = {
    thisLoad()
    INVOKESPECIAL(superClass)
    RETURN()
  }

  /** Emits a static initializer that stores a fresh instance in `singleton`. */
  def singletonStaticConstructor(thisConstructor: ConstructorMethod, singleton: StaticField)(implicit mv: MethodVisitor): Unit = {
    NEW(thisConstructor.clazz)
    DUP()
    INVOKESPECIAL(thisConstructor)
    PUTSTATIC(singleton)
    RETURN()
  }

  /** Emits code that throws an `UnsupportedOperationException` with `msg`. */
  def throwUnsupportedOperationException(msg: String)(implicit mv: MethodVisitor): Unit = {
    NEW(JavaClasses.UnsupportedOperationException)
    DUP()
    pushString(msg)
    INVOKESPECIAL(JavaClasses.UnsupportedOperationException, ConstructorMethodName,
      MethodTypeDesc.of(ConstantDescs.CD_void, JavaClasses.String))
    ATHROW()
  }

  //
  // ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Private ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  //

  /** Returns the JVM jump opcode for `c`. */
  @tailrec
  private def opcodeOf(c: Condition): Int = c match {
    case Condition.Bool => opcodeOf(Condition.NE)
    case Condition.EQ => Opcodes.IFEQ
    case Condition.NE => Opcodes.IFNE
    case Condition.LT => Opcodes.IFLT
    case Condition.LE => Opcodes.IFLE
    case Condition.GT => Opcodes.IFGT
    case Condition.GE => Opcodes.IFGE
    case Condition.ICMPEQ => Opcodes.IF_ICMPEQ
    case Condition.ICMPNE => Opcodes.IF_ICMPNE
    case Condition.ACMPEQ => Opcodes.IF_ACMPEQ
    case Condition.ACMPNE => Opcodes.IF_ACMPNE
    case Condition.NULL => Opcodes.IFNULL
    case Condition.NONNULL => Opcodes.IFNONNULL
  }

  /** Returns the logical negation of `c`. */
  @tailrec
  private def negated(c: Condition): Condition = c match {
    case Condition.Bool => negated(Condition.NE)
    case Condition.EQ => Condition.NE
    case Condition.NE => Condition.EQ
    case Condition.LT => Condition.GE
    case Condition.LE => Condition.GT
    case Condition.GT => Condition.LE
    case Condition.GE => Condition.LT
    case Condition.ICMPEQ => Condition.ICMPNE
    case Condition.ICMPNE => Condition.ICMPEQ
    case Condition.ACMPEQ => Condition.ACMPNE
    case Condition.ACMPNE => Condition.ACMPEQ
    case Condition.NULL => Condition.NONNULL
    case Condition.NONNULL => Condition.NULL
  }

  /** Returns the number of JVM stack slots occupied by `tpe`. */
  private def stackSlotsOf(tpe: ClassDesc): Int = {
    import java.lang.constant.ConstantDescs.*
    if (tpe == CD_long || tpe == CD_double) 2 else 1
  }

  /** Returns the JVM `NEWARRAY` operand for primitive type `tpe`. */
  private def newArrayOperandOf(tpe: ClassDesc): Int = {
    import java.lang.constant.ConstantDescs.*
    if (tpe == CD_boolean) Opcodes.T_BOOLEAN
    else if (tpe == CD_char) Opcodes.T_CHAR
    else if (tpe == CD_byte) Opcodes.T_BYTE
    else if (tpe == CD_short) Opcodes.T_SHORT
    else if (tpe == CD_int) Opcodes.T_INT
    else if (tpe == CD_long) Opcodes.T_LONG
    else if (tpe == CD_float) Opcodes.T_FLOAT
    else if (tpe == CD_double) Opcodes.T_DOUBLE
    else throw InternalCompilerException(s"Unexpected primitive array element type '${tpe.descriptorString()}'", SourceLocation.Unknown)
  }

}

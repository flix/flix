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

  /** A wrapper of [[MethodVisitor]] to improve its interface. */
  implicit class RichMethodVisitor(visitor: MethodVisitor) {
    def visitTypeInstruction(opcode: Int, tpe: ClassDesc): Unit =
      visitor.visitTypeInsn(opcode, ClassDescs.internalNameOf(tpe))

    def visitTypeInstructionDirect(opcode: Int, tpe: String): Unit =
      visitor.visitTypeInsn(opcode, tpe)

    def visitInstruction(opcode: Int): Unit = visitor.visitInsn(opcode)

    def visitMethodInstruction(opcode: Int, owner: ClassDesc, methodName: String, descriptor: MethodTypeDesc, isInterface: Boolean): Unit =
      visitor.visitMethodInsn(opcode, ClassDescs.internalNameOf(owner), methodName, descriptor.descriptorString(), isInterface)

    // TODO: sanitize varags
    def visitInvokeDynamicInstruction(methodName: String, descriptor: MethodTypeDesc, bootstrapMethodHandle: Handle, bootstrapMethodArguments: Any*): Unit =
      visitor.visitInvokeDynamicInsn(methodName, descriptor.descriptorString(), bootstrapMethodHandle.handle, bootstrapMethodArguments *)

    def visitFieldInstruction(opcode: Int, owner: ClassDesc, fieldName: String, fieldType: ClassDesc): Unit =
      visitor.visitFieldInsn(opcode, ClassDescs.internalNameOf(owner), fieldName, fieldType.descriptorString())

    def visitVarInstruction(opcode: Int, v: Int): Unit =
      visitor.visitVarInsn(opcode, v)

    def visitJumpInstruction(opcode: Int, label: Label): Unit =
      visitor.visitJumpInsn(opcode, label)

    def visitLabel(label: Label): Unit =
      visitor.visitLabel(label)

    def visitLineNumber(line: Int, label: Label): Unit =
      visitor.visitLineNumber(line, label)

    def visitLoadConstantInstruction(v: Any): Unit =
      visitor.visitLdcInsn(v)

    def visitIntInstruction(opcode: Int, v: Int): Unit =
      visitor.visitIntInsn(opcode, v)

    def visitTryCatchBlock(beforeTry: Label, afterTry: Label, handlerStart: Label): Unit =
      visitor.visitTryCatchBlock(beforeTry, afterTry, handlerStart, null)
  }

  sealed case class Handle(handle: asm.Handle)

  def mkStaticHandle(m: StaticMethod): Handle = {
    Handle(new asm.Handle(Opcodes.H_INVOKESTATIC, ClassDescs.internalNameOf(m.clazz), m.name, m.d.descriptorString(), false))
  }

  def mkStaticHandle(m: StaticInterfaceMethod): Handle = {
    Handle(new asm.Handle(Opcodes.H_INVOKESTATIC, ClassDescs.internalNameOf(m.clazz), m.name, m.d.descriptorString(), true))
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

  /** A local variable of type `tpe` at index `index`. */
  class Variable(val tpe: ClassDesc, index: Int) {
    def load()(implicit mv: MethodVisitor): Unit = xLoad(tpe, index)

    def store()(implicit mv: MethodVisitor): Unit = xStore(tpe, index)
  }

  //
  // ~~~~~~~~~~~~~~~~~~~~~~~~ Direct JVM Instructions ~~~~~~~~~~~~~~~~~~~~~~~~~
  //

  def ACONST_NULL()(implicit mv: MethodVisitor): Unit = mv.visitInstruction(Opcodes.ACONST_NULL)

  def ALOAD(index: Int)(implicit mv: MethodVisitor): Unit = mv.visitVarInstruction(Opcodes.ALOAD, index)

  def ANEWARRAY(className: ClassDesc)(implicit mv: MethodVisitor): Unit = mv.visitTypeInstruction(Opcodes.ANEWARRAY, className)

  def ARETURN()(implicit mv: MethodVisitor): Unit = mv.visitInstruction(Opcodes.ARETURN)

  def ARRAYLENGTH()(implicit mv: MethodVisitor): Unit = mv.visitInstruction(Opcodes.ARRAYLENGTH)

  def ASTORE(index: Int)(implicit mv: MethodVisitor): Unit =
    mv.visitVarInstruction(Opcodes.ASTORE, index)

  def ATHROW()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.ATHROW)

  def BIPUSH(i: Byte)(implicit mv: MethodVisitor): Unit =
    mv.visitIntInstruction(Opcodes.BIPUSH, i)

  def CHECKCAST(className: ClassDesc)(implicit mv: MethodVisitor): Unit =
    mv.visitTypeInstruction(Opcodes.CHECKCAST, className)

  def DLOAD(index: Int)(implicit mv: MethodVisitor): Unit =
    mv.visitVarInstruction(Opcodes.DLOAD, index)

  def DRETURN()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.DRETURN)

  def DUP()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.DUP)

  def DUP2()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.DUP2)

  def DUP_X1()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.DUP_X1)

  def DUP_X2()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.DUP_X2)

  def GETFIELD(field: InstanceField)(implicit mv: MethodVisitor): Unit =
    mv.visitFieldInstruction(Opcodes.GETFIELD, field.clazz, field.name, field.tpe)

  def GETSTATIC(field: StaticField)(implicit mv: MethodVisitor): Unit =
    mv.visitFieldInstruction(Opcodes.GETSTATIC, field.clazz, field.name, field.tpe)

  def IADD()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.IADD)

  def ICONST_0()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.ICONST_0)

  def ICONST_1()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.ICONST_1)

  def ICONST_2()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.ICONST_2)

  def ICONST_3()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.ICONST_3)

  def ICONST_4()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.ICONST_4)

  def ICONST_5()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.ICONST_5)

  def ICONST_M1()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.ICONST_M1)

  def ILOAD(index: Int)(implicit mv: MethodVisitor): Unit =
    mv.visitVarInstruction(Opcodes.ILOAD, index)

  def INSTANCEOF(tpe: ClassDesc)(implicit mv: MethodVisitor): Unit =
    mv.visitTypeInstruction(Opcodes.INSTANCEOF, tpe)

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

  def mkStaticLambda(lambdaMethod: InterfaceMethod, call: StaticMethod, drop: Int)(implicit mv: MethodVisitor): Unit =
    mkStaticLambda(lambdaMethod, call.d, mkStaticHandle(call), drop)

  def mkStaticLambda(lambdaMethod: InterfaceMethod, call: StaticInterfaceMethod, drop: Int)(implicit mv: MethodVisitor): Unit =
    mkStaticLambda(lambdaMethod, call.d, mkStaticHandle(call), drop)

  def INVOKEINTERFACE(interfaceName: ClassDesc, methodName: String, descriptor: MethodTypeDesc)(implicit mv: MethodVisitor): Unit =
    mv.visitMethodInstruction(Opcodes.INVOKEINTERFACE, interfaceName, methodName, descriptor, isInterface = true)

  def INVOKEINTERFACE(m: InterfaceMethod)(implicit mv: MethodVisitor): Unit =
    mv.visitMethodInstruction(Opcodes.INVOKEINTERFACE, m.clazz, m.name, m.d, isInterface = true)

  def INVOKESPECIAL(className: ClassDesc, methodName: String, descriptor: MethodTypeDesc)(implicit mv: MethodVisitor): Unit = {
    val isInterface = false // OBS this is not technically true if you use it to call private interface methods(?)
    mv.visitMethodInstruction(Opcodes.INVOKESPECIAL, className, methodName, descriptor, isInterface = isInterface)
  }

  def INVOKESPECIAL(c: ConstructorMethod)(implicit mv: MethodVisitor): Unit =
    mv.visitMethodInstruction(Opcodes.INVOKESPECIAL, c.clazz, c.name, c.d, isInterface = false)

  def INVOKESTATIC(className: ClassDesc, methodName: String, descriptor: MethodTypeDesc, isInterface: Boolean = false)(implicit mv: MethodVisitor): Unit =
    mv.visitMethodInstruction(Opcodes.INVOKESTATIC, className, methodName, descriptor, isInterface)

  def INVOKESTATIC(m: StaticMethod)(implicit mv: MethodVisitor): Unit =
    mv.visitMethodInstruction(Opcodes.INVOKESTATIC, m.clazz, m.name, m.d, isInterface = false)

  def INVOKESTATIC(m: StaticInterfaceMethod)(implicit mv: MethodVisitor): Unit =
    mv.visitMethodInstruction(Opcodes.INVOKESTATIC, m.clazz, m.name, m.d, isInterface = true)

  def INVOKEVIRTUAL(className: ClassDesc, methodName: String, descriptor: MethodTypeDesc, isInterface: Boolean = false)(implicit mv: MethodVisitor): Unit =
    mv.visitMethodInstruction(Opcodes.INVOKEVIRTUAL, className, methodName, descriptor, isInterface)

  def INVOKEVIRTUAL(m: AbstractMethod)(implicit mv: MethodVisitor): Unit =
    mv.visitMethodInstruction(Opcodes.INVOKEVIRTUAL, m.clazz, m.name, m.d, isInterface = false)

  def INVOKEVIRTUAL(m: InstanceMethod)(implicit mv: MethodVisitor): Unit =
    mv.visitMethodInstruction(Opcodes.INVOKEVIRTUAL, m.clazz, m.name, m.d, isInterface = false)

  def IRETURN()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.IRETURN)

  def LCMP()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.LCMP)

  def LCONST_0()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.LCONST_0)

  def LCONST_1()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.LCONST_1)

  def LLOAD(index: Int)(implicit mv: MethodVisitor): Unit =
    mv.visitVarInstruction(Opcodes.LLOAD, index)

  def LRETURN()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.LRETURN)

  def NEW(className: ClassDesc)(implicit mv: MethodVisitor): Unit =
    mv.visitTypeInstruction(Opcodes.NEW, className)

  def POP()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.POP)

  def POP2()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.POP2)

  def PUTFIELD(field: InstanceField)(implicit mv: MethodVisitor): Unit =
    mv.visitFieldInstruction(Opcodes.PUTFIELD, field.clazz, field.name, field.tpe)

  def PUTSTATIC(field: StaticField)(implicit mv: MethodVisitor): Unit =
    mv.visitFieldInstruction(Opcodes.PUTSTATIC, field.clazz, field.name, field.tpe)

  def RETURN()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.RETURN)

  def SIPUSH(i: Short)(implicit mv: MethodVisitor): Unit =
    mv.visitIntInstruction(Opcodes.SIPUSH, i)

  def SWAP()(implicit mv: MethodVisitor): Unit =
    mv.visitInstruction(Opcodes.SWAP)

  //
  // ~~~~~~~~~~~~~~~~~~~~~~~~~ Meta JVM Instructions ~~~~~~~~~~~~~~~~~~~~~~~~~~
  //

  def addLoc(loc: SourceLocation)(implicit mv: MethodVisitor): Unit = {
    val label = new Label()
    mv.visitLabel(label)
    mv.visitLineNumber(loc.startLine, label)
  }

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

  /** Emits a `CHECKCAST` of the top of the stack to `tpe`, unless `tpe` is a primitive type. */
  def castIfNotPrim(tpe: ClassDesc)(implicit mv: MethodVisitor): Unit = {
    if (!tpe.isPrimitive) mv.visitTypeInsn(Opcodes.CHECKCAST, ClassDescs.internalNameOf(tpe))
  }

  /// while(c(t)) { i }
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

  def ifCondition(c: Condition)(i: => Unit)(implicit mv: MethodVisitor): Unit = {
    val jumpLabel = new Label()
    mv.visitJumpInstruction(opcodeOf(negated(c)), jumpLabel)
    i
    mv.visitLabel(jumpLabel)
  }

  /**
    * Using [[ifCondition]] uses less jumps, so use that if the conditional code
    * is returns or throws
    */
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

  def invokeConstructor(className: ClassDesc, descriptor: MethodTypeDesc)(implicit mv: MethodVisitor): Unit =
    INVOKESPECIAL(className, ConstructorMethodName, descriptor)

  /** Returns `true` if `tpe` takes two slots on the stack, i.e. it is `long` or `double`. */
  def isCategory2(tpe: ClassDesc): Boolean =
    tpe == ConstantDescs.CD_long || tpe == ConstantDescs.CD_double

  def nop(): Unit =
    ()

  /** `[] --> return`, the body of a constructor that only calls the nullary `superClass` constructor. */
  def nullarySuperConstructor(superClass: ConstructorMethod)(implicit mv: MethodVisitor): Unit = {
    thisLoad()
    INVOKESPECIAL(superClass)
    RETURN()
  }

  def pushBool(b: Boolean)(implicit mv: MethodVisitor): Unit =
    if (b) ICONST_1() else ICONST_0()

  def pushNull()(implicit mv: MethodVisitor): Unit =
    ACONST_NULL()

  def pushString(s: String)(implicit mv: MethodVisitor): Unit =
    mv.visitLoadConstantInstruction(s)

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

  /** `[] --> return`, the body of a static constructor that stores a fresh instance in `singleton`. */
  def singletonStaticConstructor(thisConstructor: ConstructorMethod, singleton: StaticField)(implicit mv: MethodVisitor): Unit = {
    NEW(thisConstructor.clazz)
    DUP()
    INVOKESPECIAL(thisConstructor)
    PUTSTATIC(singleton)
    RETURN()
  }

  /** Emits an `xStore` of `tpe` at `index` and runs `body` with the corresponding [[Variable]]. */
  def storeWithName(index: Int, tpe: ClassDesc)(body: Variable => Unit)(implicit mv: MethodVisitor): Unit = {
    xStore(tpe, index)
    body(new Variable(tpe, index))
  }

  def thisLoad()(implicit mv: MethodVisitor): Unit = ALOAD(0)

  def throwUnsupportedOperationException(msg: String)(implicit mv: MethodVisitor): Unit = {
    NEW(JavaClasses.UnsupportedOperationException)
    DUP()
    pushString(msg)
    INVOKESPECIAL(JavaClasses.UnsupportedOperationException, ConstructorMethodName,
      MethodTypeDesc.of(ConstantDescs.CD_void, JavaClasses.String))
    ATHROW()
  }

  /** Runs `body` with the [[Variable]] of type `tpe` at `index`. */
  def withName(index: Int, tpe: ClassDesc)(body: Variable => Unit): Unit =
    body(new Variable(tpe, index))

  /** Runs `body` with the [[Variable]]s of types `tpes` starting at `index`, and the next free index. */
  def withNames(index: Int, tpes: List[ClassDesc])(body: (Int, List[Variable]) => Unit): Unit = {
    var runningIndex = index
    val variables = tpes.map(tpe => {
      val variable = new Variable(tpe, runningIndex)
      runningIndex = runningIndex + stackSlotsOf(tpe)
      variable
    })
    body(runningIndex, variables)
  }

  /** Emits a `?ALOAD` instruction that loads an element from an array with element type `elmTpe`. */
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

  /** Emits a `?ASTORE` instruction that stores an element into an array with element type `elmTpe`. */
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

  /** Emits a `?LOAD` instruction that loads the local variable of type `tpe` at `index`. */
  def xLoad(tpe: ClassDesc, index: Int)(implicit mv: MethodVisitor): Unit = {
    import java.lang.constant.ConstantDescs.*
    if (tpe == CD_boolean || tpe == CD_char || tpe == CD_byte || tpe == CD_short || tpe == CD_int) mv.visitVarInsn(Opcodes.ILOAD, index)
    else if (tpe == CD_long) mv.visitVarInsn(Opcodes.LLOAD, index)
    else if (tpe == CD_float) mv.visitVarInsn(Opcodes.FLOAD, index)
    else if (tpe == CD_double) mv.visitVarInsn(Opcodes.DLOAD, index)
    else mv.visitVarInsn(Opcodes.ALOAD, index)
  }

  /** Emits a `NEWARRAY` or `ANEWARRAY` instruction that creates an array with element type `elmTpe`. */
  def xNewArray(elmTpe: ClassDesc)(implicit mv: MethodVisitor): Unit = {
    if (elmTpe.isPrimitive) mv.visitIntInsn(Opcodes.NEWARRAY, newArrayOperandOf(elmTpe))
    else mv.visitTypeInsn(Opcodes.ANEWARRAY, ClassDescs.internalNameOf(elmTpe))
  }

  /** Emits a `POP` or `POP2` instruction that pops a value of type `tpe` off the stack. */
  def xPop(tpe: ClassDesc)(implicit mv: MethodVisitor): Unit = {
    import java.lang.constant.ConstantDescs.*
    if (isCategory2(tpe)) mv.visitInsn(Opcodes.POP2)
    else mv.visitInsn(Opcodes.POP)
  }

  /** Emits a `?RETURN` instruction that returns a value of type `tpe`. */
  def xReturn(tpe: ClassDesc)(implicit mv: MethodVisitor): Unit = {
    import java.lang.constant.ConstantDescs.*
    if (tpe == CD_boolean || tpe == CD_char || tpe == CD_byte || tpe == CD_short || tpe == CD_int) mv.visitInsn(Opcodes.IRETURN)
    else if (tpe == CD_long) mv.visitInsn(Opcodes.LRETURN)
    else if (tpe == CD_float) mv.visitInsn(Opcodes.FRETURN)
    else if (tpe == CD_double) mv.visitInsn(Opcodes.DRETURN)
    else mv.visitInsn(Opcodes.ARETURN)
  }

  /** Emits a `?STORE` instruction that stores the top of the stack into the local variable of type `tpe` at `index`. */
  def xStore(tpe: ClassDesc, index: Int)(implicit mv: MethodVisitor): Unit = {
    import java.lang.constant.ConstantDescs.*
    if (tpe == CD_boolean || tpe == CD_char || tpe == CD_byte || tpe == CD_short || tpe == CD_int) mv.visitVarInsn(Opcodes.ISTORE, index)
    else if (tpe == CD_long) mv.visitVarInsn(Opcodes.LSTORE, index)
    else if (tpe == CD_float) mv.visitVarInsn(Opcodes.FSTORE, index)
    else if (tpe == CD_double) mv.visitVarInsn(Opcodes.DSTORE, index)
    else mv.visitVarInsn(Opcodes.ASTORE, index)
  }

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

  /** Returns the number of stack slots (1 or 2) that a value of type `tpe` occupies. */
  private def stackSlotsOf(tpe: ClassDesc): Int = {
    import java.lang.constant.ConstantDescs.*
    if (tpe == CD_long || tpe == CD_double) 2 else 1
  }

  /** Returns the `NEWARRAY` type operand (e.g. [[Opcodes.T_INT]]) of the primitive type `tpe`. */
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

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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.Flix.IrFileExtension
import ca.uwaterloo.flix.language.ast.ReducedAst.{Def, Root}
import ca.uwaterloo.flix.language.ast.{MonoType, Symbol}
import ca.uwaterloo.flix.language.phase.jvm.BytecodeInstructions.{InstructionSet, MethodEnricher}
import ca.uwaterloo.flix.language.phase.jvm.GenExpression.compileInt
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor
import ca.uwaterloo.flix.util.{FileOps, ParOps}
import org.objectweb.asm.Opcodes.*
import org.objectweb.asm.{AnnotationVisitor, Attribute, ClassWriter, Handle, Label, MethodVisitor, Opcodes, TypePath}

import java.nio.file.Path

/**
  * Generates byte code for the function and closure classes.
  */
object GenFunAndClosureClasses {

  /** Print functional call information at runtime if true */
  val onCallDebugging = false

  /**
    * Returns a map of function- and closure-classes for the given set `defs`.
    */
  def gen(defs: Map[Symbol.DefnSym, Def])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    ParOps.parAgg(defs.values, Map.empty[JvmName, JvmClass])({

      case (macc, closure) if isClosure(closure) =>
        val closureName = JvmOps.getClosureClassName(closure.sym)
        val code = genCode(closureName, Closure, closure)
        macc + (closureName -> JvmClass(closureName, code))

      case (macc, defn) if isFunction(defn) =>
        flix.subtask(defn.sym.toString, sample = true)
        val functionName = JvmOps.getFunctionDefinitionClassName(defn.sym)
        val code = genCode(functionName, Function, defn)
        macc + (functionName -> JvmClass(functionName, code))

      case (macc, _) =>
        macc
    }, _ ++ _)
  }

  private def isClosure(defn: Def): Boolean = defn.cparams.nonEmpty

  private def isFunction(defn: Def): Boolean = defn.cparams.isEmpty

  private sealed trait FunctionKind

  private case object Function extends FunctionKind

  private case object Closure extends FunctionKind

  /**
    * `(a|b)` is used to represent the function (left) or closure version (right)
    *
    * {{{
    * public final class (Def$example|Clo$example$152) extends (Fn2$Obj$Int$Obj|Clo2$Obj$Int$Obj) implements Frame {
    *   // locals variables (present for both functions and closures)
    *   public int l0;
    *   public char l1;
    *   public Object l2;
    *   // closure params (assumed empty for functions
    *   public int clo0;
    *   public byte clo1;
    *   // function arguments (present for both functions and closures)
    *   public Object arg0;
    *   public int arg1
    *
    *   public final Result invoke() { return this.applyFrame(null); }
    *
    *   public final Result applyFrame(Value resumptionArg) {
    *     // fields are put into the local frame according to symbol data
    *     int ? = this.l0;
    *     char ? = this.l1;
    *     Object ? = this.l2;
    *
    *     EnterLabel:
    *
    *     int ? = this.clo0;
    *     byte ? = this.clo1;
    *     Object ? = this.arg0;
    *     int ? = this.arg1;
    *
    *     // body code ...
    *   }
    *
    *   public final (Def$example|Clo$example$152) copy {
    *     (Def$example|Clo$example$152) x = new (Def$example|Clo$example$152)();
    *     x.arg0 = this.arg0;
    *     x.arg1 = this.arg1
    *     x.clo0 = this.clo0;
    *     x.clo1 = this.clo1;
    *     x.l0 = this.l0;
    *     x.l1 = this.l1;
    *     x.l2 = this.l2;
    *     return x;
    *   }
    *
    *   // Only for closures
    *   public Clo2$Obj$Int$Obj getUniqueThreadClosure() {
    *     Clo$example$152 x = new Clo$example$152();
    *     x.clo0 = this.clo0;
    *     x.clo1 = this.clo1;
    *     return x;
    *   }
    * }
    * }}}
    */
  private def genCode(className: JvmName, kind: FunctionKind, defn: Def)(implicit root: Root, flix: Flix): Array[Byte] = {
    val visitor = AsmOps.mkClassWriter()

    // Header
    val functionInterface = kind match {
      case Function => JvmOps.getFunctionInterfaceName(defn.arrowType)
      case Closure => JvmOps.getClosureAbstractClassType(defn.arrowType).jvmName
    }
    val frameInterface = BackendObjType.Frame
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, className.toInternalName, null,
      functionInterface.toInternalName, Array(frameInterface.jvmName.toInternalName))

    // Fields
    val closureArgTypes = defn.cparams.map(_.tpe)
    for ((argType, index) <- closureArgTypes.zipWithIndex) {
      val erasedArgType = JvmOps.getErasedJvmType(argType)
      val field = visitor.visitField(ACC_PUBLIC, s"clo$index", erasedArgType.toDescriptor, null, null)
      field.visitEnd()
    }
    for ((x, i) <- defn.lparams.zipWithIndex) {
      visitor.visitField(ACC_PUBLIC, s"l$i", JvmOps.getErasedJvmType(x.tpe).toDescriptor, null, null)
    }
    visitor.visitField(ACC_PUBLIC, "pc", JvmType.PrimInt.toDescriptor, null, null)

    // Methods
    compileConstructor(functionInterface, visitor)
    compileInvokeMethod(visitor, className)
    compileFrameMethod(visitor, className, defn)
    compileCopyMethod(visitor, className, defn)
    if (onCallDebugging) compileOnCall(visitor, className, defn)
    if (kind == Closure) compileGetUniqueThreadClosureMethod(visitor, className, defn)

    visitor.visitEnd()
    visitor.toByteArray
  }

  private def compileConstructor(superClass: JvmName, visitor: ClassWriter): Unit = {
    val constructor = visitor.visitMethod(ACC_PUBLIC, JvmName.ConstructorMethod, MethodDescriptor.NothingToVoid.toDescriptor, null, null)

    constructor.visitVarInsn(ALOAD, 0)
    constructor.visitMethodInsn(INVOKESPECIAL, superClass.toInternalName, JvmName.ConstructorMethod,
      MethodDescriptor.NothingToVoid.toDescriptor, false)
    constructor.visitInsn(RETURN)

    constructor.visitMaxs(999, 999)
    constructor.visitEnd()
  }


  private def compileInvokeMethod(visitor: ClassWriter, className: JvmName): Unit = {
    val m = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, BackendObjType.Thunk.InvokeMethod.name,
      AsmOps.getMethodDescriptor(Nil, JvmType.Reference(BackendObjType.Result.jvmName)), null, null)
    m.visitCode()

    val applyMethod = BackendObjType.Frame.ApplyMethod
    m.visitVarInsn(ALOAD, 0)
    m.visitInsn(ACONST_NULL)
    m.visitMethodInsn(INVOKEVIRTUAL, className.toInternalName, applyMethod.name, applyMethod.d.toDescriptor, false)

    m.visitByteIns(BytecodeInstructions.xReturn(BackendObjType.Result.toTpe))

    m.visitMaxs(999, 999)
    m.visitEnd()
  }

  private def compileFrameMethod(visitor: ClassWriter,
                                 className: JvmName,
                                 defn: Def)(implicit root: Root, flix: Flix): Unit = {
    // Method header
    val applyMethod = BackendObjType.Frame.ApplyMethod
    val m0 = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, applyMethod.name, applyMethod.d.toDescriptor, null, null)
    val m = new Debug(m0)
    val localOffset = 2 // [this: Obj, value: Obj, ...]

    val lparams = defn.lparams.zipWithIndex.map { case (lp, i) => (s"l$i", lp.sym.getStackOffset(localOffset), lp.sym.isWild, lp.tpe) }
    val cparams = defn.cparams.zipWithIndex.map { case (cp, i) => (s"clo$i", cp.sym.getStackOffset(localOffset), false, cp.tpe) }
    val fparams = defn.fparams.zipWithIndex.map { case (fp, i) => (s"arg$i", fp.sym.getStackOffset(localOffset), false, fp.tpe) }

    def loadParamsOf(params: List[(String, Int, Boolean, MonoType)]): Unit = {
      params.foreach { case (name, offset, _, tpe) => loadFromField(m, className, name, offset, tpe) }
    }

    m.visitCode()
    loadParamsOf(lparams)

    // used for self-recursive tail calls
    val enterLabel = new Label()
    m.visitLabel(enterLabel)

    if (onCallDebugging) {
      m.visitVarInsn(ALOAD, 0)
      m.visitVarInsn(ALOAD, 1)
      m.visitMethodInsn(INVOKEVIRTUAL, className.toInternalName, "onCall", MethodDescriptor.mkDescriptor(BackendObjType.Value.toTpe)(VoidableType.Void).toDescriptor, false)
    }

    loadParamsOf(cparams)
    loadParamsOf(fparams)

    val pcLabels: Vector[Label] = Vector.range(0, defn.pcPoints).map(_ => new Label())
    if (defn.pcPoints > 0) {
      // the default label is the starting point of the function if pc = 0
      val defaultLabel = new Label()
      m.visitVarInsn(ALOAD, 0)
      m.visitFieldInsn(GETFIELD, className.toInternalName, "pc", BackendType.Int32.toDescriptor)
      m.visitTableSwitchInsn(1, pcLabels.length, defaultLabel, pcLabels *)
      m.visitLabel(defaultLabel)
    }

    // Generating the expression
    val newFrame = BytecodeInstructions.thisLoad() ~ BytecodeInstructions.cheat(_.visitMethodInsn(INVOKEVIRTUAL, className.toInternalName, copyName, nothingToTDescriptor(className).toDescriptor, false))
    val setPc = {
      import BytecodeInstructions.*
      SWAP() ~ DUP_X1() ~ SWAP() ~ // clo, pc ---> clo, clo, pc
        BytecodeInstructions.cheat(_.visitFieldInsn(Opcodes.PUTFIELD, className.toInternalName, "pc", BackendType.Int32.toDescriptor)) ~
        lparams.foldLeft(nop()) { case (acc, (name, index, isWild, tpe)) =>
          val erasedTpe = BackendType.toErasedBackendType(tpe)
          if (isWild) acc else acc ~ DUP() ~ xLoad(erasedTpe, index) ~ cheat(_.visitFieldInsn(Opcodes.PUTFIELD, className.toInternalName, name, erasedTpe.toDescriptor))
        } ~
        POP()
    }
    val ctx = GenExpression.MethodContext(enterLabel, Map(), newFrame, setPc, localOffset, pcLabels.prepended(null), Array(0))
    GenExpression.compileExpr(defn.expr)(m, ctx, root, flix)
    assert(ctx.pcCounter(0) == pcLabels.size, s"${(className, ctx.pcCounter(0), pcLabels.size)}")

    val returnValue = BytecodeInstructions.xReturn(BackendObjType.Result.toTpe)
    m.visitByteIns(returnValue)

    val path = flix.options.output.getOrElse(Path.of("./build/")).resolve("asts/").resolve("jvm/").resolve(s"${className.toBinaryName}.$IrFileExtension")
    FileOps.writeString(path, m.instructions)
    m.visitMaxs(999, 999)
    m.visitEnd()
  }

  class Debug(m: MethodVisitor) extends MethodVisitor(Opcodes.ASM9) {
    private val sb: StringBuilder = new StringBuilder()

    def instructions: String = sb.toString()

    private def addLine(opcode: Int): Unit = {
      sb.append(opcodeString(opcode))
        .append("\n")
    }

    private def addIntLine(opcode: Int, i: Int): Unit = {
      sb.append(opcodeString(opcode))
        .append(" ")
        .append(i)
        .append("\n")
    }

    private def addStringLine(opcode: Int, s: String): Unit = {
      sb.append(opcodeString(opcode))
        .append(" ")
        .append(s)
        .append("\n")
    }

    override def getDelegate: MethodVisitor =
      m.getDelegate

    override def visitParameter(name: String, access: Int): Unit =
      m.visitParameter(name, access)

    override def visitAnnotationDefault(): AnnotationVisitor =
      m.visitAnnotationDefault()

    override def visitAnnotation(descriptor: String, visible: Boolean): AnnotationVisitor =
      m.visitAnnotation(descriptor, visible)

    override def visitTypeAnnotation(typeRef: Int, typePath: TypePath, descriptor: String, visible: Boolean): AnnotationVisitor =
      m.visitTypeAnnotation(typeRef, typePath, descriptor, visible)

    override def visitAnnotableParameterCount(parameterCount: Int, visible: Boolean): Unit =
      m.visitAnnotableParameterCount(parameterCount, visible)

    override def visitParameterAnnotation(parameter: Int, descriptor: String, visible: Boolean): AnnotationVisitor =
      m.visitParameterAnnotation(parameter, descriptor, visible)

    override def visitAttribute(attribute: Attribute): Unit =
      m.visitAttribute(attribute)

    override def visitCode(): Unit =
      m.visitCode()

    override def visitFrame(`type`: Int, numLocal: Int, local: Array[AnyRef], numStack: Int, stack: Array[AnyRef]): Unit =
      m.visitFrame(`type`, numLocal, local, numStack, stack)

    override def visitInsn(opcode: Int): Unit = {
      addLine(opcode)
      m.visitInsn(opcode)
    }

    override def visitIntInsn(opcode: Int, operand: Int): Unit = {
      addIntLine(opcode, operand)
      m.visitIntInsn(opcode, operand)
    }

    override def visitVarInsn(opcode: Int, varIndex: Int): Unit = {
      addIntLine(opcode, varIndex)
      m.visitVarInsn(opcode, varIndex)
    }

    override def visitTypeInsn(opcode: Int, `type`: String): Unit = {
      addStringLine(opcode, `type`)
      m.visitTypeInsn(opcode, `type`)
    }

    override def visitFieldInsn(opcode: Int, owner: String, name: String, descriptor: String): Unit = {
      sb.append(opcodeString(opcode))
        .append(" ")
        .append(owner)
        .append(" ")
        .append(name)
        .append(" ")
        .append(descriptor)
        .append("\n")
      m.visitFieldInsn(opcode, owner, name, descriptor)
    }

    override def visitMethodInsn(opcode: Int, owner: String, name: String, descriptor: String, isInterface: Boolean): Unit = {
      sb.append(opcodeString(opcode))
        .append(" ")
        .append(owner)
        .append(" ")
        .append(name)
        .append(" ")
        .append(descriptor)
        .append("\n")
      m.visitMethodInsn(opcode, owner, name, descriptor, isInterface)
    }

    override def visitInvokeDynamicInsn(name: String, descriptor: String, bootstrapMethodHandle: Handle, bootstrapMethodArguments: Object*): Unit = {
      sb.append("dynamic")
        .append("\n")
      m.visitInvokeDynamicInsn(name, descriptor, bootstrapMethodHandle, bootstrapMethodArguments *)
    }

    override def visitJumpInsn(opcode: Int, label: Label): Unit = {
      sb.append(opcodeString(opcode))
        .append(" ")
        .append(label.toString)
        .append("\n")
      m.visitJumpInsn(opcode, label)
    }

    override def visitLabel(label: Label): Unit = {
      sb.append("LABEL")
        .append(" ")
        .append(label.toString)
        .append("\n")
      m.visitLabel(label)
    }

    override def visitLdcInsn(value: Any): Unit = {
      sb.append("LDC")
        .append(" ")
        .append(value.toString)
        .append("\n")
      m.visitLdcInsn(value)
    }

    override def visitIincInsn(varIndex: Int, increment: Int): Unit = {
      sb.append("IINC")
        .append(" ")
        .append(varIndex)
        .append(" ")
        .append(increment)
        .append("\n")
      m.visitIincInsn(varIndex, increment)
    }

    override def visitTableSwitchInsn(min: Int, max: Int, dflt: Label, labels: Label*): Unit = {
      sb.append("tableswitch")
        .append("\n")
      m.visitTableSwitchInsn(min, max, dflt, labels *)
    }

    override def visitLookupSwitchInsn(dflt: Label, keys: Array[Int], labels: Array[Label]): Unit = {
      sb.append("lookupswitch")
        .append("\n")
      m.visitLookupSwitchInsn(dflt, keys, labels)
    }

    override def visitMultiANewArrayInsn(descriptor: String, numDimensions: Int): Unit = {
      sb.append("ANEWARRAY")
        .append(" ")
        .append(descriptor)
        .append(" ")
        .append(numDimensions)
        .append("\n")
      m.visitMultiANewArrayInsn(descriptor, numDimensions)
    }

    override def visitInsnAnnotation(typeRef: Int, typePath: TypePath, descriptor: String, visible: Boolean): AnnotationVisitor =
      m.visitInsnAnnotation(typeRef, typePath, descriptor, visible)

    override def visitTryCatchBlock(start: Label, end: Label, handler: Label, `type`: String): Unit = {
      sb.append("TRYCATCHBLOCK")
        .append(" ")
        .append(start.toString)
        .append(" ")
        .append(end.toString)
        .append(" ")
        .append(handler.toString)
        .append(" ")
        .append(`type`)
        .append("\n")
      m.visitTryCatchBlock(start, end, handler, `type`)
    }

    override def visitTryCatchAnnotation(typeRef: Int, typePath: TypePath, descriptor: String, visible: Boolean): AnnotationVisitor =
      m.visitTryCatchAnnotation(typeRef, typePath, descriptor, visible)

    override def visitLocalVariable(name: String, descriptor: String, signature: String, start: Label, end: Label, index: Int): Unit =
      m.visitLocalVariable(name, descriptor, signature, start, end, index)

    override def visitLocalVariableAnnotation(typeRef: Int, typePath: TypePath, start: Array[Label], end: Array[Label], index: Array[Int], descriptor: String, visible: Boolean): AnnotationVisitor =
      m.visitLocalVariableAnnotation(typeRef, typePath, start, end, index, descriptor, visible)

    override def visitLineNumber(line: Int, start: Label): Unit =
      m.visitLineNumber(line, start)

    override def visitMaxs(maxStack: Int, maxLocals: Int): Unit =
      m.visitMaxs(maxStack, maxLocals)

    override def visitEnd(): Unit =
      m.visitEnd()
  }

  private def opcodeString(opcode: Int): String = opcode match {
    case Opcodes.NOP => "NOP"
    case Opcodes.ACONST_NULL => "ACONST_NULL"
    case Opcodes.ICONST_M1 => "ICONST_M1"
    case Opcodes.ICONST_0 => "ICONST_0"
    case Opcodes.ICONST_1 => "ICONST_1"
    case Opcodes.ICONST_2 => "ICONST_2"
    case Opcodes.ICONST_3 => "ICONST_3"
    case Opcodes.ICONST_4 => "ICONST_4"
    case Opcodes.ICONST_5 => "ICONST_5"
    case Opcodes.LCONST_0 => "LCONST_0"
    case Opcodes.LCONST_1 => "LCONST_1"
    case Opcodes.FCONST_0 => "FCONST_0"
    case Opcodes.FCONST_1 => "FCONST_1"
    case Opcodes.FCONST_2 => "FCONST_2"
    case Opcodes.DCONST_0 => "DCONST_0"
    case Opcodes.DCONST_1 => "DCONST_1"
    case Opcodes.BIPUSH => "BIPUSH"
    case Opcodes.SIPUSH => "SIPUSH"
    case Opcodes.LDC => "LDC"
    case Opcodes.ILOAD => "ILOAD"
    case Opcodes.LLOAD => "LLOAD"
    case Opcodes.FLOAD => "FLOAD"
    case Opcodes.DLOAD => "DLOAD"
    case Opcodes.ALOAD => "ALOAD"
    case Opcodes.IALOAD => "IALOAD"
    case Opcodes.LALOAD => "LALOAD"
    case Opcodes.FALOAD => "FALOAD"
    case Opcodes.DALOAD => "DALOAD"
    case Opcodes.AALOAD => "AALOAD"
    case Opcodes.BALOAD => "BALOAD"
    case Opcodes.CALOAD => "CALOAD"
    case Opcodes.SALOAD => "SALOAD"
    case Opcodes.ISTORE => "ISTORE"
    case Opcodes.LSTORE => "LSTORE"
    case Opcodes.FSTORE => "FSTORE"
    case Opcodes.DSTORE => "DSTORE"
    case Opcodes.ASTORE => "ASTORE"
    case Opcodes.IASTORE => "IASTORE"
    case Opcodes.LASTORE => "LASTORE"
    case Opcodes.FASTORE => "FASTORE"
    case Opcodes.DASTORE => "DASTORE"
    case Opcodes.AASTORE => "AASTORE"
    case Opcodes.BASTORE => "BASTORE"
    case Opcodes.CASTORE => "CASTORE"
    case Opcodes.SASTORE => "SASTORE"
    case Opcodes.POP => "POP"
    case Opcodes.POP2 => "POP2"
    case Opcodes.DUP => "DUP"
    case Opcodes.DUP_X1 => "DUP_X1"
    case Opcodes.DUP_X2 => "DUP_X2"
    case Opcodes.DUP2 => "DUP2"
    case Opcodes.DUP2_X1 => "DUP2_X1"
    case Opcodes.DUP2_X2 => "DUP2_X2"
    case Opcodes.SWAP => "SWAP"
    case Opcodes.IADD => "IADD"
    case Opcodes.LADD => "LADD"
    case Opcodes.FADD => "FADD"
    case Opcodes.DADD => "DADD"
    case Opcodes.ISUB => "ISUB"
    case Opcodes.LSUB => "LSUB"
    case Opcodes.FSUB => "FSUB"
    case Opcodes.DSUB => "DSUB"
    case Opcodes.IMUL => "IMUL"
    case Opcodes.LMUL => "LMUL"
    case Opcodes.FMUL => "FMUL"
    case Opcodes.DMUL => "DMUL"
    case Opcodes.IDIV => "IDIV"
    case Opcodes.LDIV => "LDIV"
    case Opcodes.FDIV => "FDIV"
    case Opcodes.DDIV => "DDIV"
    case Opcodes.IREM => "IREM"
    case Opcodes.LREM => "LREM"
    case Opcodes.FREM => "FREM"
    case Opcodes.DREM => "DREM"
    case Opcodes.INEG => "INEG"
    case Opcodes.LNEG => "LNEG"
    case Opcodes.FNEG => "FNEG"
    case Opcodes.DNEG => "DNEG"
    case Opcodes.ISHL => "ISHL"
    case Opcodes.LSHL => "LSHL"
    case Opcodes.ISHR => "ISHR"
    case Opcodes.LSHR => "LSHR"
    case Opcodes.IUSHR => "IUSHR"
    case Opcodes.LUSHR => "LUSHR"
    case Opcodes.IAND => "IAND"
    case Opcodes.LAND => "LAND"
    case Opcodes.IOR => "IOR"
    case Opcodes.LOR => "LOR"
    case Opcodes.IXOR => "IXOR"
    case Opcodes.LXOR => "LXOR"
    case Opcodes.IINC => "IINC"
    case Opcodes.I2L => "I2L"
    case Opcodes.I2F => "I2F"
    case Opcodes.I2D => "I2D"
    case Opcodes.L2I => "L2I"
    case Opcodes.L2F => "L2F"
    case Opcodes.L2D => "L2D"
    case Opcodes.F2I => "F2I"
    case Opcodes.F2L => "F2L"
    case Opcodes.F2D => "F2D"
    case Opcodes.D2I => "D2I"
    case Opcodes.D2L => "D2L"
    case Opcodes.D2F => "D2F"
    case Opcodes.I2B => "I2B"
    case Opcodes.I2C => "I2C"
    case Opcodes.I2S => "I2S"
    case Opcodes.LCMP => "LCMP"
    case Opcodes.FCMPL => "FCMPL"
    case Opcodes.FCMPG => "FCMPG"
    case Opcodes.DCMPL => "DCMPL"
    case Opcodes.DCMPG => "DCMPG"
    case Opcodes.IFEQ => "IFEQ"
    case Opcodes.IFNE => "IFNE"
    case Opcodes.IFLT => "IFLT"
    case Opcodes.IFGE => "IFGE"
    case Opcodes.IFGT => "IFGT"
    case Opcodes.IFLE => "IFLE"
    case Opcodes.IF_ICMPEQ => "IF_ICMPEQ"
    case Opcodes.IF_ICMPNE => "IF_ICMPNE"
    case Opcodes.IF_ICMPLT => "IF_ICMPLT"
    case Opcodes.IF_ICMPGE => "IF_ICMPGE"
    case Opcodes.IF_ICMPGT => "IF_ICMPGT"
    case Opcodes.IF_ICMPLE => "IF_ICMPLE"
    case Opcodes.IF_ACMPEQ => "IF_ACMPEQ"
    case Opcodes.IF_ACMPNE => "IF_ACMPNE"
    case Opcodes.GOTO => "GOTO"
    case Opcodes.JSR => "JSR"
    case Opcodes.RET => "RET"
    case Opcodes.TABLESWITCH => "TABLESWITCH"
    case Opcodes.LOOKUPSWITCH => "LOOKUPSWITCH"
    case Opcodes.IRETURN => "IRETURN"
    case Opcodes.LRETURN => "LRETURN"
    case Opcodes.FRETURN => "FRETURN"
    case Opcodes.DRETURN => "DRETURN"
    case Opcodes.ARETURN => "ARETURN"
    case Opcodes.RETURN => "RETURN"
    case Opcodes.GETSTATIC => "GETSTATIC"
    case Opcodes.PUTSTATIC => "PUTSTATIC"
    case Opcodes.GETFIELD => "GETFIELD"
    case Opcodes.PUTFIELD => "PUTFIELD"
    case Opcodes.INVOKEVIRTUAL => "INVOKEVIRTUAL"
    case Opcodes.INVOKESPECIAL => "INVOKESPECIAL"
    case Opcodes.INVOKESTATIC => "INVOKESTATIC"
    case Opcodes.INVOKEINTERFACE => "INVOKEINTERFACE"
    case Opcodes.INVOKEDYNAMIC => "INVOKEDYNAMIC"
    case Opcodes.NEW => "NEW"
    case Opcodes.NEWARRAY => "NEWARRAY"
    case Opcodes.ANEWARRAY => "ANEWARRAY"
    case Opcodes.ARRAYLENGTH => "ARRAYLENGTH"
    case Opcodes.ATHROW => "ATHROW"
    case Opcodes.CHECKCAST => "CHECKCAST"
    case Opcodes.INSTANCEOF => "INSTANCEOF"
    case Opcodes.MONITORENTER => "MONITORENTER"
    case Opcodes.MONITOREXIT => "MONITOREXIT"
    case Opcodes.MULTIANEWARRAY => "MULTIANEWARRAY"
    case Opcodes.IFNULL => "IFNULL"
    case Opcodes.IFNONNULL => "IFNONNULL"
  }

  private def loadFromField(m: MethodVisitor, className: JvmName, name: String, localIndex: Int, tpe: MonoType)(implicit root: Root): Unit = {
    // retrieve the erased field
    val erasedVarType = JvmOps.getErasedJvmType(tpe)
    m.visitVarInsn(ALOAD, 0)
    m.visitFieldInsn(GETFIELD, className.toInternalName, name, erasedVarType.toDescriptor)
    // cast the value and store it
    val bType = BackendType.toBackendType(tpe)
    m.visitByteIns(BytecodeInstructions.castIfNotPrim(bType))
    m.visitByteIns(BytecodeInstructions.xStore(bType, localIndex))
  }

  /**
    * Make a new `classType` with all the fields set to the same as `this`.
    * A partial copy is without local parameters and without pc
    */
  private def mkCopy(className: JvmName, defn: Def): InstructionSet = {
    import BytecodeInstructions.*
    val pc = List(("pc", MonoType.Int32))
    val fparams = defn.fparams.zipWithIndex.map(p => (s"arg${p._2}", p._1.tpe))
    val cparams = defn.cparams.zipWithIndex.map(p => (s"clo${p._2}", p._1.tpe))
    val lparams = defn.lparams.zipWithIndex.map(p => (s"l${p._2}", p._1.tpe))
    val params = pc ++ fparams ++ cparams ++ lparams

    def getThenPutField(name: String, tpe: MonoType): InstructionSet = cheat(mv => {
      val fieldType = JvmOps.getErasedJvmType(tpe).toDescriptor
      mv.visitFieldInsn(Opcodes.GETFIELD, className.toInternalName, name, fieldType)
      mv.visitFieldInsn(Opcodes.PUTFIELD, className.toInternalName, name, fieldType)
    })

    NEW(className) ~ DUP() ~ INVOKESPECIAL(className, "<init>", MethodDescriptor.NothingToVoid) ~
      params.foldLeft(nop()) {
        case (acc, (name, tpe)) => acc ~ DUP() ~ thisLoad() ~ getThenPutField(name, tpe)
      }
  }

  private val copyName: String = "copy"

  private def nothingToTDescriptor(t: JvmName): MethodDescriptor = {
    MethodDescriptor.mkDescriptor()(t.toTpe)
  }

  private def compileCopyMethod(visitor: ClassWriter, className: JvmName, defn: Def): Unit = {
    val m = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, copyName, nothingToTDescriptor(className).toDescriptor, null, null)
    m.visitCode()

    m.visitByteIns(mkCopy(className, defn))
    m.visitInsn(Opcodes.ARETURN)

    m.visitMaxs(999, 999)
    m.visitEnd()
  }

  private def compileGetUniqueThreadClosureMethod(visitor: ClassWriter, className: JvmName, defn: Def): Unit = {
    val closureAbstractClass = JvmOps.getClosureAbstractClassType(defn.arrowType)
    val m = visitor.visitMethod(ACC_PUBLIC, closureAbstractClass.GetUniqueThreadClosureMethod.name, MethodDescriptor.mkDescriptor()(closureAbstractClass.toTpe).toDescriptor, null, null)
    m.visitCode()

    m.visitByteIns(mkCopy(className, defn))
    m.visitInsn(Opcodes.ARETURN)

    m.visitMaxs(999, 999)
    m.visitEnd()
  }

  private def compileOnCall(v: ClassWriter, className: JvmName, defn: Def): Unit = {
    val m = v.visitMethod(ACC_PUBLIC, "onCall", MethodDescriptor.mkDescriptor(BackendObjType.Value.toTpe)(VoidableType.Void).toDescriptor, null, null)
    m.visitCode()

    val fparams = defn.fparams.zipWithIndex.map(p => (s"arg${p._2}", p._1.tpe))
    val cparams = defn.cparams.zipWithIndex.map(p => (s"clo${p._2}", p._1.tpe))
    val lparams = defn.lparams.zipWithIndex.map(p => (s"l${p._2}", p._1.tpe))
    val params = fparams ++ cparams ++ lparams

    val printStream = JvmName(List("java", "io"), "PrintStream")
    m.visitFieldInsn(GETSTATIC, JvmName(List("java", "lang"), "System").toInternalName, "out", printStream.toDescriptor)

    // The string
    val strings = 3 + params.length
    compileInt(strings)(m)
    m.visitTypeInsn(ANEWARRAY, BackendObjType.String.jvmName.toInternalName)

    m.visitInsn(DUP)
    compileInt(0)(m)
    m.visitLdcInsn(defn.sym.toString)
    m.visitInsn(AASTORE)

    m.visitInsn(DUP)
    compileInt(1)(m)
    m.visitVarInsn(ALOAD, 0)
    m.visitFieldInsn(GETFIELD, className.toInternalName, "pc", BackendType.Int32.toDescriptor)
    m.visitByteIns(BytecodeInstructions.xToString(BackendType.Int32))
    m.visitInsn(AASTORE)

    params.zipWithIndex.foreach {
      case ((fieldName, fieldType), i) =>
        m.visitInsn(DUP)
        compileInt(i + 2)(m)
        m.visitLdcInsn(fieldName)
        m.visitLdcInsn(" = ")
        m.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.String.jvmName.toInternalName, "concat", MethodDescriptor.mkDescriptor(BackendObjType.String.toTpe)(BackendObjType.String.toTpe).toDescriptor, false)
        m.visitVarInsn(ALOAD, 0)
        val bt = BackendType.toErasedBackendType(fieldType)
        m.visitFieldInsn(GETFIELD, className.toInternalName, fieldName, bt.toDescriptor)
        m.visitByteIns(BytecodeInstructions.xToString(bt))
        m.visitMethodInsn(INVOKEVIRTUAL, BackendObjType.String.jvmName.toInternalName, "concat", MethodDescriptor.mkDescriptor(BackendObjType.String.toTpe)(BackendObjType.String.toTpe).toDescriptor, false)
        m.visitInsn(AASTORE)
    }

    m.visitInsn(DUP)
    compileInt(strings - 1)(m)
    m.visitVarInsn(ALOAD, 1)
    m.visitByteIns(BytecodeInstructions.xToString(BackendObjType.Value.toTpe))
    m.visitInsn(AASTORE)

    m.visitLdcInsn(", ")
    m.visitInsn(SWAP)
    m.visitMethodInsn(INVOKESTATIC, "java/lang/String", "join", "(Ljava/lang/CharSequence;[Ljava/lang/CharSequence;)Ljava/lang/String;", false)

    // println
    m.visitMethodInsn(INVOKEVIRTUAL, printStream.toInternalName, "println", MethodDescriptor.mkDescriptor(BackendObjType.String.toTpe)(VoidableType.Void).toDescriptor, false)
    m.visitInsn(RETURN)

    m.visitMaxs(999, 999)
    m.visitEnd()
  }


}

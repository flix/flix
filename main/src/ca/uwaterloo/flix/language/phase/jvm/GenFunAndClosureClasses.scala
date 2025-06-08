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
import ca.uwaterloo.flix.language.ast.ReducedAst.{Def, Root}
import ca.uwaterloo.flix.language.ast.{MonoType, Purity, Symbol}
import ca.uwaterloo.flix.language.phase.jvm.BytecodeInstructions.RichMethodVisitor
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor
import ca.uwaterloo.flix.util.ParOps
import org.objectweb.asm.Opcodes.*
import org.objectweb.asm.{ClassWriter, Label, MethodVisitor, Opcodes}

/**
  * Generates byte code for the function and closure classes.
  */
object GenFunAndClosureClasses {

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
        val functionName = BackendObjType.Defn(defn.sym).jvmName
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
      case Function => JvmOps.getFunctionInterfaceType(defn.arrowType).jvmName
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

    compileConstructor(functionInterface, visitor)
    // Methods
    if (Purity.isControlPure(defn.expr.purity) && kind == Function) {
      compileStaticApplyMethod(visitor, defn)
    }
    compileInvokeMethod(visitor, className)
    compileFrameMethod(visitor, className, defn)
    compileCopyMethod(visitor, className, defn)
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

  private def compileStaticApplyMethod(visitor: ClassWriter, defn: Def)(implicit root: Root, flix: Flix): Unit = {
    // Method header
    val paramTpes = defn.fparams.map(fp => BackendType.toBackendType(fp.tpe))
    val resultTpe = BackendObjType.Result.toTpe
    val desc = MethodDescriptor(paramTpes, resultTpe)
    val modifiers = ACC_PUBLIC + ACC_FINAL + ACC_STATIC
    implicit val m: MethodVisitor = visitor.visitMethod(modifiers, JvmName.DirectApply, desc.toDescriptor, null, null)
    m.visitCode()

    // used for self-recursive tail calls
    val enterLabel = new Label()
    m.visitLabel(enterLabel)

    // Generate the expression
    val localOffset = 0
    val labelEnv = Map.empty[Symbol.LabelSym, Label]
    val ctx = GenExpression.DirectStaticContext(enterLabel, labelEnv, localOffset)
    GenExpression.compileExpr(defn.expr)(m, ctx, root, flix)

    BytecodeInstructions.xReturn(BackendObjType.Result.toTpe)


    m.visitMaxs(999, 999)
    m.visitEnd()
  }

  private def compileInvokeMethod(visitor: ClassWriter, className: JvmName): Unit = {
    implicit val m: MethodVisitor = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, BackendObjType.Thunk.InvokeMethod.name,
      AsmOps.getMethodDescriptor(Nil, JvmType.Reference(BackendObjType.Result.jvmName)), null, null)
    m.visitCode()

    val applyMethod = BackendObjType.Frame.ApplyMethod
    m.visitVarInsn(ALOAD, 0)
    m.visitInsn(ACONST_NULL)
    m.visitMethodInsn(INVOKEVIRTUAL, className.toInternalName, applyMethod.name, applyMethod.d.toDescriptor, false)

    BytecodeInstructions.xReturn(BackendObjType.Result.toTpe)

    m.visitMaxs(999, 999)
    m.visitEnd()
  }

  private def compileFrameMethod(visitor: ClassWriter,
                                 className: JvmName,
                                 defn: Def)(implicit root: Root, flix: Flix): Unit = {
    // Method header
    val applyMethod = BackendObjType.Frame.ApplyMethod
    implicit val m: MethodVisitor = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, applyMethod.name, applyMethod.d.toDescriptor, null, null)
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

    loadParamsOf(cparams)
    loadParamsOf(fparams)

    if (Purity.isControlPure(defn.expr.purity)) {
      val ctx = GenExpression.DirectInstanceContext(enterLabel, Map.empty, localOffset)
      GenExpression.compileExpr(defn.expr)(m, ctx, root, flix)
    } else {
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
      def newFrame(): Unit = {
        BytecodeInstructions.thisLoad()
        m.visitMethodInsn(INVOKEVIRTUAL, className.toInternalName, copyName, nothingToTDescriptor(className).toDescriptor, false)
      }

      def setPc(): Unit = {
        import BytecodeInstructions.*
        SWAP()
        DUP_X1()
        SWAP() // clo, pc ---> clo, clo, pc
        m.visitFieldInsn(Opcodes.PUTFIELD, className.toInternalName, "pc", BackendType.Int32.toDescriptor)
        for ((name, index, isWild, tpe) <- lparams) {
          val erasedTpe = BackendType.toErasedBackendType(tpe)
          if (isWild) {
            nop()
          } else {
            DUP()
            xLoad(erasedTpe, index)
            m.visitFieldInsn(Opcodes.PUTFIELD, className.toInternalName, name, erasedTpe.toDescriptor)
          }
        }
        POP()
      }

      val ctx = GenExpression.EffectContext(enterLabel, Map.empty, _ => newFrame(), _ => setPc(), localOffset, pcLabels.prepended(null), Array(0))
      GenExpression.compileExpr(defn.expr)(m, ctx, root, flix)
      assert(ctx.pcCounter(0) == pcLabels.size, s"${(className, ctx.pcCounter(0), pcLabels.size)}")
    }

    BytecodeInstructions.xReturn(BackendObjType.Result.toTpe)

    m.visitMaxs(999, 999)
    m.visitEnd()
  }

  private def loadFromField(m: MethodVisitor, className: JvmName, name: String, localIndex: Int, tpe: MonoType)(implicit root: Root): Unit = {
    implicit val mm: MethodVisitor = m
    // retrieve the erased field
    val erasedVarType = JvmOps.getErasedJvmType(tpe)
    m.visitVarInsn(ALOAD, 0)
    m.visitFieldInsn(GETFIELD, className.toInternalName, name, erasedVarType.toDescriptor)
    // cast the value and store it
    val bType = BackendType.toBackendType(tpe)
    BytecodeInstructions.castIfNotPrim(bType)
    BytecodeInstructions.xStore(bType, localIndex)
  }

  /**
    * Make a new `classType` with all the fields set to the same as `this`.
    * A partial copy is without local parameters and without pc
    */
  private def mkCopy(className: JvmName, defn: Def)(implicit mv: MethodVisitor): Unit = {
    import BytecodeInstructions.*
    val pc = List(("pc", MonoType.Int32))
    val fparams = defn.fparams.zipWithIndex.map(p => (s"arg${p._2}", p._1.tpe))
    val cparams = defn.cparams.zipWithIndex.map(p => (s"clo${p._2}", p._1.tpe))
    val lparams = defn.lparams.zipWithIndex.map(p => (s"l${p._2}", p._1.tpe))
    val params = pc ++ fparams ++ cparams ++ lparams

    NEW(className)
    DUP()
    INVOKESPECIAL(className, "<init>", MethodDescriptor.NothingToVoid)
    for ((name, tpe) <- params) {
      val fieldType = JvmOps.getErasedJvmType(tpe).toDescriptor
      DUP()
      thisLoad()
      mv.visitFieldInsn(Opcodes.GETFIELD, className.toInternalName, name, fieldType)
      mv.visitFieldInsn(Opcodes.PUTFIELD, className.toInternalName, name, fieldType)
    }
  }

  private val copyName: String = "copy"

  private def nothingToTDescriptor(t: JvmName): MethodDescriptor = {
    MethodDescriptor.mkDescriptor()(t.toTpe)
  }

  private def compileCopyMethod(visitor: ClassWriter, className: JvmName, defn: Def): Unit = {
    implicit val m: MethodVisitor = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, copyName, nothingToTDescriptor(className).toDescriptor, null, null)
    m.visitCode()

    mkCopy(className, defn)
    m.visitInsn(Opcodes.ARETURN)

    m.visitMaxs(999, 999)
    m.visitEnd()
  }

  private def compileGetUniqueThreadClosureMethod(visitor: ClassWriter, className: JvmName, defn: Def): Unit = {
    val closureAbstractClass = JvmOps.getClosureAbstractClassType(defn.arrowType)
    implicit val m: MethodVisitor = visitor.visitMethod(ACC_PUBLIC, closureAbstractClass.GetUniqueThreadClosureMethod.name, MethodDescriptor.mkDescriptor()(closureAbstractClass.toTpe).toDescriptor, null, null)
    m.visitCode()

    mkCopy(className, defn)
    m.visitInsn(Opcodes.ARETURN)

    m.visitMaxs(999, 999)
    m.visitEnd()
  }

}

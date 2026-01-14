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

import ca.uwaterloo.flix.api.{CompilerConstants, Flix}
import ca.uwaterloo.flix.language.ast.JvmAst.{Def, Root}
import ca.uwaterloo.flix.language.ast.{Purity, SimpleType, Symbol}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.StaticMethod
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
        val code = genClosure(closureName, closure)
        macc + (closureName -> JvmClass(closureName, code))

      case (macc, defn) if isFunction(defn) && isControlPure(defn) =>
        val functionName = BackendObjType.Defn(defn.sym).jvmName
        val code = genControlPureFunction(functionName, defn)
        macc + (functionName -> JvmClass(functionName, code))

      case (macc, defn) if isFunction(defn) =>
        val functionName = BackendObjType.Defn(defn.sym).jvmName
        val code = genControlImpureFunction(functionName, defn)
        macc + (functionName -> JvmClass(functionName, code))

      case (macc, _) =>
        macc
    }, _ ++ _)
  }

  private def isClosure(defn: Def): Boolean = defn.cparams.nonEmpty

  private def isFunction(defn: Def): Boolean = defn.cparams.isEmpty

  private def isControlPure(defn: Def): Boolean = Purity.isControlPure(defn.expr.purity)

  /**
    * Generates the following code for control-pure functions.
    *
    * {{{
    * public final class Def$example extends Fn2$Obj$Int$Obj implements Frame {
    *   // function arguments
    *   public Object arg0;
    *   public int arg1
    *
    *   public final Result invoke() { return this.staticApply((Tagged$) this.arg0, this.arg1); }
    *
    *   // Assuming the concrete type of Obj is `Tagged$`
    *   public final Result staticApply(Tagged$ var0, int var1) {
    *     EnterLabel:
    *     // body code ...
    *   }
    * }
    * }}}
    */
  private def genControlPureFunction(className: JvmName, defn: Def)(implicit root: Root, flix: Flix): Array[Byte] = {
    val visitor = AsmOps.mkClassWriter()

    // Header
    val functionInterface = JvmOps.getErasedFunctionInterfaceType(defn.arrowType).jvmName
    visitor.visit(CompilerConstants.JvmTargetVersion, ACC_PUBLIC + ACC_FINAL, className.toInternalName, null,
      functionInterface.toInternalName, null)

    compileConstructor(functionInterface, visitor)

    // Methods
    compileStaticInvokeMethod(visitor, className, defn)
    compileStaticApplyMethod(visitor, className, defn)

    visitor.visitEnd()
    visitor.toByteArray
  }

  /**
    * Generates the following code for control-impure functions.
    *
    * {{{
    * public final class Def$example extends Fn2$Obj$Int$Obj implements Frame {
    *   // locals variables
    *   public int l0;
    *   public char l1;
    *   public String l2;
    *   // function arguments
    *   public Object arg0;
    *   public int arg1
    *
    *   public final Result invoke() { return this.applyFrame(null); }
    *
    *   public final Result applyFrame(Value resumptionArg) {
    *     // fields are put into the local frame according to symbol data
    *     int ? = this.l0;
    *     char ? = this.l1;
    *     String ? = this.l2;
    *
    *     EnterLabel:
    *
    *     Object ? = this.arg0;
    *     int ? = this.arg1;
    *
    *     // body code ...
    *   }
    *
    *   public final Def$example copy {
    *     Def$example x = new Def$example();
    *     x.arg0 = this.arg0;
    *     x.arg1 = this.arg1
    *     x.l0 = this.l0;
    *     x.l1 = this.l1;
    *     x.l2 = this.l2;
    *     return x;
    *   }
    * }
    * }}}
    */
  private def genControlImpureFunction(className: JvmName, defn: Def)(implicit root: Root, flix: Flix): Array[Byte] = {
    val visitor = AsmOps.mkClassWriter()

    // Header
    val functionInterface = JvmOps.getErasedFunctionInterfaceType(defn.arrowType).jvmName
    val frameInterface = BackendObjType.Frame
    visitor.visit(CompilerConstants.JvmTargetVersion, ACC_PUBLIC + ACC_FINAL, className.toInternalName, null,
      functionInterface.toInternalName, Array(frameInterface.jvmName.toInternalName))

    // Fields
    for ((x, i) <- defn.lparams.zipWithIndex) {
      visitor.visitField(ACC_PUBLIC, s"l$i", BackendType.toBackendType(x.tpe).toDescriptor, null, null)
    }
    visitor.visitField(ACC_PUBLIC, "pc", BackendType.Int32.toDescriptor, null, null)

    compileConstructor(functionInterface, visitor)

    // Methods
    compileInvokeMethod(visitor, className)
    compileFrameMethod(visitor, className, defn)
    compileCopyMethod(visitor, className, defn)

    visitor.visitEnd()
    visitor.toByteArray
  }

  /**
    * Generates the following code for closures.
    *
    * {{{
    * public final class Clo$example$152 extends Clo2$Obj$Int$Obj implements Frame {
    *   // locals variables
    *   public int l0;
    *   public char l1;
    *   public String l2;
    *   // closure params
    *   public int clo0;
    *   public byte clo1;
    *   // function arguments
    *   public Object arg0;
    *   public int arg1
    *
    *   public final Result invoke() { return this.applyFrame(null); }
    *
    *   public final Result applyFrame(Value resumptionArg) {
    *     // fields are put into the local frame according to symbol data
    *     int ? = this.l0;
    *     char ? = this.l1;
    *     String ? = this.l2;
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
    *   public final Clo$example$152 copy {
    *     Clo$example$152 x = new Clo$example$152();
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
    *   public Clo2$Obj$Int$Obj getUniqueThreadClosure() {
    *     Clo$example$152 x = new Clo$example$152();
    *     x.clo0 = this.clo0;
    *     x.clo1 = this.clo1;
    *     return x;
    *   }
    * }
    * }}}
    */
  private def genClosure(className: JvmName, defn: Def)(implicit root: Root, flix: Flix): Array[Byte] = {
    val visitor = AsmOps.mkClassWriter()

    // Header
    val functionInterface = JvmOps.getErasedClosureAbstractClassType(defn.arrowType).jvmName
    val frameInterface = BackendObjType.Frame
    visitor.visit(CompilerConstants.JvmTargetVersion, ACC_PUBLIC + ACC_FINAL, className.toInternalName, null,
      functionInterface.toInternalName, Array(frameInterface.jvmName.toInternalName))

    // Fields
    val closureArgTypes = defn.cparams.map(_.tpe)
    for ((argType, index) <- closureArgTypes.zipWithIndex) {
      val field = visitor.visitField(ACC_PUBLIC, s"clo$index", BackendType.toBackendType(argType).toDescriptor, null, null)
      field.visitEnd()
    }
    for ((x, i) <- defn.lparams.zipWithIndex) {
      visitor.visitField(ACC_PUBLIC, s"l$i", BackendType.toBackendType(x.tpe).toDescriptor, null, null)
    }
    visitor.visitField(ACC_PUBLIC, "pc", BackendType.Int32.toDescriptor, null, null)

    compileConstructor(functionInterface, visitor)

    // Methods
    compileInvokeMethod(visitor, className)
    compileFrameMethod(visitor, className, defn)
    compileCopyMethod(visitor, className, defn)
    compileGetUniqueThreadClosureMethod(visitor, className, defn)

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

  private def staticApplyMethod(className: JvmName, defn: Def)(implicit root: Root): StaticMethod =
    StaticMethod(className, JvmName.StaticApply, MethodDescriptor(defn.fparams.map(fp => BackendType.toBackendType(fp.tpe)), BackendObjType.Result.toTpe))

  private def compileStaticApplyMethod(visitor: ClassWriter, className: JvmName, defn: Def)(implicit root: Root, flix: Flix): Unit = {
    // Method header
    val method = staticApplyMethod(className, defn)
    val modifiers = ACC_PUBLIC + ACC_FINAL + ACC_STATIC
    implicit val m: MethodVisitor = visitor.visitMethod(modifiers, method.name, method.d.toDescriptor, null, null)
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

  private def compileStaticInvokeMethod(visitor: ClassWriter, className: JvmName, defn: Def)(implicit root: Root): Unit = {
    implicit val m: MethodVisitor = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, BackendObjType.Thunk.InvokeMethod.name,
      MethodDescriptor.mkDescriptor()(BackendObjType.Result.toTpe).toDescriptor, null, null)
    m.visitCode()

    val functionInterface = JvmOps.getErasedFunctionInterfaceType(defn.arrowType).jvmName
    // Putting args on the Fn class
    for ((fp, i) <- defn.fparams.zipWithIndex) {
      // Load the `this` pointer
      m.visitVarInsn(ALOAD, 0)
      // Load arg i
      m.visitFieldInsn(GETFIELD, functionInterface.toInternalName,
        s"arg$i", BackendType.toErasedBackendType(fp.tpe).toDescriptor)
      // Insert cast to concrete type
      val bTpe = BackendType.toBackendType(fp.tpe)
      BytecodeInstructions.castIfNotPrim(bTpe)
    }

    val method = staticApplyMethod(className, defn)
    m.visitMethodInsn(INVOKESTATIC, className.toInternalName, method.name, method.d.toDescriptor, false)

    BytecodeInstructions.xReturn(BackendObjType.Result.toTpe)

    m.visitMaxs(999, 999)
    m.visitEnd()
  }

  private def compileInvokeMethod(visitor: ClassWriter, className: JvmName): Unit = {
    implicit val m: MethodVisitor = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, BackendObjType.Thunk.InvokeMethod.name,
      MethodDescriptor.mkDescriptor()(BackendObjType.Result.toTpe).toDescriptor, null, null)
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

    val lparams = defn.lparams.zipWithIndex.map { case (lp, i) => (s"l$i", JvmOps.getIndex(lp.offset, localOffset), lp.sym.isWild, BackendType.toBackendType(lp.tpe), None) }
    val cparams = defn.cparams.zipWithIndex.map { case (cp, i) => (s"clo$i", JvmOps.getIndex(cp.offset, localOffset), false, BackendType.toBackendType(cp.tpe), None) }
    val fparams = defn.fparams.zipWithIndex.map { case (fp, i) => (s"arg$i", JvmOps.getIndex(fp.offset, localOffset), false, BackendType.toErasedBackendType(fp.tpe), Some(BackendType.toBackendType(fp.tpe))) }

    def loadParamsOf(params: List[(String, Int, Boolean, BackendType, Option[BackendType])]): Unit = {
      params.foreach { case (name, offset, _, fieldType, castTo) => loadFromField(m, className, name, offset, fieldType, castTo) }
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
      def newFrame(mv: MethodVisitor): Unit = {
        BytecodeInstructions.thisLoad()(mv)
        mv.visitMethodInsn(INVOKEVIRTUAL, className.toInternalName, copyName, nothingToTDescriptor(className).toDescriptor, false)
      }

      def setPc(mv: MethodVisitor): Unit = {
        import BytecodeInstructions.*
        SWAP()(mv)
        DUP_X1()(mv)
        SWAP()(mv) // clo, pc ---> clo, clo, pc
        mv.visitFieldInsn(Opcodes.PUTFIELD, className.toInternalName, "pc", BackendType.Int32.toDescriptor)
        for ((name, index, isWild, fieldType, _: None.type) <- lparams) {
          if (isWild) {
            nop()
          } else {
            DUP()(mv)
            xLoad(fieldType, index)(mv)
            mv.visitFieldInsn(Opcodes.PUTFIELD, className.toInternalName, name, fieldType.toDescriptor)
          }
        }
        POP()(mv)
      }

      val ctx = GenExpression.EffectContext(enterLabel, Map.empty, newFrame, setPc, localOffset, pcLabels.prepended(null), Array(0))
      GenExpression.compileExpr(defn.expr)(m, ctx, root, flix)
      assert(ctx.pcCounter(0) == pcLabels.size, s"${(className, ctx.pcCounter(0), pcLabels.size)}")
    }

    BytecodeInstructions.xReturn(BackendObjType.Result.toTpe)

    m.visitMaxs(999, 999)
    m.visitEnd()
  }

  private def loadFromField(m: MethodVisitor, className: JvmName, name: String, localIndex: Int, fieldType: BackendType, castTo: Option[BackendType]): Unit = {
    implicit val mm: MethodVisitor = m
    // retrieve the erased field
    m.visitVarInsn(ALOAD, 0)
    m.visitFieldInsn(GETFIELD, className.toInternalName, name, fieldType.toDescriptor)
    // cast the value and store it
    castTo match {
      case Some(targetType) =>
        BytecodeInstructions.castIfNotPrim(targetType)
        BytecodeInstructions.xStore(targetType, localIndex)
      case None =>
        BytecodeInstructions.xStore(fieldType, localIndex)
    }
  }

  /**
    * Make a new `classType` with all the fields set to the same as `this`.
    * A partial copy is without local parameters and without pc
    */
  private def mkCopy(className: JvmName, defn: Def)(implicit mv: MethodVisitor, root: Root): Unit = {
    import BytecodeInstructions.*
    val pc = List(("pc", BackendType.Int32))
    val fparams = defn.fparams.zipWithIndex.map(p => (s"arg${p._2}", BackendType.toErasedBackendType(p._1.tpe)))
    val cparams = defn.cparams.zipWithIndex.map(p => (s"clo${p._2}", BackendType.toBackendType(p._1.tpe)))
    val lparams = defn.lparams.zipWithIndex.map(p => (s"l${p._2}", BackendType.toBackendType(p._1.tpe)))
    val params = pc ++ fparams ++ cparams ++ lparams

    NEW(className)
    DUP()
    INVOKESPECIAL(className, JvmName.ConstructorMethod, MethodDescriptor.NothingToVoid)
    for ((name, fieldType) <- params) {
      DUP()
      thisLoad()
      mv.visitFieldInsn(Opcodes.GETFIELD, className.toInternalName, name, fieldType.toDescriptor)
      mv.visitFieldInsn(Opcodes.PUTFIELD, className.toInternalName, name, fieldType.toDescriptor)
    }
  }

  private val copyName: String = "copy"

  private def nothingToTDescriptor(t: JvmName): MethodDescriptor = {
    MethodDescriptor.mkDescriptor()(t.toTpe)
  }

  private def compileCopyMethod(visitor: ClassWriter, className: JvmName, defn: Def)(implicit root: Root): Unit = {
    implicit val m: MethodVisitor = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, copyName, nothingToTDescriptor(className).toDescriptor, null, null)
    m.visitCode()

    mkCopy(className, defn)
    m.visitInsn(Opcodes.ARETURN)

    m.visitMaxs(999, 999)
    m.visitEnd()
  }

  private def compileGetUniqueThreadClosureMethod(visitor: ClassWriter, className: JvmName, defn: Def)(implicit root: Root): Unit = {
    val closureAbstractClass = JvmOps.getErasedClosureAbstractClassType(defn.arrowType)
    implicit val m: MethodVisitor = visitor.visitMethod(ACC_PUBLIC, closureAbstractClass.GetUniqueThreadClosureMethod.name, MethodDescriptor.mkDescriptor()(closureAbstractClass.toTpe).toDescriptor, null, null)
    m.visitCode()

    mkCopy(className, defn)
    m.visitInsn(Opcodes.ARETURN)

    m.visitMaxs(999, 999)
    m.visitEnd()
  }

}

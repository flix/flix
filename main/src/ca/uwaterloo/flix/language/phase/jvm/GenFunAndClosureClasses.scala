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

import ca.uwaterloo.flix.api.{CompilerConstants, Flix, FlixEvent}
import ca.uwaterloo.flix.language.ast.JvmAst.{Def, Root}
import ca.uwaterloo.flix.language.ast.{Purity, SimpleType, Symbol}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.StaticMethod
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.classes.{GenFrame, GenResult, GenThunk, GenValue}
import ca.uwaterloo.flix.util.{ClassDescs, ParOps}
import org.objectweb.asm.{ClassWriter, Label, MethodVisitor, Opcodes}

import java.lang.constant.{ClassDesc, MethodTypeDesc}
import java.lang.constant.ConstantDescs.CD_int

/**
  * Generates byte code for the function and closure classes.
  */
object GenFunAndClosureClasses {

  /**
    * Returns the descriptor of the function class `Def$Name` of `sym`.
    *
    * String.charAt     =>    String/Def$charAt
    * List.length       =>    List/Def$length
    */
  def defnDesc(sym: Symbol.DefnSym): ClassDesc =
    Mangle.mkDesc(sym.namespace, Mangle.mkClassName("Def", sym.name))

  /**
    * Returns the descriptor of the closure class `Clo$Name` of `sym`.
    *
    * String.charAt     =>    String/Clo$charAt
    * List.map          =>    List/Clo$map
    */
  def closureDesc(sym: Symbol.DefnSym): ClassDesc =
    Mangle.mkDesc(sym.namespace, Mangle.mkClassName("Clo", sym.name))

  /**
    * Returns a map of function- and closure-classes for the given set `defs`.
    */
  def gen(defs: Map[Symbol.DefnSym, Def])(implicit root: Root, flix: Flix): Map[ClassDesc, JvmClass] = {
    ParOps.parAgg(defs.values, Map.empty[ClassDesc, JvmClass])({

      case (macc, closure) if isClosure(closure) =>
        flix.profile(closure.sym, closure.loc) {
          val closureName = closureDesc(closure.sym)
          val code = genClosure(closureName, closure)
          flix.emitEvent(FlixEvent.EmittedClass(closure.sym, code.length))
          macc + (closureName -> JvmClass(closureName, code))
        }

      case (macc, defn) if isFunction(defn) && isControlPure(defn) =>
        flix.profile(defn.sym, defn.loc) {
          val functionName = defnDesc(defn.sym)
          val code = genControlPureFunction(functionName, defn)
          flix.emitEvent(FlixEvent.EmittedClass(defn.sym, code.length))
          macc + (functionName -> JvmClass(functionName, code))
        }

      case (macc, defn) if isFunction(defn) =>
        flix.profile(defn.sym, defn.loc) {
          val functionName = defnDesc(defn.sym)
          val code = genControlImpureFunction(functionName, defn)
          flix.emitEvent(FlixEvent.EmittedClass(defn.sym, code.length))
          macc + (functionName -> JvmClass(functionName, code))
        }

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
  private def genControlPureFunction(className: ClassDesc, defn: Def)(implicit root: Root, flix: Flix): Array[Byte] = {
    val visitor = ClassMaker.mkClassWriter()

    // Header
    val functionInterface = BackendObjType.Arrow.fromArrowType(defn.arrowType).desc
    visitor.visit(CompilerConstants.JvmTargetVersion, Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL, ClassDescs.internalNameOf(className), null,
      ClassDescs.internalNameOf(functionInterface), null)
    visitor.visitSource(defn.loc.source.name, null)

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
  private def genControlImpureFunction(className: ClassDesc, defn: Def)(implicit root: Root, flix: Flix): Array[Byte] = {
    val visitor = ClassMaker.mkClassWriter()

    // Header
    val functionInterface = BackendObjType.Arrow.fromArrowType(defn.arrowType).desc
    val frameInterface = GenFrame
    visitor.visit(CompilerConstants.JvmTargetVersion, Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL, ClassDescs.internalNameOf(className), null,
      ClassDescs.internalNameOf(functionInterface), Array(ClassDescs.internalNameOf(frameInterface.desc)))
    visitor.visitSource(defn.loc.source.name, null)

    // Fields — lparams use erased types (like fparams) so setPc can store without casting
    for ((x, i) <- defn.lparams.zipWithIndex) {
      visitor.visitField(Opcodes.ACC_PUBLIC, s"l$i", TypeDescs.toErasedClassDesc(x.tpe).descriptorString(), null, null)
    }
    visitor.visitField(Opcodes.ACC_PUBLIC, "pc", CD_int.descriptorString(), null, null)

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
  private def genClosure(className: ClassDesc, defn: Def)(implicit root: Root, flix: Flix): Array[Byte] = {
    val visitor = ClassMaker.mkClassWriter()

    // Header
    val functionInterface = BackendObjType.AbstractArrow.fromArrowType(defn.arrowType).desc
    val frameInterface = GenFrame
    visitor.visit(CompilerConstants.JvmTargetVersion, Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL, ClassDescs.internalNameOf(className), null,
      ClassDescs.internalNameOf(functionInterface), Array(ClassDescs.internalNameOf(frameInterface.desc)))
    visitor.visitSource(defn.loc.source.name, null)

    // Fields
    val closureArgTypes = defn.cparams.map(_.tpe)
    for ((argType, index) <- closureArgTypes.zipWithIndex) {
      val field = visitor.visitField(Opcodes.ACC_PUBLIC, s"clo$index", TypeDescs.toClassDesc(argType).descriptorString(), null, null)
      field.visitEnd()
    }
    // lparams use erased types (like fparams) so setPc can store without casting
    for ((x, i) <- defn.lparams.zipWithIndex) {
      visitor.visitField(Opcodes.ACC_PUBLIC, s"l$i", TypeDescs.toErasedClassDesc(x.tpe).descriptorString(), null, null)
    }
    visitor.visitField(Opcodes.ACC_PUBLIC, "pc", CD_int.descriptorString(), null, null)

    compileConstructor(functionInterface, visitor)

    // Methods
    compileInvokeMethod(visitor, className)
    compileFrameMethod(visitor, className, defn)
    compileCopyMethod(visitor, className, defn)
    compileGetUniqueThreadClosureMethod(visitor, className, defn)

    visitor.visitEnd()
    visitor.toByteArray
  }

  private def compileConstructor(superClass: ClassDesc, visitor: ClassWriter): Unit = {
    val constructor = visitor.visitMethod(Opcodes.ACC_PUBLIC, ClassMaker.ConstructorMethodName, MethodTypeDescs.NothingToVoid.descriptorString(), null, null)

    constructor.visitVarInsn(Opcodes.ALOAD, 0)
    constructor.visitMethodInsn(Opcodes.INVOKESPECIAL, ClassDescs.internalNameOf(superClass), ClassMaker.ConstructorMethodName,
      MethodTypeDescs.NothingToVoid.descriptorString(), false)
    constructor.visitInsn(Opcodes.RETURN)

    constructor.visitMaxs(999, 999)
    constructor.visitEnd()
  }

  private def staticApplyMethod(className: ClassDesc, defn: Def)(implicit root: Root): StaticMethod =
    StaticMethod(className, ClassMaker.StaticApplyMethodName, MethodTypeDescs.mkDescriptor(defn.fparams.map(fp => TypeDescs.toClassDesc(fp.tpe)) *)(GenResult.desc))

  private def compileStaticApplyMethod(visitor: ClassWriter, className: ClassDesc, defn: Def)(implicit root: Root, flix: Flix): Unit = {
    // Method header
    val method = staticApplyMethod(className, defn)
    val modifiers = Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL + Opcodes.ACC_STATIC
    implicit val m: MethodVisitor = visitor.visitMethod(modifiers, method.name, method.d.descriptorString(), null, null)
    m.visitCode()
    addLoc(defn.loc)

    // used for self-recursive tail calls
    val enterLabel = new Label()
    m.visitLabel(enterLabel)

    // Generate the expression
    val localOffset = 0
    val labelEnv = Map.empty[Symbol.LabelSym, Label]
    val ctx = GenExpression.DirectStaticContext(enterLabel, labelEnv, localOffset)
    GenExpression.compileExpr(defn.expr)(m, ctx, root, flix)

    xReturn(GenResult.desc)


    m.visitMaxs(999, 999)
    m.visitEnd()
  }

  private def compileStaticInvokeMethod(visitor: ClassWriter, className: ClassDesc, defn: Def)(implicit root: Root): Unit = {
    implicit val m: MethodVisitor = visitor.visitMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL, GenThunk.InvokeMethod.name,
      MethodTypeDescs.mkDescriptor()(GenResult.desc).descriptorString(), null, null)
    m.visitCode()

    val functionInterface = BackendObjType.Arrow.fromArrowType(defn.arrowType).desc
    // Putting args on the Fn class
    for ((fp, i) <- defn.fparams.zipWithIndex) {
      // Load the `this` pointer
      m.visitVarInsn(Opcodes.ALOAD, 0)
      // Load arg i
      m.visitFieldInsn(Opcodes.GETFIELD, ClassDescs.internalNameOf(functionInterface),
        s"arg$i", TypeDescs.toErasedClassDesc(fp.tpe).descriptorString())
      // Insert cast to concrete type
      castIfNotPrim(TypeDescs.toClassDesc(fp.tpe))
    }

    val method = staticApplyMethod(className, defn)
    m.visitMethodInsn(Opcodes.INVOKESTATIC, ClassDescs.internalNameOf(className), method.name, method.d.descriptorString(), false)

    xReturn(GenResult.desc)

    m.visitMaxs(999, 999)
    m.visitEnd()
  }

  private def compileInvokeMethod(visitor: ClassWriter, className: ClassDesc): Unit = {
    implicit val m: MethodVisitor = visitor.visitMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL, GenThunk.InvokeMethod.name,
      MethodTypeDescs.mkDescriptor()(GenResult.desc).descriptorString(), null, null)
    m.visitCode()

    val applyMethod = GenFrame.ApplyMethod
    m.visitVarInsn(Opcodes.ALOAD, 0)
    m.visitInsn(Opcodes.ACONST_NULL)
    m.visitMethodInsn(Opcodes.INVOKEVIRTUAL, ClassDescs.internalNameOf(className), applyMethod.name, applyMethod.d.descriptorString(), false)

    xReturn(GenResult.desc)

    m.visitMaxs(999, 999)
    m.visitEnd()
  }

  private def compileFrameMethod(visitor: ClassWriter,
                                 className: ClassDesc,
                                 defn: Def)(implicit root: Root, flix: Flix): Unit = {
    // Method header
    val classInternalName = ClassDescs.internalNameOf(className)
    val applyMethod = GenFrame.ApplyMethod
    implicit val m: MethodVisitor = visitor.visitMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL, applyMethod.name, applyMethod.d.descriptorString(), null, null)
    val localOffset = 2 // [this: Obj, value: Obj, ...]

    val lparams = defn.lparams.zipWithIndex.map { case (lp, i) => (s"l$i", lp.offset + localOffset, lp.sym.isWild, TypeDescs.toErasedClassDesc(lp.tpe), Some(TypeDescs.toClassDesc(lp.tpe))) }
    val cparams = defn.cparams.zipWithIndex.map { case (cp, i) => (s"clo$i", cp.offset + localOffset, false, TypeDescs.toClassDesc(cp.tpe), None) }
    val fparams = defn.fparams.zipWithIndex.map { case (fp, i) => (s"arg$i", fp.offset + localOffset, false, TypeDescs.toErasedClassDesc(fp.tpe), Some(TypeDescs.toClassDesc(fp.tpe))) }

    def loadParamsOf(params: List[(String, Int, Boolean, ClassDesc, Option[ClassDesc])]): Unit = {
      params.foreach { case (name, offset, _, fieldType, castTo) => loadFromField(m, className, name, offset, fieldType, castTo) }
    }

    m.visitCode()
    addLoc(defn.loc)
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
        m.visitVarInsn(Opcodes.ALOAD, 0)
        m.visitFieldInsn(Opcodes.GETFIELD, classInternalName, "pc", CD_int.descriptorString())
        m.visitTableSwitchInsn(1, pcLabels.length, defaultLabel, pcLabels *)
        m.visitLabel(defaultLabel)
      }

      // Generating the expression
      def newFrame(mv: MethodVisitor): Unit = {
        thisLoad()(mv)
        mv.visitMethodInsn(Opcodes.INVOKEVIRTUAL, classInternalName, copyName, nothingToTDescriptor(className).descriptorString(), false)
      }

      def setPc(mv: MethodVisitor): Unit = {
        SWAP()(mv)
        DUP_X1()(mv)
        SWAP()(mv) // clo, pc ---> clo, clo, pc
        mv.visitFieldInsn(Opcodes.PUTFIELD, classInternalName, "pc", CD_int.descriptorString())
        for ((name, index, isWild, fieldType, _) <- lparams) {
          if (isWild) {
            nop()
          } else {
            DUP()(mv)
            // fieldType is erased (Object for refs), so xLoad always matches the verifier type
            xLoad(fieldType, index)(mv)
            mv.visitFieldInsn(Opcodes.PUTFIELD, classInternalName, name, fieldType.descriptorString())
          }
        }
        POP()(mv)
      }

      // Re-narrow local variable types after resuming at a pcPointLabel.
      // The JVM verifier merges local types at the tableswitch targets; this
      // restores the exact declared types by loading, casting, and re-storing
      // each non-wild, non-primitive local.
      def narrowLocals(mv: MethodVisitor): Unit = {
        for ((_, index, isWild, erasedType, Some(realType)) <- lparams) {
          if (!isWild) {
            if (!realType.isPrimitive) { // primitives don't need narrowing
              xLoad(erasedType, index)(mv)
              castIfNotPrim(realType)(mv)
              xStore(realType, index)(mv)
            }
          }
        }
      }

      val ctx = GenExpression.EffectContext(enterLabel, Map.empty, newFrame, setPc, narrowLocals, localOffset, pcLabels.prepended(null), Array(0))
      GenExpression.compileExpr(defn.expr)(m, ctx, root, flix)
      assert(ctx.pcCounter(0) == pcLabels.size, s"${(className, ctx.pcCounter(0), pcLabels.size)}")
    }

    xReturn(GenResult.desc)

    m.visitMaxs(999, 999)
    m.visitEnd()
  }

  private def loadFromField(m: MethodVisitor, className: ClassDesc, name: String, localIndex: Int, fieldType: ClassDesc, castTo: Option[ClassDesc]): Unit = {
    implicit val mm: MethodVisitor = m
    // retrieve the erased field
    m.visitVarInsn(Opcodes.ALOAD, 0)
    m.visitFieldInsn(Opcodes.GETFIELD, ClassDescs.internalNameOf(className), name, fieldType.descriptorString())
    // cast the value and store it
    castTo match {
      case Some(targetType) =>
        castIfNotPrim(targetType)
        xStore(targetType, localIndex)
      case None =>
        xStore(fieldType, localIndex)
    }
  }

  /**
    * Make a new `classType` with all the fields set to the same as `this`.
    * A partial copy is without local parameters and without pc
    */
  private def mkCopy(className: ClassDesc, defn: Def)(implicit mv: MethodVisitor, root: Root): Unit = {
    val classInternalName = ClassDescs.internalNameOf(className)
    val pc = List(("pc", CD_int))
    val fparams = defn.fparams.zipWithIndex.map(p => (s"arg${p._2}", TypeDescs.toErasedClassDesc(p._1.tpe)))
    val cparams = defn.cparams.zipWithIndex.map(p => (s"clo${p._2}", TypeDescs.toClassDesc(p._1.tpe)))
    val lparams = defn.lparams.zipWithIndex.map(p => (s"l${p._2}", TypeDescs.toErasedClassDesc(p._1.tpe)))
    val params = pc ++ fparams ++ cparams ++ lparams

    NEW(className)
    DUP()
    INVOKESPECIAL(className, ClassMaker.ConstructorMethodName, MethodTypeDescs.NothingToVoid)
    for ((name, fieldType) <- params) {
      DUP()
      thisLoad()
      mv.visitFieldInsn(Opcodes.GETFIELD, classInternalName, name, fieldType.descriptorString())
      mv.visitFieldInsn(Opcodes.PUTFIELD, classInternalName, name, fieldType.descriptorString())
    }
  }

  private val copyName: String = "copy"

  private def nothingToTDescriptor(t: ClassDesc): MethodTypeDesc = {
    MethodTypeDesc.of(t)
  }

  private def compileCopyMethod(visitor: ClassWriter, className: ClassDesc, defn: Def)(implicit root: Root): Unit = {
    implicit val m: MethodVisitor = visitor.visitMethod(Opcodes.ACC_PUBLIC + Opcodes.ACC_FINAL, copyName, nothingToTDescriptor(className).descriptorString(), null, null)
    m.visitCode()

    mkCopy(className, defn)
    m.visitInsn(Opcodes.ARETURN)

    m.visitMaxs(999, 999)
    m.visitEnd()
  }

  private def compileGetUniqueThreadClosureMethod(visitor: ClassWriter, className: ClassDesc, defn: Def)(implicit root: Root): Unit = {
    val closureAbstractClass = BackendObjType.AbstractArrow.fromArrowType(defn.arrowType)
    implicit val m: MethodVisitor = visitor.visitMethod(Opcodes.ACC_PUBLIC, closureAbstractClass.GetUniqueThreadClosureMethod.name, MethodTypeDescs.mkDescriptor()(closureAbstractClass.desc).descriptorString(), null, null)
    m.visitCode()

    mkCopy(className, defn)
    m.visitInsn(Opcodes.ARETURN)

    m.visitMaxs(999, 999)
    m.visitEnd()
  }

}

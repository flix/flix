package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ReducedAst.{Effect, Op, Root}
import ca.uwaterloo.flix.language.ast.{MonoType, Symbol}
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor
import ca.uwaterloo.flix.util.{InternalCompilerException, ParOps}
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes.*

/** An effect class like this:
  * {{{
  * eff SomeEffect {
  *     pub def flip(): Bool
  *     pub def add(x: Int32, y: Int32): Int32
  * }
  * }}}
  * Is conceptually understood as (the input types of `cont` are actually boxed in `Value`):
  * {{{
  * eff SomeEffect {
  *     pub def flip(unit: Unit, cont: Bool -> Result): Value
  *     pub def add(x: Int32, y: Int32, cont: Int32 -> Result): Value
  * }
  * }}}
  * and is generated like so:
  * {{{
  * public final class Eff$SomeEffect implements Handler {
  *     public Fn2$Obj$Obj$Obj flip;
  *     public Fn3$Int32$Int32&Obj$Obj add;
  *
  *     public static EffectCall flip(Object var0, Handler h, Resumption r) {
  *         Fn2$Obj$Obj$Obj f = ((Eff$SomeEffect) h).flip;
  *         f.arg0 = var0;
  *         f.arg1 = new ResumptionWrapper(r);
  *         return f.invoke();
  *     }
  *
  *     public static EffectCall add(Int var0, Int var1, Handler h, Resumption r) {
  *         Fn2$Obj$Obj$Obj f = ((Eff$SomeEffect) h).flip;
  *         f.arg0 = var0;
  *         f.arg1 = var1;
  *         f.arg2 = new ResumptionWrapper(r);
  *         return f.invoke();
  *     }
  * }
  * }}}
  */
object GenEffectClasses {

  def gen(effects: Iterable[Effect])(implicit flix: Flix): Map[JvmName, JvmClass] = {
    ParOps.parAgg(effects, Map.empty[JvmName, JvmClass])({
      case (macc, effect) =>
        val classType = JvmOps.getEffectDefinitionClassType(effect.sym)
        val className = classType.name
        macc + (className -> JvmClass(className, genByteCode(classType, effect)))
    }, _ ++ _)
  }

  private def genByteCode(effectType: JvmType.Reference, effect: Effect)(implicit flix: Flix): Array[Byte] = {
    val visitor = AsmOps.mkClassWriter()

    val interfaces = Array(BackendObjType.Handler.jvmName.toInternalName)
    val superClass = BackendObjType.JavaObject.jvmName.toInternalName

    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, effectType.name.toInternalName,
      null, superClass, interfaces)

    for (op <- effect.ops) genFieldAndMethod(visitor, effectType, op)

    genConstructor(visitor, superClass)

    visitor.visitEnd()
    visitor.toByteArray
  }

  private def genConstructor(visitor: ClassWriter, superClass: String): Unit = {
    val mv = visitor.visitMethod(ACC_PUBLIC, JvmName.ConstructorMethod, MethodDescriptor.NothingToVoid.toDescriptor, null, null)
    mv.visitCode()

    mv.visitVarInsn(ALOAD, 0)
    mv.visitMethodInsn(INVOKESPECIAL, superClass, JvmName.ConstructorMethod, MethodDescriptor.NothingToVoid.toDescriptor, false)
    mv.visitInsn(RETURN)

    mv.visitMaxs(999, 999)
    mv.visitEnd()
  }

  private def genFieldAndMethod(visitor: ClassWriter, effectType: JvmType.Reference, op: Op): Unit = {
    // Field
    val writtenOpArgsMono = op.fparams.map(_.tpe)
    val arrowType = MonoType.Arrow(writtenOpArgsMono :+ MonoType.Object, MonoType.Object)

    val resumption = JvmType.Reference(BackendObjType.Resumption.jvmName)
    val writtenOpArgs = writtenOpArgsMono.map(JvmOps.getErasedJvmType)
    val opName = JvmOps.getEffectOpName(op.sym)
    val opFunctionType = JvmOps.getFunctionInterfaceType(arrowType)
    visitor.visitField(ACC_PUBLIC, opName, opFunctionType.toDescriptor, null, null)
    // Method
    // 1. Cast the given generic handler to the current effect
    // 2. Convert the given resumption into a callable Fn1$Obj (Value -> Result) via ResumptionWrapper
    // 3. call invoke on the op
    val (writtenOpArgsOffsetRev, handlerOffset) = writtenOpArgs.foldLeft((Nil: List[(JvmType, Int)], 0)) {
      case ((acc, prev), arg) => ((arg, prev) :: acc, prev + AsmOps.getStackSize(arg))
    }
    val writtenOpArgsOffset = writtenOpArgsOffsetRev.reverse
    val handlerType = JvmType.Reference(BackendObjType.Handler.jvmName)
    val methodArgs = writtenOpArgs ++ List(handlerType, resumption)
    val methodResult = JvmType.Reference(BackendObjType.Result.jvmName)
    val effectName = effectType.name.toInternalName
    val mv = visitor.visitMethod(ACC_PUBLIC + ACC_STATIC, opName, AsmOps.getMethodDescriptor(methodArgs, methodResult), null, null)
    mv.visitCode()

    mv.visitVarInsn(ALOAD, handlerOffset)
    mv.visitTypeInsn(CHECKCAST, effectName)
    mv.visitFieldInsn(GETFIELD, effectName, opName, opFunctionType.toDescriptor)
    // bind all regular arguments
    for (((t, localOffset), i) <- writtenOpArgsOffset.zipWithIndex) {
      val xLoad = AsmOps.getLoadInstruction(t)
      mv.visitInsn(DUP)
      mv.visitVarInsn(xLoad, localOffset)
      mv.visitFieldInsn(PUTFIELD, opFunctionType.name.toInternalName, s"arg$i", t.toDescriptor)
    }
    // convert the resumption to a function
    mv.visitInsn(DUP)

    val wrapperType = BackendObjType.ResumptionWrapper(BackendType.asErasedBackendType(op.tpe))
    val wrapperName = wrapperType.jvmName.toInternalName
    mv.visitTypeInsn(NEW, wrapperName)
    mv.visitInsn(DUP)
    mv.visitVarInsn(ALOAD, handlerOffset + 1) // the resumption is the stack offset after handler
    mv.visitMethodInsn(INVOKESPECIAL, wrapperName, JvmName.ConstructorMethod, wrapperType.Constructor.d.toDescriptor, false)

    mv.visitFieldInsn(PUTFIELD, opFunctionType.name.toInternalName, s"arg${writtenOpArgs.size}", resumption.toErased.toDescriptor)
    // call invoke
    val invokeMethod = BackendObjType.Thunk.InvokeMethod
    mv.visitMethodInsn(INVOKEVIRTUAL, opFunctionType.name.toInternalName, invokeMethod.name, invokeMethod.d.toDescriptor, false)
    mv.visitInsn(ARETURN)

    mv.visitMaxs(999, 999)
    mv.visitEnd()
  }

  def opStaticFunctionDescriptor(sym: Symbol.OpSym)(implicit root: Root): MethodDescriptor = {
    val effect = root.effects(sym.eff)
    val op = effect.ops.find(op => op.sym == sym).getOrElse(throw InternalCompilerException(s"Could not find op '$sym' in effect '$effect'.", sym.loc))
    val writtenOpArgs = op.fparams.map(_.tpe).map(BackendType.toErasedBackendType)
    val handlerType = BackendObjType.Handler.toTpe
    val resumption = BackendObjType.Resumption.toTpe

    val methodArgs = writtenOpArgs ++ List(handlerType, resumption)
    val methodResult = BackendObjType.Result.toTpe

    MethodDescriptor(methodArgs, methodResult)
  }

  def opFieldType(sym: Symbol.OpSym)(implicit root: Root): JvmType = {
    val effect = root.effects(sym.eff)
    val op = effect.ops.find(op => op.sym == sym).getOrElse(throw InternalCompilerException(s"Could not find op '$sym' in effect '$effect'.", sym.loc))
    val writtenOpArgs = op.fparams.map(_.tpe)
    JvmOps.getFunctionInterfaceType(MonoType.Arrow(writtenOpArgs :+ MonoType.Object, MonoType.Object))
  }

}

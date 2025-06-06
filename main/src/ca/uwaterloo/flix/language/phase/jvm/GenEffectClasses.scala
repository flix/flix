package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ReducedAst.{Effect, Op, Root}
import ca.uwaterloo.flix.language.ast.{MonoType, Symbol}
import ca.uwaterloo.flix.language.phase.jvm.BytecodeInstructions.MethodEnricher
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

  def gen(effects: Iterable[Effect])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    ParOps.parAgg(effects, Map.empty[JvmName, JvmClass])({
      case (macc, effect) =>
        val className = JvmOps.getEffectDefinitionClassName(effect.sym)
        macc + (className -> JvmClass(className, genByteCode(className, effect)))
    }, _ ++ _)
  }

  private def genByteCode(effectName: JvmName, effect: Effect)(implicit root: Root, flix: Flix): Array[Byte] = {
    val visitor = AsmOps.mkClassWriter()

    val interfaces = Array(BackendObjType.Handler.jvmName.toInternalName)
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, effectName.toInternalName,
      null, BackendObjType.JavaObject.jvmName.toInternalName, interfaces)

    for (op <- effect.ops) genFieldAndMethod(visitor, effectName, op)

    genConstructor(visitor)

    visitor.visitEnd()
    visitor.toByteArray
  }

  private def genConstructor(visitor: ClassWriter): Unit = {
    val mv = visitor.visitMethod(ACC_PUBLIC, JvmName.ConstructorMethod, MethodDescriptor.NothingToVoid.toDescriptor, null, null)
    mv.visitCode()

    mv.visitByteIns({
      import BytecodeInstructions.*
      ALOAD(0) ~
        INVOKESPECIAL(BackendObjType.JavaObject.Constructor) ~
        RETURN()
    })

    mv.visitMaxs(999, 999)
    mv.visitEnd()
  }

  private def genFieldAndMethod(visitor: ClassWriter, effectName: JvmName, op: Op)(implicit root: Root): Unit = {
    // Field
    val writtenOpArgsMono = op.fparams.map(_.tpe)
    val arrowType = MonoType.Arrow(writtenOpArgsMono :+ MonoType.Object, MonoType.Object)

    val writtenOpArgs = writtenOpArgsMono.map(BackendType.toErasedBackendType)
    val opName = JvmOps.getEffectOpName(op.sym)
    val opFunction = JvmOps.getFunctionInterfaceType(arrowType)
    visitor.visitField(ACC_PUBLIC, opName, opFunction.jvmName.toDescriptor, null, null)
    // Method
    // 1. Cast the given generic handler to the current effect
    // 2. Convert the given resumption into a callable Fn1$Obj (Value -> Result) via ResumptionWrapper
    // 3. call invoke on the op
    val (writtenOpArgsOffsetRev, handlerOffset) = writtenOpArgs.foldLeft((Nil: List[(BackendType, Int)], 0)) {
      case ((acc, prev), arg) => ((arg, prev) :: acc, prev + arg.stackSlots)
    }
    val writtenOpArgsOffset = writtenOpArgsOffsetRev.reverse
    val methodArgs = writtenOpArgs ++ List(BackendObjType.Handler.toTpe, BackendObjType.Resumption.toTpe)
    val mv = visitor.visitMethod(ACC_PUBLIC + ACC_STATIC, opName, MethodDescriptor(methodArgs, BackendObjType.Result.toTpe).toDescriptor, null, null)
    mv.visitCode()

    val wrapperType = BackendObjType.ResumptionWrapper(BackendType.toBackendType(op.tpe))
    mv.visitByteIns({
      import BytecodeInstructions.*
      ALOAD(handlerOffset) ~
        CHECKCAST(effectName) ~
        GETFIELD(ClassMaker.InstanceField(effectName, opName, opFunction.toTpe)) ~
        composeN(for (((t, localOffset), i) <- writtenOpArgsOffset.zipWithIndex) yield {
          // bind all regular arguments
          DUP() ~
            xLoad(t, localOffset) ~
            PUTFIELD(ClassMaker.InstanceField(opFunction.jvmName, s"arg$i", t))
        }) ~
        // convert the resumption to a function
        DUP() ~
        NEW(wrapperType.jvmName) ~
        DUP() ~
        ALOAD(handlerOffset + 1) ~ // the resumption is the stack offset after handler
        INVOKESPECIAL(wrapperType.Constructor) ~
        PUTFIELD(ClassMaker.InstanceField(opFunction.jvmName, s"arg${writtenOpArgs.size}", BackendObjType.Resumption.toTpe.toErased)) ~
        // call invoke
        INVOKEINTERFACE(BackendObjType.Thunk.InvokeMethod) ~
        ARETURN()
    })

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

  def opFieldType(sym: Symbol.OpSym)(implicit root: Root): BackendObjType.Arrow = {
    val effect = root.effects(sym.eff)
    val op = effect.ops.find(op => op.sym == sym).getOrElse(throw InternalCompilerException(s"Could not find op '$sym' in effect '$effect'.", sym.loc))
    val writtenOpArgs = op.fparams.map(_.tpe)
    JvmOps.getFunctionInterfaceType(MonoType.Arrow(writtenOpArgs :+ MonoType.Object, MonoType.Object))
  }

}

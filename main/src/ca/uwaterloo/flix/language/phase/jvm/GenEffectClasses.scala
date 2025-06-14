package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ReducedAst.{Effect, Root}
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.{IsFinal, NotFinal}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.InstanceField
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.IsPublic
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Volatility.NotVolatile
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor
import ca.uwaterloo.flix.util.InternalCompilerException
import org.objectweb.asm.MethodVisitor

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

  def gen(effects: Iterable[Effect])(implicit root: Root, flix: Flix): List[JvmClass] = {
    for (effect <- effects.toList) yield {
      val className = JvmOps.getEffectDefinitionClassName(effect.sym)
      JvmClass(className, genByteCode(className, effect))
    }
  }

  private def genByteCode(effectName: JvmName, effect: Effect)(implicit root: Root, flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkClass(effectName, IsFinal, interfaces = List(BackendObjType.Handler.jvmName))

    cm.mkConstructor(ClassMaker.ConstructorMethod(effectName, Nil), IsPublic, constructorIns(_))

    for (op <- effect.ops) {
      val opName = JvmOps.getEffectOpName(op.sym)
      val erasedParams = op.fparams.map(_.tpe).map(BackendType.toErasedBackendType)
      val opFunction = BackendObjType.Arrow(erasedParams :+ BackendType.Object, BackendType.Object)
      val opField = ClassMaker.InstanceField(effectName, opName, opFunction.toTpe)
      cm.mkField(opField, IsPublic, NotFinal, NotVolatile)
      val methodArgs = erasedParams ++ List(BackendObjType.Handler.toTpe, BackendObjType.Resumption.toTpe)
      val returnType = BackendType.toBackendType(op.tpe)
      cm.mkStaticMethod(ClassMaker.StaticMethod(effectName, opName, MethodDescriptor(methodArgs, BackendObjType.Result.toTpe)), IsPublic, NotFinal, methodIns(effectName, opFunction, opField, erasedParams, returnType)(_))
    }

    cm.closeClassMaker()
  }

  private def constructorIns(implicit mv: MethodVisitor): Unit = {
    import BytecodeInstructions.*
    ALOAD(0)
    INVOKESPECIAL(ClassConstants.Object.Constructor)
    RETURN()
  }

  private def methodIns(effectName: JvmName, opFunction: BackendObjType.Arrow, opField: InstanceField, erasedParams: List[BackendType], returnType: BackendType)(implicit mv: MethodVisitor): Unit = {
    import BytecodeInstructions.*
    val wrapperType = BackendObjType.ResumptionWrapper(returnType)

    withNames(0, erasedParams) { case (paramsOffset, params) =>
      withName(paramsOffset, BackendObjType.Handler.toTpe) { handler =>
        withName(paramsOffset + 1, BackendObjType.Resumption.toTpe) { resumption =>
          // Cast the given generic handler to the current effect.
          handler.load()
          CHECKCAST(effectName)
          // Convert the given resumption into a callable Fn1$Obj (Value -> Result) via ResumptionWrapper.
          GETFIELD(opField)
          for ((par, i) <- params.zipWithIndex) {
            DUP()
            par.load()
            PUTFIELD(opFunction.ArgField(i))
          }
          // Convert the resumption to a function.
          DUP()
          NEW(wrapperType.jvmName)
          DUP()
          resumption.load()
          INVOKESPECIAL(wrapperType.Constructor)
          PUTFIELD(ClassMaker.InstanceField(opFunction.jvmName, s"arg${params.size}", BackendObjType.Resumption.toTpe.toErased))
          // Call invoke.
          INVOKEINTERFACE(BackendObjType.Thunk.InvokeMethod)
          ARETURN()
        }
      }
    }
  }

  def opStaticFunctionDescriptor(sym: Symbol.OpSym)(implicit root: Root): MethodDescriptor = {
    val effect = root.effects(sym.eff)
    val op = effect.ops.find(op => op.sym == sym).getOrElse(throw InternalCompilerException(s"Could not find op '$sym' in effect '$effect'.", sym.loc))
    val erasedParams = op.fparams.map(_.tpe).map(BackendType.toErasedBackendType)
    val methodArgs = erasedParams ++ List(BackendObjType.Handler.toTpe, BackendObjType.Resumption.toTpe)
    MethodDescriptor(methodArgs, BackendObjType.Result.toTpe)
  }

  def opFieldType(sym: Symbol.OpSym)(implicit root: Root): BackendObjType.Arrow = {
    val effect = root.effects(sym.eff)
    val op = effect.ops.find(op => op.sym == sym).getOrElse(throw InternalCompilerException(s"Could not find op '$sym' in effect '$effect'.", sym.loc))
    val erasedParams = op.fparams.map(_.tpe).map(BackendType.toErasedBackendType)
    BackendObjType.Arrow(erasedParams :+ BackendType.Object, BackendType.Object)
  }

}

package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.JvmAst.{Effect, Root}
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.classes.{GenHandler, GenResult, GenResumption, GenResumptionWrapper, GenThunk}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.{IsFinal, NotFinal}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.InstanceField
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.IsPublic
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Volatility.NotVolatile
import ca.uwaterloo.flix.util.InternalCompilerException
import org.objectweb.asm.MethodVisitor

import java.lang.constant.{ClassDesc, MethodTypeDesc}
import java.lang.constant.ConstantDescs.CD_Object

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

  /**
    * Returns the descriptor of the effect definition class of `sym`.
    *
    * Print       =>  Eff$Print
    * List.Crash  =>  List.Eff$Crash
    */
  def effectDesc(sym: Symbol.EffSym): ClassDesc =
    Mangle.mkDesc(sym.namespace, Mangle.mkClassName("Eff", sym.name))

  def gen(effects: Iterable[Effect])(implicit root: Root, flix: Flix): List[JvmClass] = {
    for (effect <- effects.toList) yield {
      val className = effectDesc(effect.sym)
      JvmClass(className, genByteCode(className, effect))
    }
  }

  private def genByteCode(effectName: ClassDesc, effect: Effect)(implicit root: Root, flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkClass(effectName, IsFinal, interfaces = List(GenHandler.desc))

    cm.mkConstructor(ClassMaker.ConstructorMethod(effectName, Nil), IsPublic, constructorIns(_))

    for (op <- effect.ops) {
      val name = opName(op.sym)
      val erasedParams = op.fparams.map(_.tpe).map(TypeDescs.toErasedClassDesc)
      val opFunction = BackendObjType.Arrow(erasedParams :+ CD_Object, CD_Object)
      val opField = ClassMaker.InstanceField(effectName, name, opFunction.desc)
      cm.mkField(opField, IsPublic, NotFinal, NotVolatile)
      val methodArgs = erasedParams ++ List(GenHandler.desc, GenResumption.desc)
      val returnType = TypeDescs.toErasedClassDesc(op.tpe)
      cm.mkStaticMethod(ClassMaker.StaticMethod(effectName, name, MethodTypeDescs.mkDescriptor(methodArgs *)(GenResult.desc)), IsPublic, NotFinal, methodIns(effectName, opFunction, opField, erasedParams, returnType)(_))
    }

    cm.closeClassMaker()
  }

  private def constructorIns(implicit mv: MethodVisitor): Unit = {
    ALOAD(0)
    INVOKESPECIAL(ClassConstants.Object.Constructor)
    RETURN()
  }

  private def methodIns(effectName: ClassDesc, opFunction: BackendObjType.Arrow, opField: InstanceField, erasedParams: List[ClassDesc], returnType: ClassDesc)(implicit mv: MethodVisitor): Unit = {

    withNames(0, erasedParams) { case (paramsOffset, params) =>
      withName(paramsOffset, GenHandler.desc) { handler =>
        withName(paramsOffset + 1, GenResumption.desc) { resumption =>
          // Cast the given generic handler to the current effect.
          handler.load()
          CHECKCAST(effectName)
          GETFIELD(opField)
          // The handler closure is shared across every invocation of this operation. Make a fresh
          // copy (preserving captures, but with fresh argument/local/pc slots) so that re-entrant
          // invocations from deep or multi-shot resumptions do not clobber each other's arguments.
          val absArrow = BackendObjType.AbstractArrow(opFunction.args, opFunction.result)
          CHECKCAST(absArrow.desc)
          INVOKEVIRTUAL(absArrow.GetUniqueThreadClosureMethod)
          for ((par, i) <- params.zipWithIndex) {
            DUP()
            par.load()
            PUTFIELD(opFunction.ArgField(i))
          }
          // Convert the resumption to a function.
          DUP()
          NEW(GenResumptionWrapper.desc(returnType))
          DUP()
          resumption.load()
          INVOKESPECIAL(GenResumptionWrapper.Constructor(returnType))
          PUTFIELD(ClassMaker.InstanceField(opFunction.desc, s"arg${params.size}", JavaClasses.Object))
          // Call invoke.
          INVOKEINTERFACE(GenThunk.InvokeMethod)
          ARETURN()
        }
      }
    }
  }

  def opStaticFunctionDescriptor(sym: Symbol.OpSym)(implicit root: Root): MethodTypeDesc = {
    val effect = root.effects(sym.eff)
    val op = effect.ops.find(op => op.sym == sym).getOrElse(throw InternalCompilerException(s"Could not find op '$sym' in effect '$effect'.", sym.loc))
    val erasedParams = op.fparams.map(_.tpe).map(TypeDescs.toErasedClassDesc)
    val methodArgs = erasedParams ++ List(GenHandler.desc, GenResumption.desc)
    MethodTypeDescs.mkDescriptor(methodArgs *)(GenResult.desc)
  }

  /** Returns the JVM field/method name of the effect operation `sym`. */
  def opName(sym: Symbol.OpSym): String =
    Mangle.mangle(sym.name)

  def opFieldType(sym: Symbol.OpSym)(implicit root: Root): BackendObjType.Arrow = {
    val effect = root.effects(sym.eff)
    val op = effect.ops.find(op => op.sym == sym).getOrElse(throw InternalCompilerException(s"Could not find op '$sym' in effect '$effect'.", sym.loc))
    val erasedParams = op.fparams.map(_.tpe).map(TypeDescs.toErasedClassDesc)
    BackendObjType.Arrow(erasedParams :+ CD_Object, CD_Object)
  }

}

package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ReducedAst.{Effect, FormalParam, Op, Root}
import ca.uwaterloo.flix.language.ast.{MonoType, Symbol}
import ca.uwaterloo.flix.util.ParOps
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes._

/// An effect class like this:
/// ```
/// eff SomeEffect {
///     pub def flip(): Bool
///     pub def add(x: Int32, y: Int32): Int32
/// }
/// ```
/// Is conceptually understood as (`Resumption` is a backend interface):
/// ```
/// eff SomeEffect {
///     pub def flip(unit: Unit, cont: Resumption): Value
///     pub def add(x: Int32, y: Int32, cont: Resumption): Value
/// }
/// ```
/// and is generated like so:
/// ```
/// public final class Eff$SomeEffect implements Handler {
///     public Fn2$Obj$Obj flip
///     public Fn3$Int32$Int32&Obj add
/// }
/// ```
object GenEffectClasses {

  def gen(effects: Iterable[Effect])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
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

    genFields(visitor, effect)

    visitor.visitEnd()
    visitor.toByteArray
  }

  private def genFields(visitor: ClassWriter, effect: Effect): Unit = {
    for (Op(sym, _, _, fparams, _, _, _) <- effect.ops) {
      val erasedFparams = fparams.map(_.tpe).map(JvmOps.getErasedJvmType)
      val resumptionType = JvmType.Object // actually a function
      val res = JvmType.Object // actually a Value
      val args = erasedFparams :+ resumptionType
      val functionType = JvmOps.getFunctionInterfaceType(args, res)
      visitor.visitField(ACC_PUBLIC, JvmOps.getEffectOpName(sym), functionType.toDescriptor, null, null)
    }
  }

}

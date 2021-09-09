package ca.uwaterloo.flix.language.phase.sjvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ErasedAst.Root
import ca.uwaterloo.flix.language.ast.PRefType._
import ca.uwaterloo.flix.language.ast.PType.PReference
import ca.uwaterloo.flix.language.ast.RRefType._
import ca.uwaterloo.flix.language.phase.sjvm.BytecodeCompiler._
import ca.uwaterloo.flix.language.phase.sjvm.Instructions._
import org.objectweb.asm.Opcodes

object GenFlixErrorClass {

  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    val className = JvmName.Flix.FlixError
    val superClass = JvmName.Java.RuntimeException
    Map() + (className -> JvmClass(className, genByteCode(className, superClass)))
  }

  def genByteCode(className: JvmName, superClass: JvmName)(implicit flix: Flix): Array[Byte] = {
    val classMaker = ClassMaker.mkAbstractClass(className, superClass)
    classMaker.mkConstructor(genConstructor(superClass), descriptor = RStr.thisToNothingDescriptor)
    classMaker.closeClassMaker
  }

  def genConstructor(superClass: JvmName): F[StackNil] => F[StackEnd] = {
    START[StackNil] ~
      preInitALOAD(0, tagOf[PAnyObject]) ~
      ALOAD(1, RStr.rType) ~
      ((f: F[StackNil ** PReference[PAnyObject] ** PReference[PStr]]) => {
        f.visitMethodInsn(Opcodes.INVOKESPECIAL, superClass.internalName, JvmName.constructorMethod, RStr.thisToNothingDescriptor)
        f.asInstanceOf[F[StackNil]]
      }) ~
      RETURN
  }

}
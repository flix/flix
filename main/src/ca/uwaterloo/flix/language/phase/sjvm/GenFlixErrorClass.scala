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
    Map() + (className -> JvmClass(className, genByteCode(className)))
  }

  def genByteCode(className: JvmName)(implicit flix: Flix): Array[Byte] = {
    val classMaker = ClassMaker.mkAbstractClass(className, JvmName.Java.RuntimeException)
    classMaker.mkConstructor(genConstructor(className), descriptor = RStr.rType.thisToNothingMethodDescriptor)
    classMaker.closeClassMaker
  }

  def genConstructor(name: JvmName): F[StackNil] => F[StackEnd] = {
    START[StackNil] ~
      preInitALOAD(0, tagOf[PAnyObject]) ~
      ALOAD(1, RStr.rType) ~
      ((f: F[StackNil ** PReference[PAnyObject] ** PReference[PStr]]) => {
        f.visitor.visitMethodInsn(Opcodes.INVOKESPECIAL, JvmName.Java.RuntimeException.internalName, JvmName.constructorMethod, RStr.rType.thisToNothingMethodDescriptor, false)
        f.asInstanceOf[F[StackNil]]
      }) ~
      RETURN
  }

}
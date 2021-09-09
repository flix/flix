package ca.uwaterloo.flix.language.phase.sjvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ErasedAst.Root
import ca.uwaterloo.flix.language.ast.PRefType._
import ca.uwaterloo.flix.language.ast.PType.PReference
import ca.uwaterloo.flix.language.ast.RRefType._
import ca.uwaterloo.flix.language.ast.RType._
import ca.uwaterloo.flix.language.phase.sjvm.BytecodeCompiler._
import ca.uwaterloo.flix.language.phase.sjvm.ClassMaker.Mod
import ca.uwaterloo.flix.language.phase.sjvm.Instructions._
import org.objectweb.asm.Opcodes

object GenHoleErrorClass {

  val HoleFieldName: String = "hole"
  val HoleFieldType: RReference[PStr] = RStr.rType
  val LocationFieldName: String = "location"
  val LocationFieldType: JvmName = JvmName.Flix.ReifiedSourceLocation

  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    val className = JvmName.Flix.HoleError
    val superClass = JvmName.Flix.FlixError
    Map() + (className -> JvmClass(className, genByteCode(className, superClass)))
  }

  def genByteCode(className: JvmName, superClass: JvmName)(implicit flix: Flix): Array[Byte] = {
    val classMaker = ClassMaker.mkClass(className, superClass)
    classMaker.mkConstructor(genConstructor(className, superClass), descriptor = JvmName.getMethodDescriptor(List(RStr, LocationFieldType), None))
    classMaker.mkMethod(???, JvmName.equalsMethod, JvmName.getMethodDescriptor(RObject, RBool), Mod.isPublic)
    classMaker.mkMethod(???, JvmName.hashcodeMethod, RInt32.nothingToThisDescriptor, Mod.isPublic)
    classMaker.mkField(HoleFieldName, HoleFieldType, Mod.isPublic.isFinal)
    classMaker.mkField(LocationFieldName, LocationFieldType, Mod.isPublic.isFinal)

    classMaker.closeClassMaker
  }

  private def builderAppend[R <: Stack]: F[R ** PReference[PStr]] => F[R ** PReference[PAnyObject]] = f => {
    val builder = JvmName.Java.StringBuilder
    f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, builder.internalName, "append", JvmName.getMethodDescriptor(RStr, builder))
    f.asInstanceOf[F[R ** PReference[PAnyObject]]]
  }

  def genConstructor(name: JvmName, superClass: JvmName): F[StackNil] => F[StackEnd] = {
    START[StackNil] ~
      preInitALOAD(0, tagOf[PAnyObject]) ~
      createSimpleObject(JvmName.Java.StringBuilder, tagOf[PAnyObject]) ~
      pushString("Hole '") ~
      builderAppend ~
      ALOAD(1, HoleFieldType) ~
      builderAppend ~
      pushString("' at ") ~
      builderAppend ~
      ALOAD(2, LocationFieldType, tagOf[PAnyObject]) ~
      (f => {
        f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, LocationFieldType.internalName, "toString", RStr.nothingToThisDescriptor)
        f.asInstanceOf[F[StackNil ** PReference[PAnyObject] ** PReference[PAnyObject] ** PReference[PStr]]]
      }) ~
      builderAppend ~
      (f => {
        f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, JvmName.Java.StringBuilder.internalName, "toString", RStr.nothingToThisDescriptor)
        f.asInstanceOf[F[StackNil ** PReference[PAnyObject] ** PReference[PStr]]]
      }) ~
      (f => {
        f.visitMethodInsn(Opcodes.INVOKESPECIAL, superClass.internalName, JvmName.constructorMethod, RStr.thisToNothingDescriptor)
        f.asInstanceOf[F[StackNil]]
      }) ~
      THISLOAD(name, tagOf[PAnyObject]) ~
      ALOAD(1, HoleFieldType) ~
      PUTFIELD(name, HoleFieldName, HoleFieldType, erasedType = false) ~
      THISLOAD(name, tagOf[PAnyObject]) ~
      ALOAD(2, LocationFieldType, tagOf[PAnyObject]) ~
      PUTFIELD(name, LocationFieldName, LocationFieldType, erasedType = false)
      RETURN
  }

}

package ca.uwaterloo.flix.language.phase.sjvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ErasedAst.Root
import ca.uwaterloo.flix.language.ast.PRefType._
import ca.uwaterloo.flix.language.ast.PType.PReference
import ca.uwaterloo.flix.language.ast.RRefType._
import ca.uwaterloo.flix.language.ast.RType
import ca.uwaterloo.flix.language.ast.RType._
import ca.uwaterloo.flix.language.phase.sjvm.BytecodeCompiler._
import ca.uwaterloo.flix.language.phase.sjvm.ClassMaker.Mod
import ca.uwaterloo.flix.language.phase.sjvm.Instructions._
import org.objectweb.asm.Opcodes

object GenHoleErrorClass {

  val HoleFieldName: String = "hole"
  val HoleFieldType: RReference[PStr] = RStr.rType
  val LocationFieldName: String = "location"
  val locationFieldType: JvmName = JvmName.Flix.ReifiedSourceLocation

  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    val className = JvmName.Flix.HoleError
    val superClass = JvmName.Flix.FlixError
    Map() + (className -> JvmClass(className, genByteCode(className, superClass)))
  }

  def genByteCode(className: JvmName, superClass: JvmName)(implicit flix: Flix): Array[Byte] = {
    val classMaker = ClassMaker.mkClass(className, superClass)
    classMaker.mkConstructor(genConstructor(className, superClass), descriptor = JvmName.getMethodDescriptor(List(RStr, locationFieldType), None))
    classMaker.mkMethod(???, JvmName.equalsMethod, JvmName.getMethodDescriptor(RObject, RBool), Mod.isPublic)
    classMaker.mkMethod(???, JvmName.hashcodeMethod, RInt32.nothingToThisMethodDescriptor, Mod.isPublic)
    classMaker.mkField(HoleFieldName, HoleFieldType, Mod.isPublic.isFinal)
    classMaker.mkField(LocationFieldName, locationFieldType, Mod.isPublic.isFinal)

    classMaker.closeClassMaker
  }

  private def builderAppend[R <: Stack](): F[R]

  def genConstructor(name: JvmName): F[StackNil] => F[StackEnd] = {
    val builder = JvmName.Java.StringBuilder
    val stringBuilderDescription = JvmName.getMethodDescriptor(RStr, builder)

    START[StackNil] ~
      preInitALOAD(0, tagOf[PAnyObject]) ~
      createSimpleObject(builder) ~
      pushString("Hole '") ~

  }

}

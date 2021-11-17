package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ErasedAst.Root
import ca.uwaterloo.flix.language.phase.jvm.BytecodeInstructions._
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{Finality, Instancing, Visibility}
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor.mkDescriptor

object GenRefClasses {

  val ValueFieldName: String = "value"

  def gen(types: List[BackendObjType.Ref])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    types.map { refType =>
      refType.jvmName -> JvmClass(refType.jvmName, genRefClass(refType))
    }.toMap
  }

  private def genRefClass(refType: BackendObjType.Ref)(implicit root: Root, flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkClass(refType.jvmName, Visibility.Public, Finality.Final)

    cm.mkField(ValueFieldName, refType.tpe, Visibility.Public, Finality.NonFinal, Instancing.NonStatic)
    cm.mkConstructor(genConstructor(refType), mkDescriptor(refType.tpe)(VoidableType.Void), Visibility.Public)

    cm.closeClassMaker
  }

  private def genConstructor(refType: BackendObjType.Ref)(implicit root: Root, flix: Flix): InstructionSet = {
    ALOAD(0) ~
      invokeConstructor(JvmName.Object, mkDescriptor()(VoidableType.Void)) ~
      ALOAD(0) ~
      xLoad(refType.tpe, 1) ~
      PUTFIELD(refType.jvmName, ValueFieldName, refType.tpe) ~
      RETURN()
  }
}

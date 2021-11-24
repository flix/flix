package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ErasedAst.Root
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{Finality, Instancing, Visibility}

object GenRefClasses {

  val ValueFieldName: String = "value"

  def gen(types: Iterable[BackendObjType.Ref])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    types.map { refType =>
      refType.jvmName -> JvmClass(refType.jvmName, genRefClass(refType))
    }.toMap
  }

  private def genRefClass(refType: BackendObjType.Ref)(implicit root: Root, flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkClass(refType.jvmName, Visibility.Public, Finality.Final)

    cm.mkField(ValueFieldName, refType.tpe, Visibility.Public, Finality.NonFinal, Instancing.NonStatic)
    cm.mkObjectConstructor(Visibility.Public)

    cm.closeClassMaker
  }
}

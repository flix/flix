package ca.uwaterloo.flix.language.phase.sjvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ErasedAst.Root
import ca.uwaterloo.flix.language.ast.PRefType.PUnit
import ca.uwaterloo.flix.language.ast.RRefType._
import ca.uwaterloo.flix.language.ast.RType.RReference

object GenUnitClass {

  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    val unitType = RReference(RUnit)
    val unitName = unitType.jvmName
    Map() + (unitName -> JvmClass(unitName, genByteCode(unitType)))
  }

  def genByteCode(unitType: RReference[PUnit]): Array[Byte] = {
    //    val classMaker = ClassMaker.mkClass(unitType.jvmName, addSource = true, )
    null
  }


}

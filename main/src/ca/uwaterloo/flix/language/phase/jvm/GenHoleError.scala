package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix

object GenHoleError {

  def gen()(implicit flix: Flix): Map[JvmName, JvmClass] = {
    val jvmName = JvmName.HoleError
    val bytecode = genByteCode(jvmName)
    Map(jvmName -> JvmClass(jvmName, bytecode))
  }

  def genByteCode(name: JvmName)(implicit flix: Flix): Array[Byte] = ???

}

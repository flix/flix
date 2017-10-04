package ca.uwaterloo.flix.language.phase.jvm

import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes._

object AsmOps {

  /**
    * Returns the JVM target version.
    */
  val JavaVersion: Int = V1_8

  /**
    * Returns a freshly created class writer object.
    */
  def mkClassWriter(): ClassWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES) {
    override def getCommonSuperClass(tpe1: String, tpe2: String): String = {
      JvmType.Object.name.toInternalName
    }
  }

}

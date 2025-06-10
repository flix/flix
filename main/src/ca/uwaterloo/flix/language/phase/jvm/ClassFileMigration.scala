package ca.uwaterloo.flix.language.phase.jvm

import org.objectweb.asm.{MethodVisitor, Opcodes}

/**
  * This object enriches [[org.objectweb.asm.MethodVisitor]] to have similar methods to those of
  * java.lang.classFile.CodeBuilder
  * [[https://docs.oracle.com/en/java/javase/24/docs/api/java.base/java/lang/classfile/CodeBuilder.html]]
  * from Java 24.
  *
  * This is intended to replace [[ca.uwaterloo.flix.language.phase.jvm.BytecodeInstructions.RichMethodVisitor]].
  */
object ClassFileMigration {

  /**
    * Every public function here should match those in
    * [[https://docs.oracle.com/en/java/javase/24/docs/api/java.base/java/lang/classfile/CodeBuilder.html]]
    * as close as possible without the new Java additions.
    */
  implicit class CodeBuilder(mv: MethodVisitor) {

    def aconst_null(): CodeBuilder =
      builder(_.visitInsn(Opcodes.ACONST_NULL))

    // Private functions.

    private def builder(f: MethodVisitor => Unit): CodeBuilder = {
      f(mv)
      CodeBuilder(mv)
    }

  }

}

package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.ast.MonoType
import org.objectweb.asm.Opcodes._

/**
  * Generates bytecode for the tuple interfaces.
  */
object GenRecordInterfaces {

  /**
    * Returns the set of tuple interfaces for the given set of types `ts`.
    */
  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    val jvmType = JvmOps.getRecordInterfaceType()
    val jvmName = jvmType.name
    val targs = List()
    val bytecode = genByteCode(jvmType, targs)
    Map(jvmName -> JvmClass(jvmName, bytecode))
  }

  /**
    * This method will generate code for a tuple interface.
    * There is a getter and a setter method for each element of `fields` on this interface.
    * After creating a tuple object using a tuple class which corresponds to the same tuple type as this interface,
    * the class type should never be used to reference to that object and this interface should be used for all interactions
    * with that object.
    */
  private def genByteCode(interfaceType: JvmType.Reference, targs: List[JvmType])(implicit root: Root, flix: Flix): Array[Byte] = {
    // class writer
    val visitor = AsmOps.mkClassWriter()

    // Super descriptor
    val superClass = JvmName.Object.toInternalName

    // Initialize the visitor to create a class.
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_ABSTRACT + ACC_INTERFACE, interfaceType.name.toInternalName, null, superClass, Array())

    // Source of the class
    visitor.visitSource(interfaceType.name.toInternalName, null)

    val getField = visitor.visitMethod(ACC_PUBLIC + ACC_ABSTRACT, "getField", AsmOps.getMethodDescriptor(List(JvmType.String), JvmType.Object), null, null)
    getField.visitEnd()

    val removeField = visitor.visitMethod(ACC_PUBLIC + ACC_ABSTRACT, "removeField", AsmOps.getMethodDescriptor(List(JvmType.String), interfaceType), null, null)
    removeField.visitEnd()


    visitor.visitEnd()
    visitor.toByteArray
  }

}

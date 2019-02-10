package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.ast.MonoType
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes._

/**
  * Generates bytecode for the tuple classes.
  */
object GenRecordEmpty {

  /**
    * Returns the set of tuple classes for the given set of types `ts`.
    */
  def gen(ts: Set[MonoType])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    ts.foldLeft(Map.empty[JvmName, JvmClass]) {
      case (macc, MonoType.RecordEmpty()) =>
        // Case 1: The type constructor is a tuple.
        // Construct tuple class.
        val interfaceType = JvmOps.getRecordInterfaceType()
        val jvmType = JvmOps.getRecordEmptyClassType()
        val jvmName = jvmType.name
        val targs = List()
        val bytecode = genByteCode(jvmType,interfaceType, targs)
        macc + (jvmName -> JvmClass(jvmName, bytecode))
      case (macc, _) =>
        // Case 2: The type constructor is not a RecordEmpty
        // Nothing to be done. Return the map.
        macc
    }
  }

  /**
    * This method creates the class for RecordEmpty.
    * Here, we first instantiate the visitor required to emit the code.
    *
    * Then we create the name of the class to be generated and store the result in `className`
    *
    * We then define the super of this class (Object is the supper here) and interfaces which this class implements
    * (RecordEmpty).
    * Then using super and interfaces we will create the class header.
    *
    * Then we precede with generating the code for constructor.The constructor doesn't receive any arguments.
    * For example for RecordEmpty() creates the following constructor:
    *
    * public RecordEmpty() {}
    *
    *
    * Next, we will generate the `toString()` method which will always throws an exception, since `toString` should not be called.
    * The `toString` method is always the following:
    *
    * public string toString(Object var1) throws Exception {
    * throw new Exception("toString method shouldn't be called");
    * }
    *
    * Then, we will generate the `hashCode()` method which will always throws an exception, since `hashCode` should not be called.
    * The `hashCode` method is always the following:
    *
    * public int hashCode(Object var1) throws Exception {
    * throw new Exception("hashCode method shouldn't be called");
    * }
    *
    * Finally, we generate the `equals(Obj)` method which will always throws an exception, since `equals` should not be called.
    * The `equals` method is always the following:
    *
    * public boolean equals(Object var1) throws Exception {
    * throw new Exception("equals method shouldn't be called");
    * }
    *
    */
  private def genByteCode(classType: JvmType.Reference, interfaceType: JvmType.Reference, targs: List[JvmType])(implicit root: Root, flix: Flix): Array[Byte] = {
    // class writer
    val visitor = AsmOps.mkClassWriter()

    // internal name of super
    val superClass = JvmName.Object.toInternalName

    // internal name of interfaces to be implemented
    val implementedInterfaces = Array(interfaceType.name.toInternalName)

    // Initialize the visitor to create a class.
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, classType.name.toInternalName, null, superClass, implementedInterfaces)

    // Source of the class
    visitor.visitSource(classType.name.toInternalName, null)

    // Emit the code for the constructor
    compileRecordEmptyConstructor(visitor, classType, targs)

    // Generate 'getField' method
    AsmOps.compileExceptionThrowerMethod(visitor, ACC_PUBLIC + ACC_FINAL, "getField", AsmOps.getMethodDescriptor(List(JvmType.String), JvmType.Object),
      "getField method shouldn't be called")

    // Generate `toString` method
    AsmOps.compileExceptionThrowerMethod(visitor, ACC_PUBLIC + ACC_FINAL, "toString", AsmOps.getMethodDescriptor(Nil, JvmType.String),
      "toString method shouldn't be called")

    // Generate `hashCode` method
    AsmOps.compileExceptionThrowerMethod(visitor, ACC_PUBLIC + ACC_FINAL, "hashCode", AsmOps.getMethodDescriptor(Nil, JvmType.PrimInt),
      "hashCode method shouldn't be called")

    // Generate `equals` method
    AsmOps.compileExceptionThrowerMethod(visitor, ACC_PUBLIC + ACC_FINAL, "equals", AsmOps.getMethodDescriptor(List(JvmType.Object), JvmType.Void),
      "equals method shouldn't be called")

    visitor.visitEnd()
    visitor.toByteArray
  }


  /**
    * This method generates the constructor for the RecordEmpty class. This constructor doesn't receive any arguments.
    * For example for RecordEmpty() creates the following constructor:
    *
    * public RecordEmpty() {}
    */
  def compileRecordEmptyConstructor(visitor: ClassWriter, classType: JvmType.Reference, fields: List[JvmType])(implicit root: Root, flix: Flix): Unit = {

    val constructor = visitor.visitMethod(ACC_PUBLIC, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), null, null)

    constructor.visitCode()
    constructor.visitVarInsn(ALOAD, 0)

    // Call the super (java.lang.Object) constructor
    constructor.visitMethodInsn(INVOKESPECIAL, JvmName.Object.toInternalName, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)

    // Return
    constructor.visitInsn(RETURN)

    // Parameters of visit max are thrown away because visitor will calculate the frame and variable stack size
    constructor.visitMaxs(65535, 65535)
    constructor.visitEnd()
  }

}

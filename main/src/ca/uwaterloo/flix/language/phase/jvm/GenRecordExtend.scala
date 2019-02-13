package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import ca.uwaterloo.flix.language.ast.MonoType
import org.objectweb.asm.{ClassWriter, Label}
import org.objectweb.asm.Opcodes._
/**
  * Generates bytecode for the tuple classes.
  */
object GenRecordExtend {

  /**
    * Returns the set of tuple classes for the given set of types `ts`.
    */
  def gen(ts: Set[MonoType])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    ts.foldLeft(Map.empty[JvmName, JvmClass]) {
      case (macc, MonoType.RecordExtend(label, value, rest)) =>
        // Case 1: The type constructor is a tuple.
        // Construct tuple class.
        val interfaceType = JvmOps.getRecordInterfaceType()
        val jvmType = JvmOps.getRecordExtendClassType()
        val jvmName = jvmType.name
        val targs = List[JvmType](JvmType.String, JvmType.Object, interfaceType)
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

    for ((field, ind) <- targs.zipWithIndex) {
      // Name of the field
      val fieldName = s"field$ind"

      // Defining fields of the tuple
      AsmOps.compileField(visitor, fieldName, field, isStatic = false, isPrivate = true)
    }

    // Emit the code for the constructor
    compileRecordExtendConstructor(visitor, classType, targs)

    // Emit code for the 'getField' method
    compileRecordExtendGetField(visitor, classType)

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
  def compileRecordExtendConstructor(visitor: ClassWriter, classType: JvmType.Reference, fields: List[JvmType])(implicit root: Root, flix: Flix): Unit = {

    val constructor = visitor.visitMethod(ACC_PUBLIC, "<init>", AsmOps.getMethodDescriptor(fields, JvmType.Void), null, null)

    constructor.visitCode()
    constructor.visitVarInsn(ALOAD, 0)

    // Call the super (java.lang.Object) constructor
    constructor.visitMethodInsn(INVOKESPECIAL, JvmName.Object.toInternalName, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)


    var offset: Int = 1

    for ((field, ind) <- fields.zipWithIndex) {
      val iLoad = AsmOps.getLoadInstruction(field)

      constructor.visitVarInsn(ALOAD, 0)
      constructor.visitVarInsn(iLoad, offset)
      constructor.visitFieldInsn(PUTFIELD, classType.name.toInternalName, s"field$ind", field.toDescriptor)

      field match {
        case JvmType.PrimLong | JvmType.PrimDouble => offset += 2
        case _ => offset += 1
      }
    }

    // Return
    constructor.visitInsn(RETURN)

    // Parameters of visit max are thrown away because visitor will calculate the frame and variable stack size
    constructor.visitMaxs(65535, 65535)
    constructor.visitEnd()
  }


  def compileRecordExtendGetField(visitor: ClassWriter, classType: JvmType.Reference)(implicit root: Root, flix: Flix): Unit = {

    val getField = visitor.visitMethod(ACC_PUBLIC, "getField", AsmOps.getMethodDescriptor(List(JvmType.String), JvmType.Object), null, null)

    getField.visitCode()

    //Push "this" onto stack
    getField.visitVarInsn(ALOAD, 0)

    //Push this.field0 onto the stack
    getField.visitFieldInsn(GETFIELD, classType.name.toInternalName, "field0", JvmType.String.toDescriptor)

    //Push the function argument onto the stack
    getField.visitVarInsn(ALOAD, 1)

    //Compare both strings on the stack using equals.
    getField.visitMethodInsn(INVOKEVIRTUAL, JvmName.String.toInternalName,
      "equals", AsmOps.getMethodDescriptor(List(JvmType.Object), JvmType.PrimBool), false)


    //create new labels
    val falseCase = new Label
    val ret = new Label


    //if the strings are equal ...
    getField.visitJumpInsn(IFEQ, falseCase)

    //true case
    //return this.field1

    //Load 'this' onto the stack
    getField.visitVarInsn(ALOAD, 0)

    //Push this.field1 onto the stack
    getField.visitFieldInsn(GETFIELD, classType.name.toInternalName, "field1", JvmType.Object.toDescriptor)


    getField.visitJumpInsn(GOTO,ret)


    getField.visitLabel(falseCase)

    //false case
    //recursively call this.field2.getField(var1)

    //Load 'this' onto the stack
    getField.visitVarInsn(ALOAD, 0)

    //Push this.field2 onto the stack
    getField.visitFieldInsn(GETFIELD, classType.name.toInternalName, "field2", JvmOps.getRecordInterfaceType().toDescriptor)

    //Push var1 onto the stack
    getField.visitVarInsn(ALOAD, 1)

    //call this.field2.getField(var1)
    getField.visitMethodInsn(INVOKEINTERFACE, JvmOps.getRecordInterfaceType().name.toInternalName,
      "getField", AsmOps.getMethodDescriptor(List(JvmType.String), JvmType.Object), true)

    getField.visitLabel(ret)

    getField.visitInsn(ARETURN)

    getField.visitMaxs(1, 1)
    getField.visitEnd()
  }

}

/*
 * Copyright 2019 Miguel Fialho
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ca.uwaterloo.flix.language.phase.jvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.FinalAst.Root
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes._

/**
  * Generates bytecode for the empty record class.
  */
object GenRecordEmpty {

  /**
    * Returns a Map with a single entry, for the empty record class
    */
  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    val interfaceType = JvmOps.getRecordInterfaceType()
    val jvmType = JvmOps.getRecordEmptyClassType()
    val jvmName = jvmType.name
    val targs = List()
    val bytecode = genByteCode(jvmType,interfaceType, targs)
    Map(jvmName -> JvmClass(jvmName, bytecode))
  }

  /**
    * This method creates the class for RecordEmpty.
    * Here, we first instantiate the visitor required to emit the code.
    *
    * Then we create the name of the class to be generated and store the result in `className`
    *
    * We then define the super of this class (Object is the super here) and interfaces which this class implements
    * (RecordEmpty).
    * Then using super and interfaces we will create the class header.
    *
    * Then we precede with generating the code for constructor.The constructor doesn't receive any arguments.
    * For example for RecordEmpty() creates the following constructor:
    *
    * public RecordEmpty() {}
    *
    * First, we will generate the `getRecordWithField(String)` method which will always throws an exception,
    * since `getRecordWithField` should not be called.
    * The `getRecordWithField` method is always the following:
    *
    * public IRecord getRecordWithField(String var1) throws Exception {
    * throw new Exception("getField method shouldn't be called");
    * }
    *
    * Afterwards, we will generate the `restrictField(String)` method which will always throws an exception, since `restrictField` should not be called.
    * The `restrictField` method is always the following:
    *
    * public string getField(String var1) throws Exception {
    * throw new Exception("restrictField method shouldn't be called");
    * }
    *
    * Next, we will generate the `toString()` method which will always throws an exception, since `toString` should not be called.
    * The `toString` method is always the following:
    *
    * public string toString() throws Exception {
    * throw new Exception("toString method shouldn't be called");
    * }
    *
    * Then, we will generate the `hashCode()` method which will always throws an exception, since `hashCode` should not be called.
    * The `hashCode` method is always the following:
    *
    * public int hashCode() throws Exception {
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

    // Generate 'getRecordWithField' method
    AsmOps.compileExceptionThrowerMethod(visitor, ACC_PUBLIC + ACC_FINAL, "getRecordWithField",
      AsmOps.getMethodDescriptor(List(JvmType.String), interfaceType),
      "getField method shouldn't be called")

    // Generate 'restrictField' method
    AsmOps.compileExceptionThrowerMethod(visitor, ACC_PUBLIC + ACC_FINAL, "restrictField",
      AsmOps.getMethodDescriptor(List(JvmType.String), JvmOps.getRecordInterfaceType()),
      "restrictField method shouldn't be called")

    // Generate `toString` method
    AsmOps.compileExceptionThrowerMethod(visitor, ACC_PUBLIC + ACC_FINAL, "toString",
      AsmOps.getMethodDescriptor(Nil, JvmType.String),
      "toString method shouldn't be called")

    // Generate `hashCode` method
    AsmOps.compileExceptionThrowerMethod(visitor, ACC_PUBLIC + ACC_FINAL, "hashCode",
      AsmOps.getMethodDescriptor(Nil, JvmType.PrimInt),
      "hashCode method shouldn't be called")

    // Generate `equals` method
    AsmOps.compileExceptionThrowerMethod(visitor, ACC_PUBLIC + ACC_FINAL, "equals",
      AsmOps.getMethodDescriptor(List(JvmType.Object), JvmType.Void),
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

/*
 * Copyright 2017 Magnus Madsen
 * Copyright 2021 Jonathan Lindegaard Starup
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
import ca.uwaterloo.flix.language.ast.ReducedAst.Root
import ca.uwaterloo.flix.language.ast.MonoType
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes._

/**
  * Generates bytecode for the tuple classes.
  */
object GenTupleClasses {

  /**
    * Returns the set of tuple classes for the given set of types `ts`.
    */
  def gen(ts: Set[MonoType])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    ts.foldLeft(Map.empty[JvmName, JvmClass]) {
      case (macc, tpe@MonoType.Tuple(elms)) =>
        // Case 1: The type constructor is a tuple.
        // Construct tuple class.
        val jvmType = JvmOps.getTupleClassType(tpe)
        val jvmName = jvmType.name
        val bytecode = genByteCode(jvmType, elms)
        macc + (jvmName -> JvmClass(jvmName, bytecode))
      case (macc, _) =>
        // Case 2: The type constructor is a non-tuple.
        // Nothing to be done. Return the map.
        macc
    }
  }

  /**
    * This method creates the class for each tuple.
    * Here, we first instantiate the visitor required to emit the code.
    *
    * Then we create the name of the class to be generated and store the result in `className`
    *
    * We then define the super of this class (Object is the supper here) and interfaces which is none.
    * Then using super and interfaces we will create the class header.
    *
    * We then precede to creating a field for each element of the tuple on the class. We use `compileField` helper with
    * name = `field${ind}` with `ind` being the index of the element in tuple.
    * For example, if the second element of type is of type `Bool`, we create the following
    * field on the class:
    *
    * public boolean field1;
    *
    * and if the 5th element of the tuple if of type `Set(..)` we create the following field on the class:
    *
    * public Object field4;
    *
    * Then we precede with generating the code for constructor. Number of arguments on this constructor is equal number
    * of elements in the tuple. Each of these arguments will be used to set a field on the class.
    * For example for tuple (Char, Int8) we create the following constructor:
    *
    * public Tuple(char var1, byte var2) {
    * this.field0 = var1;
    * this.field1 = var2;
    * }
    *
    * Then we generate the `getBoxedValue()` method which will return an array containing all the elements of the represented
    * tuple but all elements are boxed if their type is not a primitive.
    *
    * Next, we will generate the `toString()` method.
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
  private def genByteCode(classType: JvmType.Reference, monoTargs: List[MonoType])(implicit root: Root, flix: Flix): Array[Byte] = {
    // class writer
    val visitor = AsmOps.mkClassWriter()

    val targs = monoTargs.map(JvmOps.getErasedJvmType)

    // internal name of super
    val superClass = BackendObjType.JavaObject.jvmName.toInternalName

    // Initialize the visitor to create a class.
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, classType.name.toInternalName, null, superClass, null)

    // Source of the class
    visitor.visitSource(classType.name.toInternalName, null)

    // Adding fields and getters and setters to the class
    for ((field, ind) <- targs.zipWithIndex) {
      // Name of the field
      val fieldName = s"field$ind"

      // Defining fields of the tuple
      AsmOps.compileField(visitor, fieldName, field, isStatic = false, isPrivate = false, isVolatile = false)
    }

    // Emit the code for the constructor
    compileTupleConstructor(visitor, classType, targs)

    // Emit the code for `getBoxedValue()` method
    compileGetBoxedValueMethod(visitor, classType, targs)

    compileToStringMethod(visitor, classType, monoTargs)

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
    * This method emits the code for `getBoxedValue()` method. This method returns an array of objects containing all the
    * elements of the tuple in the same order that they appear on the tuple but if the element is a primitive then it will
    * box the value.
    */
  private def compileGetBoxedValueMethod(visitor: ClassWriter, classType: JvmType.Reference, fields: List[JvmType])(implicit root: Root, flix: Flix): Unit = {
    // header of the method
    val method = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, "getBoxedValue", s"()[Ljava/lang/Object;", null, null)

    method.visitCode()

    // Creating an array of objected
    method.visitLdcInsn(fields.length)
    method.visitTypeInsn(ANEWARRAY, BackendObjType.JavaObject.jvmName.toInternalName)

    // Putting boxed of value on the array
    fields.zipWithIndex.foreach { case (field, ind) =>
      // Duplicating the array address
      method.visitInsn(DUP)

      // Putting index on top of the stack
      method.visitLdcInsn(ind)

      // Boxing the field
      AsmOps.boxField(method, field, classType, s"field$ind")

      // Storing the value inside the array
      method.visitInsn(AASTORE)
    }

    // Returning the array
    method.visitInsn(ARETURN)

    // Parameters of visit max are thrown away because visitor will calculate the frame and variable stack size
    method.visitMaxs(1, 1)
    method.visitEnd()
  }

  /**
    * This method generates the constructor for the tuple class. Number of arguments on this constructor is equal number
    * of elements in the tuple and each argument corresponds to an element of the tuple with the appropriate type.
    * For example for tuple (Char, Int8) we create the following constructor:
    *
    * public Tuple(char var1, byte var2) {
    * this.field0 = var1;
    * this.field1 = var2;
    * }
    */
  def compileTupleConstructor(visitor: ClassWriter, classType: JvmType.Reference, fields: List[JvmType])(implicit root: Root, flix: Flix): Unit = {

    val constructor = visitor.visitMethod(ACC_PUBLIC, "<init>", AsmOps.getMethodDescriptor(fields, JvmType.Void), null, null)

    constructor.visitCode()
    constructor.visitVarInsn(ALOAD, 0)

    // Call the super (java.lang.Object) constructor
    constructor.visitMethodInsn(INVOKESPECIAL, BackendObjType.JavaObject.jvmName.toInternalName, "<init>", AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)

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

  def compileToStringMethod(visitor: ClassWriter, classType: JvmType.Reference, fields: List[MonoType])(implicit root: Root, flix: Flix): Unit = {
    val method = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, "toString", AsmOps.getMethodDescriptor(Nil, JvmType.String), null, null)
    // this is for the new bytecode framework
    val methodF = new BytecodeInstructions.F(method)
    // create an array of "(", inner, ")"
    method.visitInsn(ICONST_3)
    method.visitTypeInsn(ANEWARRAY, JvmType.String.name.toInternalName)
    method.visitInsn(DUP)
    method.visitInsn(ICONST_0)
    method.visitLdcInsn("(")
    method.visitInsn(AASTORE)
    method.visitInsn(DUP)
    method.visitInsn(ICONST_1)
    // create the inner string, which is the comma separated fields
    method.visitLdcInsn(", ")
    //     new array
    method.visitLdcInsn(fields.length)
    method.visitTypeInsn(ANEWARRAY, JvmType.String.name.toInternalName)
    //     the running index
    method.visitInsn(ICONST_M1)
    //     store string reps of fields
    for ((field, ind) <- fields.zipWithIndex) {
      val jvmType = JvmOps.getErasedJvmType(field)
      // add to index
      method.visitInsn(ICONST_1)
      method.visitInsn(IADD)
      method.visitInsn(DUP2)
      method.visitVarInsn(ALOAD, 0)
      method.visitFieldInsn(GETFIELD, classType.name.toInternalName, s"field$ind", jvmType.toDescriptor)
      BytecodeInstructions.xToString(BackendType.toErasedBackendType(field))(methodF)
      method.visitInsn(AASTORE)
    }
    method.visitInsn(POP)
    method.visitMethodInsn(INVOKESTATIC, "java/lang/String", "join", "(Ljava/lang/CharSequence;[Ljava/lang/CharSequence;)Ljava/lang/String;", false)
    method.visitInsn(AASTORE)
    method.visitInsn(DUP)
    method.visitInsn(ICONST_2)
    method.visitLdcInsn(")")
    method.visitInsn(AASTORE)
    method.visitLdcInsn("")
    method.visitInsn(SWAP)
    method.visitMethodInsn(INVOKESTATIC, "java/lang/String", "join", "(Ljava/lang/CharSequence;[Ljava/lang/CharSequence;)Ljava/lang/String;", false)
    method.visitInsn(ARETURN)
    method.visitMaxs(999, 999)
    method.visitEnd()
  }

}

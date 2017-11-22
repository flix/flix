/*
 * Copyright 2017 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast.ExecutableAst.Root
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes._

/**
  * Generates bytecode for the fusion classes.
  */
object GenFusionClasses {

  /**
    * Returns the set of fusion classes for the given set of fusion tags `tags`.
    */
  def gen(tags: Set[FusionTagInfo])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    tags.map { tag =>
      val jvmType = JvmOps.getFusionClassType(tag)
      jvmType.name -> JvmClass(jvmType.name, genByteCode(tag))
    }.toMap
  }

  /**
    * Returns the fusion class for the given fusion info.
    */
  def genByteCode(tagInfo: FusionTagInfo)(implicit root: Root, flix: Flix): Array[Byte] = {
    // Class visitor
    val visitor = AsmOps.mkClassWriter()

    // Super descriptor
    val superDescriptor = JvmName.Object.toInternalName

    // Tuple Interface
    val tupleInterface = JvmOps.getTupleInterfaceType(tagInfo.tupleType)

    // enum Interface
    val enumInterface = JvmOps.getEnumInterfaceType(tagInfo.enumType)

    // Fusion type
    val fusionType = JvmOps.getFusionClassType(tagInfo)

    // Descriptors of implemented interfaces
    val interfaceDescriptors = Array(tupleInterface.name.toInternalName, enumInterface.name.toInternalName)

    // Initialize the visitor to create a class.
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, fusionType.name.toInternalName, null,
      superDescriptor, interfaceDescriptors)

    /**
      * Fields and Methods for Tuple part of the class
      */
    // Adding fields and methods required for Tuple
    for ((elmType, ind) <- tagInfo.elms.zipWithIndex) {
      // Erased type of the field
      val jvmType = JvmOps.getErasedJvmType(elmType)

      // Name of the field
      val fieldName = s"field$ind"

      // Defining fields of the tuple
      AsmOps.compileField(visitor, fieldName, jvmType, isStatic = false, isPrivate = true)

      // Emitting getter for each field
      AsmOps.compileGetFieldMethod(visitor, fusionType.name, fieldName, s"getIndex$ind", jvmType)

      // Emitting setter for each field
      AsmOps.compileSetFieldMethod(visitor, fusionType.name, fieldName, s"setIndex$ind", jvmType)
    }

    // Emit the code for `getBoxedValue()` method for Tuple
    AsmOps.compileGetBoxedTagValueMethod(visitor, fusionType, JvmType.Object)

    // Emit code for the constructor
    GenTupleClasses.compileTupleConstructor(visitor, fusionType, tagInfo.elms.map(JvmOps.getErasedJvmType))

    /**
      * Fields and Methods for Enum part of the class
      */
    // type of the `value` field of enum
    val enumFieldType = JvmType.Object

    // Generate the `getValue` method
    compileGetEnumValueMethod(visitor)

    // Generate `getTag` method
    GenTagClasses.compileGetTagMethod(visitor, tagInfo.tag)

    // Generate the `toString` method.
    AsmOps.compileExceptionThrowerMethod(visitor, ACC_PUBLIC + ACC_FINAL, "toString", AsmOps.getMethodDescriptor(Nil, JvmType.String),
      "toString method shouldn't be called")

    // Generate the `hashCode` method.
    AsmOps.compileExceptionThrowerMethod(visitor, ACC_PUBLIC + ACC_FINAL, "hashCode", AsmOps.getMethodDescriptor(Nil, JvmType.PrimInt),
      "hashCode method shouldn't be called")

    // Generate the `equals` method.
    AsmOps.compileExceptionThrowerMethod(visitor, ACC_PUBLIC + ACC_FINAL, "equals", AsmOps.getMethodDescriptor(List(JvmType.Object), JvmType.PrimBool),
      "equals method shouldn't be called")

    visitor.visitEnd()
    visitor.toByteArray
  }

  /**
    * This will generate a method for class identified by `qualName` which return the `value` of the field of the enum
    * which is just `this`.
    *
    * @param visitor ClassWriter to emit method to the class
    */
  def compileGetEnumValueMethod(visitor: ClassWriter): Unit = {
    val method = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, "getValue", AsmOps.getMethodDescriptor(Nil, JvmType.Object), null, null)
    method.visitCode()

    // load `this`
    method.visitVarInsn(ALOAD, 0)

    // return this
    method.visitInsn(ARETURN)
    method.visitMaxs(1, 1)
    method.visitEnd()
  }

}

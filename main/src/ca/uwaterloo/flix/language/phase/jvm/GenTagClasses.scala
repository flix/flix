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
import ca.uwaterloo.flix.language.ast.ErasedAst.Root
import ca.uwaterloo.flix.language.ast.{ErasedAst, MonoType}
import org.objectweb.asm.ClassWriter
import org.objectweb.asm.Opcodes._

/**
  * Generates bytecode for the tag classes.
  */
object GenTagClasses {

  /**
    * Returns the set of tuple interfaces for the given set of types `tags`.
    */
  def gen(tags: Iterable[ErasedAst.Case])(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    tags.foldLeft(Map.empty[JvmName, JvmClass]) {
      case (macc, tag) =>
        val jvmType = JvmOps.getTagClassType(tag.sym)
        val jvmName = jvmType.name
        val bytecode = genByteCode(tag)
        macc + (jvmName -> JvmClass(jvmName, bytecode))
    }
  }

  /** Generate bytecode for each enum case (also called tag).
    *
    * First we generate bytecode for enum case interface:
    * This interface will generate a single method called `getValue` which does not have a parameter and returns the value
    * of the enum.
    *
    * Second we generate bytecode for enum case class:
    *
    * A class will be generated for each enum case.
    * This class will extend the enum case interface generated for the same enum case with the same field type.
    * This class contains one field: `value`. `value` contains the field of the case.
    * If the field is primitive, then `value` is of the type of that primitive, otherwise, `value` is just an object.
    * For example, for the case `Some$123` (corresponding to `Some[Int32]`) we generate:
    *
    * public int value;
    *
    * but for the case `Some$32` (corresponding to `Some[List[Int32]]`) we generate:
    *
    * public Object value;
    *
    * Classes generated at this step implements the interface corresponding the symbol of the enum case and they include
    * implementations of following methods: `getTag()`, `getValue()`, `getBoxedTagValue()`,`toString()`, `hashCode()` and
    * `equals(Object)`.
    * `getTag()` is the function which returns the name of the enum case. `getValue()` returns the value of `value` field.
    * `getBoxedTagValue()` returns the `value` field but the result is boxed inside an object. As an example, `getValue()` and
    * `getBoxedTagValue()` of the class representing `Some$123` (corresponding to `Some[Int32]`) is as follows:
    *
    * public final int getValue() {
    * return this.value;
    * }
    *
    * public final Object getBoxedTagValue() {
    * return new Integer(this.value);
    * }
    *
    * Next, we will generate the `toString()` method.
    *
    * Next, we will generate the `hashCode()` method which will always throws an exception, since `hashCode` should not be called.
    * The `hashCode` method is always the following:
    *
    * public String hashCode() throws Exception {
    * throw new Exception("hashCode method shouldn't be called")
    * }
    *
    * Finally, we generate the `equals(Obj)` method which will always throws an exception, since `equals` should not be called.
    * The `equals` method is always the following:
    *
    * public boolean equals(Object var1) throws Exception {
    * throw new Exception("equals method shouldn't be called");
    * }
    */
  private def genByteCode(tag: ErasedAst.Case)(implicit root: Root, flix: Flix): Array[Byte] = {
    // The JvmType of the interface for enum of `tag`.
    val superType = JvmOps.getEnumInterfaceType(tag.sym.enumSym)

    // The JvmType of the class for `tag`..
    val classType = JvmOps.getTagClassType(tag.sym)

    // The erased JvmType of the value of `tag`.
    val valueType = JvmOps.getErasedJvmType(tag.tpeDeprecated)

    // Create a new class writer.
    val visitor = AsmOps.mkClassWriter()

    // The super class of the generated class.
    val superClass = BackendObjType.JavaObject.jvmName.toInternalName

    // The interfaces implemented by the generated class.
    val implementedInterfaces = Array(superType.name.toInternalName)

    // The class header.
    visitor.visit(AsmOps.JavaVersion, ACC_PUBLIC + ACC_FINAL, classType.name.toInternalName, null, superClass, implementedInterfaces)

    // The source of the generated class.
    visitor.visitSource(classType.name.toInternalName, null)

    // Generate the value field.
    AsmOps.compileField(visitor, "value", valueType, isStatic = false, isPrivate = true, isVolatile = false)

    // Generate static `INSTANCE` field if it is a singleton
    if (JvmOps.isUnitTag(tag)) {
      AsmOps.compileField(visitor, "unitInstance", classType, isStatic = true, isPrivate = false, isVolatile = false)
    }

    // Generate the constructor of the generated class.
    compileEnumConstructor(visitor, classType, valueType, isSingleton = JvmOps.isUnitTag(tag))

    // Initialize the static field if it is a singleton.
    if (JvmOps.isUnitTag(tag)) {
      compileUnitInstance(visitor, classType)
    }

    // Generate the `getValue` method
    AsmOps.compileGetFieldMethod(visitor, classType.name, "value", "getValue", valueType)

    // Generate the `getBoxedTagValue` method.
    AsmOps.compileGetBoxedTagValueMethod(visitor, classType, valueType)

    // Generate the `getTag` method.
    compileGetTagMethod(visitor, tag.sym.name)

    compileToStringMethod(visitor, classType, tag)

    // Generate the `hashCode` method.
    AsmOps.compileExceptionThrowerMethod(visitor, ACC_PUBLIC + ACC_FINAL, "hashCode", AsmOps.getMethodDescriptor(Nil, JvmType.PrimInt),
      "hashCode method shouldn't be called")

    // Generate the `equals` method.
    AsmOps.compileExceptionThrowerMethod(visitor, ACC_PUBLIC + ACC_FINAL, "equals", AsmOps.getMethodDescriptor(List(JvmType.Object), JvmType.PrimBool),
      "equals method shouldn't be called")

    // Complete the visitor and get the bytecode.
    visitor.visitEnd()
    visitor.toByteArray
  }

  /**
    * Creates the single argument constructor of the enum case class which is named `classType`.
    * The only argument required to instantiate the class is the `value`.
    * The type of the field of the case is give by `descriptor`.
    * If the `valueType` is only `Unit`, then we can make the constructor private since the `unitInstance` field
    * can be used to obtain an instance of the case.
    *
    * @param visitor     class visitor
    * @param classType   name of the class
    * @param valueType   type of the `value` field
    * @param isSingleton if the class is a singleton this flag is set
    */
  private def compileEnumConstructor(visitor: ClassWriter, classType: JvmType.Reference, valueType: JvmType, isSingleton: Boolean)(implicit root: Root, flix: Flix): Unit = {
    // If this is a singleton then we should make the constructor private
    val specifier =
      ACC_PUBLIC

    val constructor = visitor.visitMethod(specifier, "<init>", AsmOps.getMethodDescriptor(List(valueType), JvmType.Void),
      null, null)

    constructor.visitCode()
    constructor.visitVarInsn(ALOAD, 0)

    // Call the super (java.lang.Object) constructor
    constructor.visitMethodInsn(INVOKESPECIAL, BackendObjType.JavaObject.jvmName.toInternalName, "<init>",
      AsmOps.getMethodDescriptor(Nil, JvmType.Void), false)

    // Load instruction for type of `value`
    val iLoad = AsmOps.getLoadInstruction(valueType)

    // Put the object given to the constructor on the `value` field
    constructor.visitVarInsn(ALOAD, 0)
    constructor.visitVarInsn(iLoad, 1)
    constructor.visitFieldInsn(PUTFIELD, classType.name.toInternalName, "value", valueType.toDescriptor)

    // Return
    constructor.visitInsn(RETURN)
    constructor.visitMaxs(65535, 65535)
    constructor.visitEnd()
  }

  /**
    * Generates the `getTag()` method of the class which is the implementation of `getTag` method on `tagInterface`.
    * This methods returns an string containing the tag name.
    * For example, `Val$42` (corresponding to `Val[Char]`) has following `getTag()`method:
    *
    * public final String getTag() {
    * return "Var";
    * }
    *
    * @param visitor class visitor
    * @param tag     tag String
    */
  def compileGetTagMethod(visitor: ClassWriter, tag: String)(implicit root: Root, flix: Flix): Unit = {
    val method = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, "getTag", AsmOps.getMethodDescriptor(Nil, JvmType.String), null, null)
    method.visitLdcInsn(tag)
    method.visitInsn(ARETURN)
    method.visitMaxs(1, 1)
    method.visitEnd()
  }

  def compileToStringMethod(visitor: ClassWriter, classType: JvmType.Reference, tag: ErasedAst.Case)(implicit root: Root, flix: Flix): Unit = {
    val method = visitor.visitMethod(ACC_PUBLIC + ACC_FINAL, "toString", AsmOps.getMethodDescriptor(Nil, JvmType.String), null, null)
    tag.tpeDeprecated match {
      case MonoType.Unit => // "$Tag"
        method.visitLdcInsn(tag.sym.name)
        method.visitInsn(ARETURN)

      case _ => // "$Tag($value)" or "$Tag$value" if value already prints "(...)"
        val printParanthesis = tag.tpeDeprecated match {
          case MonoType.Tuple(_) => false
          case MonoType.Unit => false
          case _ => true
        }
        method.visitLdcInsn("") // for last join call

        method.visitInsn(ICONST_3)
        method.visitTypeInsn(ANEWARRAY, JvmType.String.name.toInternalName)
        method.visitInsn(DUP)
        method.visitInsn(ICONST_0)
        method.visitLdcInsn(tag.sym.name + (if (printParanthesis) "(" else ""))
        method.visitInsn(AASTORE)
        method.visitInsn(DUP)
        method.visitInsn(ICONST_1)
        method.visitVarInsn(ALOAD, 0)
        method.visitMethodInsn(INVOKEVIRTUAL, classType.name.toInternalName, "getBoxedTagValue", AsmOps.getMethodDescriptor(Nil, JvmType.Object), false)
        method.visitMethodInsn(INVOKESTATIC, JvmType.String.name.toInternalName, "valueOf", AsmOps.getMethodDescriptor(List(JvmType.Object), JvmType.String), false)
        method.visitInsn(AASTORE)
        method.visitInsn(DUP)
        method.visitInsn(ICONST_2)
        method.visitLdcInsn(if (printParanthesis) ")" else "")
        method.visitInsn(AASTORE)
        method.visitMethodInsn(INVOKESTATIC, JvmType.String.name.toInternalName, "join", "(Ljava/lang/CharSequence;[Ljava/lang/CharSequence;)Ljava/lang/String;", false)
        method.visitInsn(ARETURN)
    }
    method.visitMaxs(1, 1)
    method.visitEnd()
  }

  /**
    * Initializing `getInstance` static field if the `value` field can be `Unit`
    *
    * @param visitor   class visitor
    * @param classType JvmType.Reference of the class
    */
  private def compileUnitInstance(visitor: ClassWriter, classType: JvmType.Reference)(implicit root: Root, flix: Flix): Unit = {
    val method = visitor.visitMethod(ACC_STATIC, "<clinit>", "()V", null, null)
    method.visitCode()

    // Instantiating the object
    method.visitTypeInsn(NEW, classType.name.toInternalName)
    method.visitInsn(DUP)

    // Getting instance of `UnitClass`
    method.visitFieldInsn(GETSTATIC, BackendObjType.Unit.jvmName.toInternalName, BackendObjType.Unit.InstanceField.name, BackendObjType.Unit.jvmName.toDescriptor)
    // Calling constructor on the object
    method.visitMethodInsn(INVOKESPECIAL, classType.name.toInternalName, "<init>",
      AsmOps.getMethodDescriptor(List(JvmType.Object), JvmType.Void), false)

    // Initializing the static field
    method.visitFieldInsn(PUTSTATIC, classType.name.toInternalName, "unitInstance", classType.toDescriptor)

    // Return
    method.visitInsn(RETURN)
    method.visitMaxs(2, 0)
    method.visitEnd()
  }

}

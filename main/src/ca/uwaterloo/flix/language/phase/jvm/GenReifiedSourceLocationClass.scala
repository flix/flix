/*
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
import ca.uwaterloo.flix.language.phase.jvm.BytecodeInstructions._
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.{IsFinal, NotFinal}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.InstanceField
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.IsPublic
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor.mkDescriptor

object GenReifiedSourceLocationClass {

  private def mkInstanceField(fieldName: String, tpe: BackendType) =
    InstanceField(JvmName.ReifiedSourceLocation, fieldName, tpe)

  private val sourceField: InstanceField =
    mkInstanceField("source", BackendObjType.String.toTpe)
  private val beginLineField: InstanceField =
    mkInstanceField("beginLine", BackendType.Int32)
  private val beginColField: InstanceField =
    mkInstanceField("beginCol", BackendType.Int32)
  private val endLineField: InstanceField =
    mkInstanceField("endLine", BackendType.Int32)
  private val endColField: InstanceField =
    mkInstanceField("endCol", BackendType.Int32)
  val ConstructorDescriptor: MethodDescriptor = mkDescriptor(BackendObjType.String.toTpe, BackendType.Int32, BackendType.Int32, BackendType.Int32, BackendType.Int32)(VoidableType.Void)

  def gen()(implicit flix: Flix): Map[JvmName, JvmClass] = {
    Map(JvmName.ReifiedSourceLocation -> JvmClass(JvmName.ReifiedSourceLocation, genByteCode()))
  }

  private def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkClass(JvmName.ReifiedSourceLocation, IsFinal)

    sourceField.mkField(cm, IsPublic, IsFinal)
    beginLineField.mkField(cm, IsPublic, IsFinal)
    beginColField.mkField(cm, IsPublic, IsFinal)
    endLineField.mkField(cm, IsPublic, IsFinal)
    endColField.mkField(cm, IsPublic, IsFinal)

    cm.mkConstructor(genConstructor(), ConstructorDescriptor, IsPublic)
    cm.mkMethod(genEqualsMethod(), "equals", mkDescriptor(JvmName.Object.toTpe)(BackendType.Bool), IsPublic, NotFinal)
    cm.mkMethod(genHashCodeMethod(), "hashCode", mkDescriptor()(BackendType.Int32), IsPublic, NotFinal)
    cm.mkMethod(genToStringMethod(), "toString", mkDescriptor()(BackendObjType.String.toTpe), IsPublic, NotFinal)

    cm.closeClassMaker
  }

  private def genConstructor()(implicit flix: Flix): InstructionSet = {
    // call super constructor
    thisLoad() ~ invokeConstructor(JvmName.Object) ~
      // store source
      thisLoad() ~
      ALOAD(1) ~
      sourceField.putField() ~
      // store begin line
      thisLoad() ~
      ILOAD(2) ~
      beginLineField.putField() ~
      // store begin col
      thisLoad() ~
      ILOAD(3) ~
      beginColField.putField() ~
      // store end line
      thisLoad() ~
      ILOAD(4) ~
      endLineField.putField() ~
      // store end col
      thisLoad() ~
      ILOAD(5) ~
      endColField.putField() ~
      // return
      RETURN()
  }

  private def genToStringMethod(): InstructionSet = {
    def appendString(): InstructionSet = INVOKEVIRTUAL(JvmName.StringBuilder, "append",
      mkDescriptor(BackendObjType.String.toTpe)(JvmName.StringBuilder.toTpe))

    def appendInt(): InstructionSet = INVOKEVIRTUAL(JvmName.StringBuilder, "append",
      mkDescriptor(BackendType.Int32)(JvmName.StringBuilder.toTpe))

    // create string builder
    NEW(JvmName.StringBuilder) ~ DUP() ~ invokeConstructor(JvmName.StringBuilder) ~
      // build string
      //TODO missing dups ???
      DUP() ~ thisLoad() ~ sourceField.getField() ~ appendString() ~
      DUP() ~ pushString(":") ~ appendString() ~
      DUP() ~ thisLoad() ~ beginLineField.getField() ~ appendInt() ~
      DUP() ~ pushString(":") ~ appendString() ~
      DUP() ~ thisLoad() ~ beginColField.getField() ~ appendInt() ~
      // create the string
      INVOKEVIRTUAL(JvmName.StringBuilder, "toString", mkDescriptor()(BackendObjType.String.toTpe)) ~
      ARETURN()
  }

  private def genHashCodeMethod(): InstructionSet = {
    def boxInt(): InstructionSet = INVOKESTATIC(JvmName.Integer, "valueOf",
      mkDescriptor(BackendType.Int32)(JvmName.Integer.toTpe))

    // create array
    ICONST_5() ~
      ANEWARRAY(JvmName.Object) ~ // TODO this was Objects??
      // insert source
      DUP() ~
      ICONST_0() ~
      thisLoad() ~ sourceField.getField() ~
      AASTORE() ~
      // insert begin line
      DUP() ~
      ICONST_1() ~
      thisLoad() ~ beginLineField.getField() ~ boxInt() ~
      AASTORE() ~
      // insert begin col
      DUP() ~
      ICONST_2() ~
      thisLoad() ~ beginColField.getField() ~ boxInt() ~
      AASTORE() ~
      // insert end line
      DUP() ~
      ICONST_3() ~
      thisLoad() ~ endLineField.getField() ~ boxInt() ~
      AASTORE() ~
      // insert end col
      DUP() ~
      ICONST_4() ~
      thisLoad() ~ endColField.getField() ~ boxInt() ~
      AASTORE() ~
      // hash the array
      INVOKESTATIC(JvmName.Objects, "hash", mkDescriptor(BackendType.Array(JvmName.Object.toTpe))(BackendType.Int32)) ~
      IRETURN()
  }

  private def genEqualsMethod(): InstructionSet = withName(1, JvmName.Object.toTpe) { otherObj =>
    // check exact equality
    thisLoad() ~
      otherObj.load() ~
      ifTrue(Condition.ACMPEQ)(pushBool(true) ~ IRETURN()) ~
      // check `other == null`
      otherObj.load() ~
      ifTrue(Condition.NULL)(pushBool(false) ~ IRETURN()) ~
      // the class equality
      thisLoad() ~
      INVOKEVIRTUAL(JvmName.Object, "getClass", mkDescriptor()(JvmName.Class.toTpe)) ~
      otherObj.load() ~
      INVOKEVIRTUAL(JvmName.Object, "getClass", mkDescriptor()(JvmName.Class.toTpe)) ~
      ifTrue(Condition.ACMPNE)(pushBool(false) ~ IRETURN()) ~
      // check individual fields
      otherObj.load() ~
      CHECKCAST(JvmName.ReifiedSourceLocation) ~
      storeWithName(2, JvmName.ReifiedSourceLocation.toTpe) { otherLoc =>
        thisLoad() ~ beginLineField.getField() ~
          otherLoc.load() ~ beginLineField.getField() ~
          ifTrue(Condition.ICMPNE)(pushBool(false) ~ IRETURN()) ~
          thisLoad() ~ beginColField.getField() ~
          otherLoc.load() ~ beginColField.getField() ~
          ifTrue(Condition.ICMPNE)(pushBool(false) ~ IRETURN()) ~
          thisLoad() ~ endLineField.getField() ~
          otherLoc.load() ~ endLineField.getField() ~
          ifTrue(Condition.ICMPNE)(pushBool(false) ~ IRETURN()) ~
          thisLoad() ~ endColField.getField() ~
          otherLoc.load() ~ endColField.getField() ~
          ifTrue(Condition.ICMPNE)(pushBool(false) ~ IRETURN()) ~
          thisLoad() ~ sourceField.getField() ~
          otherLoc.load() ~ sourceField.getField() ~
          INVOKESTATIC(JvmName.Objects, "equals", mkDescriptor(JvmName.Object.toTpe, JvmName.Object.toTpe)(BackendType.Bool)) ~
          IRETURN()
      }
  }
}

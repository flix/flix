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
import ca.uwaterloo.flix.language.phase.jvm.BackendObjType.{JavaObject, ReifiedSourceLocation}
import ca.uwaterloo.flix.language.phase.jvm.BytecodeInstructions._
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.IsFinal
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.IsPublic
import ca.uwaterloo.flix.language.phase.jvm.JvmName.MethodDescriptor.mkDescriptor

object GenReifiedSourceLocationClass {

  def gen()(implicit flix: Flix): Map[JvmName, JvmClass] = {
    val name = BackendObjType.ReifiedSourceLocation.jvmName
    Map(name -> JvmClass(name, genByteCode()))
  }

  private def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkClass(ReifiedSourceLocation.jvmName, IsFinal)

    cm.mkField(ReifiedSourceLocation.SourceField)
    cm.mkField(ReifiedSourceLocation.BeginLineField)
    cm.mkField(ReifiedSourceLocation.BeginColField)
    cm.mkField(ReifiedSourceLocation.EndLineField)
    cm.mkField(ReifiedSourceLocation.EndColField)

    cm.mkConstructor(genConstructor(), ReifiedSourceLocation.ConstructorDescriptor, IsPublic)
    cm.mkMethod(JavaObject.EqualsMethod.implementation(ReifiedSourceLocation.jvmName, Some(genEqualsMethod())))
    cm.mkMethod(JavaObject.HashcodeMethod.implementation(ReifiedSourceLocation.jvmName, Some(genHashCodeMethod())))
    cm.mkMethod(JavaObject.ToStringMethod.implementation(ReifiedSourceLocation.jvmName, Some(genToStringMethod())))

    cm.closeClassMaker()
  }

  private def genConstructor()(implicit flix: Flix): InstructionSet = {
    // call super constructor
    thisLoad() ~ invokeConstructor(JavaObject.jvmName) ~
      // store source
      thisLoad() ~
      ALOAD(1) ~
      PUTFIELD(ReifiedSourceLocation.SourceField) ~
      // store begin line
      thisLoad() ~
      ILOAD(2) ~
      PUTFIELD(ReifiedSourceLocation.BeginLineField) ~
      // store begin col
      thisLoad() ~
      ILOAD(3) ~
      PUTFIELD(ReifiedSourceLocation.BeginColField) ~
      // store end line
      thisLoad() ~
      ILOAD(4) ~
      PUTFIELD(ReifiedSourceLocation.EndLineField) ~
      // store end col
      thisLoad() ~
      ILOAD(5) ~
      PUTFIELD(ReifiedSourceLocation.EndColField) ~
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
      DUP() ~ thisLoad() ~ GETFIELD(ReifiedSourceLocation.SourceField) ~ appendString() ~
      DUP() ~ pushString(":") ~ appendString() ~
      DUP() ~ thisLoad() ~ GETFIELD(ReifiedSourceLocation.BeginLineField) ~ appendInt() ~
      DUP() ~ pushString(":") ~ appendString() ~
      DUP() ~ thisLoad() ~ GETFIELD(ReifiedSourceLocation.BeginColField) ~ appendInt() ~
      // create the string
      INVOKEVIRTUAL(JavaObject.ToStringMethod) ~
      ARETURN()
  }

  private def genHashCodeMethod(): InstructionSet = {
    def boxInt(): InstructionSet = INVOKESTATIC(JvmName.Integer, "valueOf",
      mkDescriptor(BackendType.Int32)(JvmName.Integer.toTpe))

    // create array
    ICONST_5() ~
      ANEWARRAY(JavaObject.jvmName) ~
      // insert source
      DUP() ~
      ICONST_0() ~
      thisLoad() ~ GETFIELD(ReifiedSourceLocation.SourceField) ~
      AASTORE() ~
      // insert begin line
      DUP() ~
      ICONST_1() ~
      thisLoad() ~ GETFIELD(ReifiedSourceLocation.BeginLineField) ~ boxInt() ~
      AASTORE() ~
      // insert begin col
      DUP() ~
      ICONST_2() ~
      thisLoad() ~ GETFIELD(ReifiedSourceLocation.BeginColField) ~ boxInt() ~
      AASTORE() ~
      // insert end line
      DUP() ~
      ICONST_3() ~
      thisLoad() ~ GETFIELD(ReifiedSourceLocation.EndLineField) ~ boxInt() ~
      AASTORE() ~
      // insert end col
      DUP() ~
      ICONST_4() ~
      thisLoad() ~ GETFIELD(ReifiedSourceLocation.EndColField) ~ boxInt() ~
      AASTORE() ~
      // hash the array
      INVOKESTATIC(JvmName.Objects, "hash", mkDescriptor(BackendType.Array(JavaObject.toTpe))(BackendType.Int32)) ~
      IRETURN()
  }

  private def genEqualsMethod(): InstructionSet = withName(1, JavaObject.toTpe) { otherObj =>
    // check exact equality
    thisLoad() ~
      otherObj.load() ~
      ifTrue(Condition.ACMPEQ)(pushBool(true) ~ IRETURN()) ~
      // check `other == null`
      otherObj.load() ~
      ifTrue(Condition.NULL)(pushBool(false) ~ IRETURN()) ~
      // the class equality
      thisLoad() ~
      INVOKEVIRTUAL(BackendObjType.JavaObject.GetClassMethod) ~
      otherObj.load() ~
      INVOKEVIRTUAL(BackendObjType.JavaObject.GetClassMethod) ~
      ifTrue(Condition.ACMPNE)(pushBool(false) ~ IRETURN()) ~
      // check individual fields
      otherObj.load() ~
      CHECKCAST(BackendObjType.ReifiedSourceLocation.jvmName) ~
      storeWithName(2, BackendObjType.ReifiedSourceLocation.toTpe) { otherLoc =>
        thisLoad() ~ GETFIELD(ReifiedSourceLocation.BeginLineField) ~
          otherLoc.load() ~ GETFIELD(ReifiedSourceLocation.BeginLineField) ~
          ifTrue(Condition.ICMPNE)(pushBool(false) ~ IRETURN()) ~
          thisLoad() ~ GETFIELD(ReifiedSourceLocation.BeginColField) ~
          otherLoc.load() ~ GETFIELD(ReifiedSourceLocation.BeginColField) ~
          ifTrue(Condition.ICMPNE)(pushBool(false) ~ IRETURN()) ~
          thisLoad() ~ GETFIELD(ReifiedSourceLocation.EndLineField) ~
          otherLoc.load() ~ GETFIELD(ReifiedSourceLocation.EndLineField) ~
          ifTrue(Condition.ICMPNE)(pushBool(false) ~ IRETURN()) ~
          thisLoad() ~ GETFIELD(ReifiedSourceLocation.EndColField) ~
          otherLoc.load() ~ GETFIELD(ReifiedSourceLocation.EndColField) ~
          ifTrue(Condition.ICMPNE)(pushBool(false) ~ IRETURN()) ~
          thisLoad() ~ GETFIELD(ReifiedSourceLocation.SourceField) ~
          otherLoc.load() ~ GETFIELD(ReifiedSourceLocation.SourceField) ~
          INVOKESTATIC(JvmName.Objects, "equals", mkDescriptor(JavaObject.toTpe, JavaObject.toTpe)(BackendType.Bool)) ~
          IRETURN()
      }
  }
}

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

package ca.uwaterloo.flix.language.phase.sjvm

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.ErasedAst.Root
import ca.uwaterloo.flix.language.ast.PRefType
import ca.uwaterloo.flix.language.ast.PRefType._
import ca.uwaterloo.flix.language.ast.PType._
import ca.uwaterloo.flix.language.ast.RRefType._
import ca.uwaterloo.flix.language.ast.RType._
import ca.uwaterloo.flix.language.phase.sjvm.BytecodeCompiler._
import ca.uwaterloo.flix.language.phase.sjvm.ClassMaker.Mod
import ca.uwaterloo.flix.language.phase.sjvm.Instructions._
import org.objectweb.asm.Opcodes

object GenHoleErrorClass {

  val HoleFieldName: String = "hole"
  val HoleFieldType: RReference[PStr] = RStr.rType
  val LocationFieldName: String = "location"
  val LocationFieldType: JvmName = JvmName.Flix.ReifiedSourceLocation

  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    val className = JvmName.Flix.HoleError
    val superClass = JvmName.Flix.FlixError
    Map() + (className -> JvmClass(className, genByteCode(className, superClass)))
  }

  private def genByteCode(className: JvmName, superClass: JvmName)(implicit flix: Flix): Array[Byte] = {
    val classMaker = ClassMaker.mkClass(className, superClass)
    classMaker.mkConstructor(genConstructor(className, superClass), descriptor = JvmName.getMethodDescriptor(List(RStr, LocationFieldType), None))
    classMaker.mkMethod(genEquals(className), JvmName.equalsMethod, JvmName.getMethodDescriptor(RObject, RBool), Mod.isPublic)
    classMaker.mkMethod(genHashCode(className), JvmName.hashcodeMethod, RInt32.nothingToThisDescriptor, Mod.isPublic)
    classMaker.mkField(HoleFieldName, HoleFieldType, Mod.isPublic.isFinal)
    classMaker.mkField(LocationFieldName, LocationFieldType, Mod.isPublic.isFinal)

    classMaker.closeClassMaker
  }

  private def builderAppend[R <: Stack]: F[R ** PReference[PAnyObject] ** PReference[PStr]] => F[R ** PReference[PAnyObject]] = f => {
    val builder = JvmName.Java.StringBuilder
    f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, builder.internalName, "append", JvmName.getMethodDescriptor(RStr, builder))
    f.asInstanceOf[F[R ** PReference[PAnyObject]]]
    F.pop(tagOf[PReference[PStr]])(f)
  }

  private def genConstructor(name: JvmName, superClass: JvmName): F[StackNil] => F[StackEnd] = {
    START[StackNil] ~
      preInitALOAD(0, tagOf[PAnyObject]) ~
      createSimpleObject(JvmName.Java.StringBuilder, tagOf[PAnyObject]) ~
      pushString("Hole '") ~
      builderAppend ~
      ALOAD(1, HoleFieldType) ~
      builderAppend ~
      pushString("' at ") ~
      builderAppend ~
      ALOAD(2, LocationFieldType, tagOf[PAnyObject]) ~
      (f => {
        f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, LocationFieldType.internalName, "toString", RStr.nothingToThisDescriptor)
        f.asInstanceOf[F[StackNil ** PReference[PAnyObject] ** PReference[PAnyObject] ** PReference[PStr]]]
      }) ~
      builderAppend ~
      (f => {
        f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, JvmName.Java.StringBuilder.internalName, "toString", RStr.nothingToThisDescriptor)
        f.asInstanceOf[F[StackNil ** PReference[PAnyObject] ** PReference[PStr]]]
      }) ~
      (f => {
        f.visitMethodInsn(Opcodes.INVOKESPECIAL, superClass.internalName, JvmName.constructorMethod, RStr.thisToNothingDescriptor)
        F.pop(tagOf[PReference[PAnyObject]])(F.pop(tagOf[PReference[PStr]])(f))
      }) ~
      THISLOAD(name, tagOf[PAnyObject]) ~
      ALOAD(1, HoleFieldType) ~
      PUTFIELD(name, HoleFieldName, HoleFieldType, erasedType = false) ~
      THISLOAD(name, tagOf[PAnyObject]) ~
      ALOAD(2, LocationFieldType, tagOf[PAnyObject]) ~
      PUTFIELD(name, LocationFieldName, LocationFieldType, erasedType = false) ~
      RETURN
  }

  private def getClass
  [R <: Stack, T <: PRefType]:
  F[R ** PReference[T]] => F[R ** PReference[PAnyObject]] = f => {
    f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, JvmName.Java.Object.internalName, "getClass", JvmName.Java.Class.nothingToThisDescriptor)
    f.asInstanceOf[F[R ** PReference[PAnyObject]]]
  }

  private def genEquals(name: JvmName): F[StackNil] => F[StackEnd] = {
    START[StackNil] ~
      THISLOAD(name, tagOf[PAnyObject]) ~
      ALOAD(1, RObject.rType) ~
      IF_ACMPNE() { // objects are not same reference
        ALOAD(1, RObject.rType) ~
          IFNULL { // other object is null
            pushBool(false) ~ IRETURN
          } { // other object is not null
            START[StackNil] ~
              THISLOAD(name, tagOf[PAnyObject]) ~
              getClass ~
              ALOAD(1, RObject.rType) ~
              getClass ~
              IF_ACMPEQ { // objects are both HoleErrors
                START[StackNil] ~
                  ALOAD(1, RObject.rType) ~
                  CAST(name, tagOf[PAnyObject]) ~
                  ASTORE(2) ~
                  THISLOAD(name, tagOf[PAnyObject]) ~
                  GETFIELD(name, HoleFieldName, RStr.rType, undoErasure = false) ~
                  ALOAD(2, name, tagOf[PAnyObject]) ~
                  GETFIELD(name, HoleFieldName, RStr.rType, undoErasure = false) ~
                  objectsEquals ~
                  IRETURN
              } { // objects are of different classes
                pushBool(false) ~ IRETURN
              }
          }
      } { // objects are the same reference
        pushBool(true) ~ IRETURN
      }
  }

  private def genHashCode(name: JvmName): F[StackNil] => F[StackEnd] = {
    START[StackNil] ~
      pushInt32(2) ~
      ANEWARRAY(RObject.rType) ~
      DUP ~
      pushInt32(0) ~
      THISLOAD(name, tagOf[PAnyObject]) ~
      GETFIELD(name, HoleFieldName, HoleFieldType, undoErasure = false) ~
      SUBTYPE ~
      AASTORE ~
      DUP ~
      pushInt32(1) ~
      THISLOAD(name, tagOf[PAnyObject]) ~
      GETFIELD(name, HoleFieldName, LocationFieldType, undoErasure = false, tagOf[PReference[PAnyObject]]) ~
      SUBTYPE ~
      AASTORE ~
      objectsHash ~
      IRETURN
  }

}

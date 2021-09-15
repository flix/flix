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
import ca.uwaterloo.flix.language.ast.RRefType.{RObject, RStr}
import ca.uwaterloo.flix.language.ast.RType.{RBool, RInt32}
import ca.uwaterloo.flix.language.phase.sjvm.BytecodeCompiler._
import ca.uwaterloo.flix.language.phase.sjvm.ClassMaker.Mod
import ca.uwaterloo.flix.language.phase.sjvm.Instructions._
import org.objectweb.asm.Opcodes

object GenMatchErrorClass {

  val LocationFieldName: String = "location"
  val LocationFieldType: JvmName = JvmName.Flix.ReifiedSourceLocation

  private val errorPrefix = "Non-exhaustive match at "

  def gen()(implicit root: Root, flix: Flix): Map[JvmName, JvmClass] = {
    val className = JvmName.Flix.MatchError
    val superClass = JvmName.Flix.FlixError
    Map() + (className -> JvmClass(className, genByteCode(className, superClass)))
  }

  private def genByteCode(className: JvmName, superClass: JvmName)(implicit flix: Flix): Array[Byte] = {
    val classMaker = ClassMaker.mkClass(className, superClass)

    classMaker.mkConstructor(genConstructor(className, superClass), descriptor = JvmName.getMethodDescriptor(List(LocationFieldType), None))
    classMaker.mkMethod(genEquals(className), JvmName.equalsMethod, JvmName.getMethodDescriptor(RObject, RBool), Mod.isPublic)
    classMaker.mkMethod(genHashCode(className), JvmName.hashcodeMethod, RInt32.nothingToThisDescriptor, Mod.isPublic)
    classMaker.mkField(LocationFieldName, LocationFieldType, Mod.isPublic.isFinal)

    classMaker.closeClassMaker
  }

  private def builderAppend[R <: Stack]: F[R ** PReference[PAnyObject] ** PReference[PStr]] => F[R ** PReference[PAnyObject]] = f => {
    val builder = JvmName.Java.StringBuilder
    f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, builder.internalName, "append", JvmName.getMethodDescriptor(RStr, builder))
    f.asInstanceOf[F[R ** PReference[PAnyObject]]]
    F.pop(tagOf[PReference[PStr]])(f)
  }

  private def rslToString[R <: Stack]: F[R ** PReference[PAnyObject]] => F[R ** PReference[PStr]] = f => {
    f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, JvmName.Flix.ReifiedSourceLocation.internalName, "toString", JvmName.getMethodDescriptor(Nil, RStr))
    val f1 = F.pop(tagOf[PReference[PAnyObject]])(f)
    F.push(tagOf[PReference[PStr]])(f1)
  }

  private def builderToString[R <: Stack]: F[R ** PReference[PAnyObject]] => F[R ** PReference[PStr]] = f => {
    val builder = JvmName.Java.StringBuilder
    f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, builder.internalName, "toString", JvmName.getMethodDescriptor(Nil, RStr))
    f.asInstanceOf[F[R ** PReference[PStr]]]
  }

  private def genConstructor(matchError: JvmName, superClass: JvmName): F[StackNil] => F[StackEnd] = {
    START[StackNil] ~
      preInitALOAD(0, tagOf[PAnyObject]) ~
      NEW(JvmName.Java.StringBuilder, tagOf[PAnyObject]) ~
      DUP ~
      invokeSimpleConstructor(JvmName.Java.StringBuilder) ~
      pushString(errorPrefix) ~
      builderAppend ~
      ALOAD(1, LocationFieldType, tagOf[PAnyObject]) ~
      rslToString ~
      builderAppend ~
      builderToString ~
      ((f: F[StackNil ** PReference[PAnyObject] ** PReference[PStr]]) => {
        f.visitMethodInsn(Opcodes.INVOKESPECIAL, superClass.internalName, JvmName.constructorMethod, RStr.thisToNothingDescriptor)
        val f1 = F.pop(tagOf[PReference[PStr]])(f)
        F.pop(tagOf[PReference[PAnyObject]])(f1)
      }) ~
      preInitALOAD(0, tagOf[PAnyObject]) ~
      ALOAD(1, LocationFieldType, tagOf[PAnyObject]) ~
      PUTFIELD(matchError, LocationFieldName, LocationFieldType, erasedType = false) ~
      RETURN
  }

  private def getClass
  [R <: Stack, T <: PRefType]:
  F[R ** PReference[T]] => F[R ** PReference[PAnyObject]] = f => {
    f.visitMethodInsn(Opcodes.INVOKEVIRTUAL, JvmName.Java.Object.internalName, "getClass", JvmName.Java.Class.nothingToThisDescriptor)
    val f1 = F.pop(tagOf[PReference[T]])(f)
    F.push(tagOf[PReference[PAnyObject]])(f1)
  }

  private def genEquals(matchError: JvmName): F[StackNil] => F[StackEnd] = {
    START[StackNil] ~
      THISLOAD(matchError, tagOf[PAnyObject]) ~
      ALOAD(1, RObject.rType) ~
      IF_ACMPNE() { // objects are not same reference
        ALOAD(1, RObject.rType) ~
          IFNULL { // other object is null
            pushBool(false) ~ IRETURN
          } { // other object is not null
            START[StackNil] ~
              THISLOAD(matchError, tagOf[PAnyObject]) ~
              getClass ~
              ALOAD(1, RObject.rType) ~
              getClass ~
              IF_ACMPEQ { // objects are both HoleErrors
                START[StackNil] ~
                  ALOAD(1, RObject.rType) ~
                  CAST(matchError, tagOf[PAnyObject]) ~
                  ASTORE(2) ~
                  THISLOAD(matchError, tagOf[PAnyObject]) ~
                  GETFIELD(matchError, LocationFieldName, LocationFieldType, undoErasure = false, tagOf[PReference[PAnyObject]]) ~
                  ALOAD(2, matchError, tagOf[PAnyObject]) ~
                  GETFIELD(matchError, LocationFieldName, LocationFieldType, undoErasure = false, tagOf[PReference[PAnyObject]]) ~
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
    //    START[StackNil] ~
    //      pushInt32(2) ~
    //      ANEWARRAY(RObject.rType) ~
    //      DUP ~
    //      pushInt32(0) ~
    //      THISLOAD(name, tagOf[PAnyObject]) ~
    //      GETFIELD(name, HoleFieldName, HoleFieldType, undoErasure = false) ~
    //      SUBTYPE ~
    //      AASTORE ~
    //      DUP ~
    //      pushInt32(1) ~
    //      THISLOAD(name, tagOf[PAnyObject]) ~
    //      GETFIELD(name, HoleFieldName, LocationFieldType, undoErasure = false, tagOf[PReference[PAnyObject]]) ~
    //      SUBTYPE ~
    //      AASTORE ~
    //      objectsHash ~
    //      IRETURNf
    ???
  }

}

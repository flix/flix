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

package ca.uwaterloo.flix.language.phase.jvm.classes

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.{IsFinal, NotFinal}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.{IsPrivate, IsPublic}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Volatility.NotVolatile
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{ConstructorMethod, StaticConstructorMethod, StaticField, StaticMethod}
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.Mangle.{DevFlixRuntime, mkDesc}
import ca.uwaterloo.flix.language.phase.jvm.MethodTypeDescs.{mkDescriptor, mkVoidDescriptor}
import ca.uwaterloo.flix.language.phase.jvm.{ClassConstants, ClassMaker, JavaClasses, MethodTypeDescs}
import org.objectweb.asm.{MethodVisitor, Opcodes}

import java.lang.constant.ConstantDescs.{CD_int, CD_long, CD_void}
import java.lang.constant.{ClassDesc, MethodTypeDesc}

/**
  * The `dev.flix.runtime.Global` class, which holds the global id counter and the command line arguments.
  */
object GenGlobal {

  /** "Global" is fixed in source code, so it should not be mangled and `$` suffixed. */
  val desc: ClassDesc = mkDesc(DevFlixRuntime, "Global")

  def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = ClassMaker.mkClass(this.desc, IsFinal)

    cm.mkConstructor(Constructor, IsPublic, nullarySuperConstructor(ClassConstants.Object.Constructor)(_))
    cm.mkStaticConstructor(StaticConstructorMethod(this.desc), staticConstructorIns(_))

    cm.mkField(CounterField, IsPrivate, IsFinal, NotVolatile)
    cm.mkStaticMethod(NewIdMethod, IsPublic, IsFinal, newIdIns(_))

    cm.mkField(ArgsField, IsPrivate, NotFinal, NotVolatile)
    cm.mkStaticMethod(GetArgsMethod, IsPublic, IsFinal, getArgsIns(_))
    cm.mkStaticMethod(SetArgsMethod, IsPublic, IsFinal, setArgsIns(_))

    cm.closeClassMaker()
  }

  def Constructor: ConstructorMethod = ConstructorMethod(this.desc, Nil)

  private def staticConstructorIns(implicit mv: MethodVisitor): Unit = {
    NEW(JavaClasses.AtomicLong)
    DUP()
    invokeConstructor(JavaClasses.AtomicLong, MethodTypeDescs.NothingToVoid)
    PUTSTATIC(CounterField)
    ICONST_0()
    ANEWARRAY(JavaClasses.String)
    PUTSTATIC(ArgsField)
    RETURN()
  }

  private def NewIdMethod: StaticMethod = StaticMethod(this.desc, "newId", mkDescriptor()(CD_long))

  private def newIdIns(implicit mv: MethodVisitor): Unit = {
    GETSTATIC(CounterField)
    INVOKEVIRTUAL(JavaClasses.AtomicLong, "getAndIncrement",
      mkDescriptor()(CD_long))
    LRETURN()
  }

  private def GetArgsMethod: StaticMethod = StaticMethod(this.desc, "getArgs", mkDescriptor()(JavaClasses.String.arrayType()))

  private def getArgsIns(implicit mv: MethodVisitor): Unit = {
    GETSTATIC(ArgsField)
    ARRAYLENGTH()
    ANEWARRAY(JavaClasses.String)
    ASTORE(0)
    // the new array is now created, now to copy the args
    GETSTATIC(ArgsField)
    ICONST_0()
    ALOAD(0)
    ICONST_0()
    GETSTATIC(ArgsField)
    ARRAYLENGTH()
    arrayCopy()
    ALOAD(0)
    ARETURN()
  }

  def SetArgsMethod: StaticMethod =
    StaticMethod(this.desc, "setArgs", MethodTypeDesc.of(CD_void, JavaClasses.String.arrayType()))

  private def setArgsIns(implicit mv: MethodVisitor): Unit = {
    ALOAD(0)
    ARRAYLENGTH()
    ANEWARRAY(JavaClasses.String)
    ASTORE(1)
    ALOAD(0)
    ICONST_0()
    ALOAD(1)
    ICONST_0()
    ALOAD(0)
    ARRAYLENGTH()
    arrayCopy()
    ALOAD(1)
    PUTSTATIC(ArgsField)
    RETURN()
  }

  private def CounterField: StaticField = StaticField(this.desc, "counter", JavaClasses.AtomicLong)

  private def ArgsField: StaticField = StaticField(this.desc, "args", JavaClasses.String.arrayType())

  private def arrayCopy()(implicit mv: MethodVisitor): Unit = {
    mv.visitMethodInstruction(Opcodes.INVOKESTATIC, JavaClasses.System, "arraycopy",
      mkVoidDescriptor(JavaClasses.Object, CD_int, JavaClasses.Object, CD_int, CD_int), isInterface = false)
  }

}

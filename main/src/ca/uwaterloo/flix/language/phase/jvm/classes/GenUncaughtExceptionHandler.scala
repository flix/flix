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
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.IsFinal
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.{IsPrivate, IsPublic}
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Volatility.NotVolatile
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{ConstructorMethod, InstanceField, InstanceMethod, mkClass}
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.Mangle.{DevFlixRuntime, mkDesc}
import ca.uwaterloo.flix.language.phase.jvm.{BackendObjType, ClassConstants, JavaClasses, Mangle}
import org.objectweb.asm.MethodVisitor

import java.lang.constant.ClassDesc

/**
  * The `UncaughtExceptionHandler` class, which reports exceptions of a region's child threads
  * back to that region.
  */
object GenUncaughtExceptionHandler {

  val desc: ClassDesc = mkDesc(DevFlixRuntime, Mangle.mkClassName("UncaughtExceptionHandler"))

  def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = mkClass(this.desc, IsFinal, interfaces = List(JavaClasses.Thread$UncaughtExceptionHandler))

    cm.mkField(RegionField, IsPrivate, IsFinal, NotVolatile)
    cm.mkConstructor(Constructor, IsPublic, constructorIns(_))
    cm.mkMethod(Nil, UncaughtExceptionMethod, IsPublic, IsFinal, uncaughtExceptionsIns(_))

    cm.closeClassMaker()
  }

  // private final Region r;
  private def RegionField: InstanceField = InstanceField(this.desc, "r", GenRegion.desc)

  // UncaughtExceptionHandler(Region r) { this.r = r; }
  def Constructor: ConstructorMethod = ConstructorMethod(this.desc, GenRegion.desc :: Nil)

  private def constructorIns(implicit mv: MethodVisitor): Unit = {
    thisLoad()
    INVOKESPECIAL(ClassConstants.Object.Constructor)
    thisLoad()
    ALOAD(1)
    PUTFIELD(RegionField)
    RETURN()
  }

  // public void uncaughtException(Thread t, Throwable e) { r.reportChildException(e); }
  private def UncaughtExceptionMethod: InstanceMethod =
    InstanceMethod(this.desc, "uncaughtException", ClassConstants.ThreadUncaughtExceptionHandler.UncaughtExceptionMethod.d)

  private def uncaughtExceptionsIns(implicit mv: MethodVisitor): Unit = {
    thisLoad()
    GETFIELD(RegionField)
    ALOAD(2)
    INVOKEVIRTUAL(GenRegion.ReportChildExceptionMethod)
    RETURN()
  }

}

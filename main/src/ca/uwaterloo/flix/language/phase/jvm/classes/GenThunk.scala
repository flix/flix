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
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.NotFinal
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.IsPublic
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{DefaultMethod, InterfaceMethod, mkInterface}
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.Mangle.{DevFlixRuntime, mkDesc}
import ca.uwaterloo.flix.language.phase.jvm.MethodTypeDescs.{mkDescriptor, mkVoidDescriptor}
import ca.uwaterloo.flix.language.phase.jvm.{JavaClasses, Mangle}
import ca.uwaterloo.flix.util.ClassDescs
import org.objectweb.asm.MethodVisitor

import java.lang.constant.ClassDesc

/**
  * The `Thunk` interface, a [[GenResult]] holding a computation that has not run yet.
  */
object GenThunk {

  val desc: ClassDesc = mkDesc(DevFlixRuntime, Mangle.mkClassName("Thunk"))

  def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = mkInterface(this.desc, interfaces = List(GenResult.desc, JavaClasses.Runnable))

    cm.mkInterfaceMethod(InvokeMethod)
    cm.mkDefaultMethod(RunMethod, IsPublic, NotFinal, runIns(_))

    cm.closeClassMaker()
  }

  def InvokeMethod: InterfaceMethod = InterfaceMethod(this.desc, "invoke", mkDescriptor()(GenResult.desc))

  private def RunMethod: DefaultMethod = DefaultMethod(this.desc, "run", mkVoidDescriptor())

  private def runIns(implicit mv: MethodVisitor): Unit = {
    thisLoad()
    GenResult.unwindSuspensionFreeThunk(s"in ${ClassDescs.binaryNameOf(JavaClasses.Runnable)}", SourceLocation.Unknown)
    POP()
    RETURN()
  }

}

/*
 * Copyright 2021 Jonathan Lindegaard Starup
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
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
import ca.uwaterloo.flix.language.phase.jvm.Mangle
import ca.uwaterloo.flix.language.jvm.{ClassDescs, JavaClasses}
import org.objectweb.asm.MethodVisitor

import java.lang.constant.ClassDesc

/**
  * The `Thunk` interface, a [[GenResult]] holding a computation that has not run yet.
  */
object GenThunk {

  /** The JVM class descriptor for the generated `Thunk` class. */
  val Desc: ClassDesc = mkDesc(DevFlixRuntime, Mangle.mkClassName("Thunk"))

  def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = mkInterface(this.Desc, interfaces = List(GenResult.Desc, JavaClasses.Runnable))

    cm.mkInterfaceMethod(InvokeMethod)
    cm.mkDefaultMethod(RunMethod, IsPublic, NotFinal, runIns(_))

    cm.closeClassMaker()
  }

  def InvokeMethod: InterfaceMethod = InterfaceMethod(this.Desc, "invoke", mkDescriptor()(GenResult.Desc))

  private def RunMethod: DefaultMethod = DefaultMethod(this.Desc, "run", mkVoidDescriptor())

  private def runIns(implicit mv: MethodVisitor): Unit = {
    thisLoad()
    GenResult.unwindSuspensionFreeThunk(s"in ${ClassDescs.binaryNameOf(JavaClasses.Runnable)}", SourceLocation.Unknown)
    POP()
    RETURN()
  }

}

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
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.mkInterface
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.Mangle.{DevFlixRuntime, mkDesc}
import ca.uwaterloo.flix.language.phase.jvm.Mangle
import org.objectweb.asm.MethodVisitor

import java.lang.constant.ClassDesc

/**
  * The `Result` interface, the return type of every compiled Flix function.
  *
  * A `Result` is one of [[GenValue]] (a finished computation), [[GenThunk]] (a computation
  * still to run), or [[GenSuspension]] (a computation stopped by an effect operation).
  */
object GenResult {

  val desc: ClassDesc = mkDesc(DevFlixRuntime, Mangle.mkClassName("Result"))

  def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = mkInterface(this.desc)
    cm.closeClassMaker()
  }

  /**
    * Expects a Result on the stack and leaves a non-Thunk Result.
    * [..., Result] --> [..., Suspension|Value]
    */
  def unwindThunk()(implicit mv: MethodVisitor): Unit = {
    whileLoop(Condition.NE) {
      DUP()
      INSTANCEOF(GenThunk.desc)
    } {
      CHECKCAST(GenThunk.desc)
      INVOKEINTERFACE(GenThunk.InvokeMethod)
    }
  }

  /**
    * Expects a Result on the stack.
    * If the result is a Suspension, this will return a modified Suspension.
    * If the result in NOT a Suspension, this will leave it on the stack.
    * [..., Result] --> [..., Thunk|Value]
    * side effect: Will return a modified suspension if a suspension occurs
    */
  private def handleSuspension(pc: Int, newFrame: MethodVisitor => Unit, setPc: MethodVisitor => Unit)(implicit mv: MethodVisitor): Unit = {
    DUP()
    INSTANCEOF(GenSuspension.desc)
    ifCondition(Condition.NE) {
      DUP()
      CHECKCAST(GenSuspension.desc) // [..., s]
      // Add our new frame
      NEW(GenSuspension.desc)
      DUP()
      INVOKESPECIAL(GenSuspension.Constructor) // [..., s, s']
      SWAP() // [..., s', s]
      DUP2() // [..., s', s, s', s]
      GETFIELD(GenSuspension.EffSymField)
      PUTFIELD(GenSuspension.EffSymField) // [..., s', s]
      DUP2()
      GETFIELD(GenSuspension.EffOpField)
      PUTFIELD(GenSuspension.EffOpField) // [..., s', s]
      DUP2()
      GETFIELD(GenSuspension.ResumptionField)
      PUTFIELD(GenSuspension.ResumptionField) // [..., s', s]
      DUP2()
      GETFIELD(GenSuspension.PrefixField) // [..., s', s, s', s.prefix]
      // Make the new frame and push it
      newFrame(mv)
      DUP()
      pushInt(pc)
      setPc(mv)
      INVOKEINTERFACE(GenFrames.PushMethod) // [..., s', s, s', prefix']
      PUTFIELD(GenSuspension.PrefixField) // [..., s', s]
      POP() // [..., s']
      // Return the suspension up the stack
      xReturn(GenSuspension.desc)
    }
  }

  /**
    * Expects a Result on the stack and leaves a Value.
    * This might return if a Suspension is encountered.
    * [..., Result] --> [..., Value.value: tpe]
    * side effect: Will return any Suspension found
    */
  def unwindThunkToValue(pc: Int, newFrame: MethodVisitor => Unit, setPc: MethodVisitor => Unit)(implicit mv: MethodVisitor): Unit = {
    unwindThunk()
    handleSuspension(pc, newFrame, setPc)
    CHECKCAST(GenValue.desc) // Cannot fail
  }

  /**
    * Expects a Result on the stack and leaves something of the given tpe but erased.
    * Assumes that the result is control-pure, i.e. it is not a suspension and will never return a suspension through a thunk.
    * [..., Result] --> [..., Value.value: tpe]
    * side effect: crashes on suspensions
    */
  def unwindSuspensionFreeThunkToType(tpe: ClassDesc, errorHint: String, loc: SourceLocation)(implicit mv: MethodVisitor): Unit = {
    unwindThunk()
    crashIfSuspension(errorHint, loc)
    CHECKCAST(GenValue.desc) // Cannot fail
    GETFIELD(GenValue.fieldFromType(tpe))
    castIfNotPrim(tpe)
  }

  /**
    * Expects a Result on the stack and leaves a Value.
    * Assumes that the result is control-pure, i.e. it is not a suspension and will never return a suspension through a thunk.
    * [..., Result] --> [..., Value]
    * side effect: crashes on suspensions
    */
  def unwindSuspensionFreeThunk(errorHint: String, loc: SourceLocation)(implicit mv: MethodVisitor): Unit = {
    unwindThunk()
    crashIfSuspension(errorHint, loc)
    CHECKCAST(GenValue.desc)
  }

  /**
    * [..., Result] -> [..., Value|Thunk]
    * side effect: if the result is a suspension, a [[GenUnhandledEffectError]] is thrown.
    */
  def crashIfSuspension(errorHint: String, loc: SourceLocation)(implicit mv: MethodVisitor): Unit = {
    DUP()
    INSTANCEOF(GenSuspension.desc)
    ifCondition(Condition.NE) {
      CHECKCAST(GenSuspension.desc)
      NEW(GenUnhandledEffectError.desc)
      // [.., suspension, UEE] -> [.., suspension, UEE, UEE, suspension]
      DUP2()
      SWAP()
      pushString(errorHint)
      pushLoc(loc)
      // [.., suspension, UEE, UEE, suspension, info, rsl] -> [.., suspension, UEE]
      INVOKESPECIAL(GenUnhandledEffectError.Constructor)
      ATHROW()
    }
  }

}

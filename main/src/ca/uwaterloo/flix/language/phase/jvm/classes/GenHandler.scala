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
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Final.NotFinal
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.Visibility.IsPublic
import ca.uwaterloo.flix.language.phase.jvm.ClassMaker.{StaticInterfaceMethod, mkInterface}
import ca.uwaterloo.flix.language.phase.jvm.Instructions.*
import ca.uwaterloo.flix.language.phase.jvm.Mangle.{DevFlixRuntime, mkDesc}
import ca.uwaterloo.flix.language.phase.jvm.MethodTypeDescs.mkDescriptor
import ca.uwaterloo.flix.language.phase.jvm.{ClassConstants, JavaClasses, Mangle}
import org.objectweb.asm.MethodVisitor

import java.lang.constant.ClassDesc

/**
  * The `Handler` interface, implemented by every generated effect class.
  *
  * [[InstallHandlerMethod]] runs a thunk under a handler: if the thunk suspends on this
  * handler's effect it applies the operation, and otherwise it passes the suspension
  * further up with this handler recorded in the resumption.
  */
object GenHandler {

  val desc: ClassDesc = mkDesc(DevFlixRuntime, Mangle.mkClassName("Handler"))

  def genByteCode()(implicit flix: Flix): Array[Byte] = {
    val cm = mkInterface(this.desc)
    cm.mkStaticInterfaceMethod(InstallHandlerMethod, IsPublic, NotFinal, installHandlerIns(_))
    cm.closeClassMaker()
  }

  def InstallHandlerMethod: StaticInterfaceMethod = StaticInterfaceMethod(
    this.desc,
    "installHandler",
    mkDescriptor(JavaClasses.String, GenHandler.desc, GenFrames.desc, GenThunk.desc)(GenResult.desc)
  )

  private def installHandlerIns(implicit mv: MethodVisitor): Unit = {
    withName(0, JavaClasses.String) { effSym =>
      withName(1, GenHandler.desc) { handler =>
        withName(2, GenFrames.desc) { frames =>
          withName(3, GenThunk.desc) { thunk =>
            thunk.load()
            // Thunk|Value|Suspension
            GenResult.unwindThunk()
            // Value|Suspension
            // handle suspension
            DUP()
            INSTANCEOF(GenSuspension.desc)
            ifCondition(Condition.NE) {
              DUP()
              CHECKCAST(GenSuspension.desc)
              storeWithName(4, GenSuspension.desc) { s =>
                NEW(GenResumptionCons.desc)
                DUP()
                INVOKESPECIAL(GenResumptionCons.Constructor)
                DUP()
                effSym.load()
                PUTFIELD(GenResumptionCons.SymField)
                DUP()
                handler.load()
                PUTFIELD(GenResumptionCons.HandlerField)
                DUP()
                s.load()
                GETFIELD(GenSuspension.PrefixField)
                frames.load()
                INVOKEINTERFACE(GenFrames.ReverseOntoMethod)
                PUTFIELD(GenResumptionCons.FramesField)
                DUP()
                s.load()
                GETFIELD(GenSuspension.ResumptionField)
                PUTFIELD(GenResumptionCons.TailField)
                storeWithName(5, GenResumptionCons.desc) { r =>
                  s.load()
                  GETFIELD(GenSuspension.EffSymField)
                  effSym.load()
                  INVOKEVIRTUAL(ClassConstants.Object.EqualsMethod)
                  ifCondition(Condition.NE) {
                    s.load()
                    GETFIELD(GenSuspension.EffOpField)
                    handler.load()
                    r.load()
                    INVOKEINTERFACE(GenEffectCall.ApplyMethod)
                    xReturn(GenResult.desc)
                  }
                  NEW(GenSuspension.desc)
                  DUP()
                  INVOKESPECIAL(GenSuspension.Constructor)
                  DUP()
                  s.load()
                  GETFIELD(GenSuspension.EffSymField)
                  PUTFIELD(GenSuspension.EffSymField)
                  DUP()
                  s.load()
                  GETFIELD(GenSuspension.EffOpField)
                  PUTFIELD(GenSuspension.EffOpField)
                  DUP()
                  NEW(GenFramesNil.desc)
                  DUP()
                  INVOKESPECIAL(GenFramesNil.Constructor)
                  PUTFIELD(GenSuspension.PrefixField)
                  DUP()
                  r.load()
                  PUTFIELD(GenSuspension.ResumptionField)
                  xReturn(GenSuspension.desc)
                }
              }
            }

            // Value
            CHECKCAST(GenValue.desc)
            storeWithName(6, GenValue.desc) { res =>
              //
              // Case on frames
              // FramesNil
              frames.load()
              INSTANCEOF(GenFramesNil.desc)
              ifCondition(Condition.NE) {
                res.load()
                xReturn(GenValue.desc)
              }
              // FramesCons
              frames.load()
              CHECKCAST(GenFramesCons.desc)
              storeWithName(7, GenFramesCons.desc) { cons => {
                effSym.load()
                handler.load()
                cons.load()
                GETFIELD(GenFramesCons.TailField)
                // thunk
                cons.load()
                GETFIELD(GenFramesCons.HeadField)
                res.load()
                mkStaticLambda(GenThunk.InvokeMethod, GenFrame.StaticApplyMethod, drop = 0)
                INVOKESTATIC(InstallHandlerMethod)
                xReturn(GenResult.desc)
              }
              }
            }
          }
        }
      }
    }
  }

}
